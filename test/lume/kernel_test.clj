(ns lume.kernel-test
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [are deftest is testing use-fixtures]]
            [lume.agent :as agent]
            [lume.agents :as agents]
            [lume.ask :as ask]
            [lume.clean :as clean]
            [lume.drive :as drive]
            [lume.features :as features]
            [lume.inspect :as inspect]
            [lume.install :as install]
            [lume.item :as item]
            [lume.kernel :as kernel]
            [lume.measure :as measure]
            [lume.note :as note]
            [lume.ntfy :as ntfy]
            [lume.policy :as policy]
            [lume.session :as session]
            [lume.spec :as spec]
            [lume.status :as status]
            [lume.store :as store])
  (:import (java.time Instant)))

(def ^:dynamic *root* nil)

;; The fake driver's scripts and calls, per test so tests can run in parallel.
(def ^:private ^:dynamic *driver-turns* nil)
(def ^:private ^:dynamic *driver-calls* nil)

(use-fixtures :each
  (fn [t]
    (let [home (str (fs/create-temp-dir {:prefix "lume-home"}))
          root (str (fs/create-temp-dir {:prefix "lume-repo"}))]
      (try
        (binding [store/*home* home store/*session* nil kernel/*actor* "human" kernel/*spawned-as* nil *root* root
                  kernel/*features* features/all
                  kernel/*confirm* (constantly true) kernel/*read-line* (constantly nil)
                  drive/*source* nil ntfy/*ntfy* nil drive/*reexec* (fn [& _] (throw (ex-info "no exec in tests" {})))
                  *driver-turns* (atom []) *driver-calls* (atom [])
                  agents/*claude-home* (str (fs/path home "claude"))
                  agents/*devin-home* (str (fs/path home "devin"))]
          (t))
        (finally (fs/delete-tree home) (fs/delete-tree root))))))

(defn- new-session [goal]
  (session/init *root* [goal])
  (store/current-session *root*))

(defmethod agents/command :test [_ _prompt _opts]
  ["sh" "-c" "printf 'working\\ndone by %s\\n' \"$LUME_ACTOR\"; exit 3"])

(deftest items-and-notes
  (let [s (new-session "build the kernel")]
    (is (= "i1" (item/item-add s ["first"])))
    (is (= "i2" (item/item-add s ["second"])))
    (item/item-close s ["i1" "done" "works"])
    (note/note s ["keep it small"])
    (let [{:keys [goal items notes]} (status/project (store/read-events s))]
      (is (= "build the kernel" goal))
      (is (= :done (get-in items ["i1" :state])))
      (is (= :open (get-in items ["i2" :state])))
      (is (= ["keep it small"] (map :text notes))))
    (testing "closing twice is refused"
      (is (thrown-with-msg? Exception #"already done" (item/item-close s ["i1" "dropped" "x"]))))
    (testing "unknown items are refused"
      (is (thrown-with-msg? Exception #"no such item" (item/item-close s ["i9" "done" "x"]))))
    (testing "flags may come before the intent"
      (let [id (item/item-add s ["--gate" "bb test" "gated first"])
            item (get-in (status/project (store/read-events s)) [:items id])]
        (is (= ["gated first" "bb test"] [(:intent item) (:gate item)]))))
    (testing "an intent that looks like a flag, or a stray argument, is refused"
      (is (thrown-with-msg? Exception #"usage" (item/item-add s ["--gat" "bb test" "x"])))
      (is (thrown-with-msg? Exception #"usage" (item/item-add s ["--gate" "bb test"])))
      (is (thrown-with-msg? Exception #"usage" (item/item-add s ["one" "two"]))))))

(deftest appends-are-checked
  (let [s (new-session "declared events")
        logged #(count (store/read-events s))
        n (logged)]
    (testing "an undeclared type is refused"
      (is (thrown-with-msg? Exception #"undeclared event type :item/renamed"
                            (kernel/append! s (constantly {:type :item/renamed :item/id "i1"})))))
    (testing "a missing required key is refused"
      (is (thrown-with-msg? Exception #":item/added needs :intent"
                            (kernel/append! s (constantly {:type :item/added :item/id "i1"}))))
      (is (thrown-with-msg? Exception #"needs :intent"
                            (kernel/append! s (constantly {:type :item/added :item/id "i1" :intent nil})))))
    (testing "a human type from any other actor is refused"
      (doseq [who ["s1" "d1" "claude"]]
        (binding [kernel/*actor* who]
          (is (thrown-with-msg? Exception (re-pattern (str "is the human's, not " who "'s"))
                                (kernel/append! s (constantly {:type :session/allowed :command "bb test"}))))
          (is (thrown-with-msg? Exception #"human's"
                                (kernel/append! s (constantly {:type :ask/answered :ask/id "a1" :answer "y"})))))))
    (is (= n (logged)) "nothing refused reached the log")
    (testing ":by is the actor unless the event names one; nil writes nothing"
      (binding [kernel/*actor* "s1"]
        (is (= "s1" (:by (kernel/append! s (constantly {:type :note/added :note/id "n9" :text "x"})))))
        (is (= "s2" (:by (kernel/append! s (constantly {:type :spawn/exited :spawn/id "s2" :by "s2"}))))))
      (is (= "human" (:by (kernel/append! s (constantly {:type :session/allowed :command "bb test"})))))
      (is (nil? (kernel/append! s (constantly nil))))
      (is (= (+ n 3) (logged))))
    (testing "reading stays lenient: an undeclared type in the log still folds"
      (store/append! s (constantly {:type :item/renamed :item/id "i1"}))
      (is (= "declared events" (:goal (status/project (store/read-events s))))))))

(deftest features-are-data
  (let [s (new-session "features")
        tally {:name "tally"
               :events {:tally/counted {:req #{:n}}}
               :init {:tally 0}
               :fold {:tally/counted (fn [st e] (update st :tally + (:n e)))
                      :note/added (fn [st _] (update st :tally inc))}
               :guards {:tally/counted (fn [st e] (when (> (+ (:tally st) (:n e)) 10) "tally full"))}}]
    (testing "a feature bound for a run declares, folds and guards its events"
      (binding [kernel/*features* (conj features/all tally)]
        (kernel/append! s (constantly {:type :tally/counted :n 4}))
        (note/note s ["also counts"])
        (is (= 5 (:tally (kernel/project (store/read-events s)))))
        (is (thrown-with-msg? Exception #"tally full"
                              (kernel/append! s (constantly {:type :tally/counted :n 6}))))
        (is (= "features" (:goal (kernel/project (store/read-events s)))) "built-in folds still run")))
    (testing "without it, its events are undeclared and skipped when folding"
      (is (thrown-with-msg? Exception #"undeclared event type"
                            (kernel/append! s (constantly {:type :tally/counted :n 1}))))
      (is (nil? (:tally (status/project (store/read-events s))))))
    (testing "nothing bound is an error, not an empty state"
      (binding [kernel/*features* nil]
        (is (thrown? IllegalStateException (kernel/project [])))))))

(deftest commands-are-data
  (let [s (new-session "commands")
        echo {:name "echo"
              :commands [{:path ["echo"] :needs :session :usage "usage: lume echo <word>" :help "lume echo <word>   says it"
                          :run (fn [ctx args] (str (= s (:session ctx)) " " (str/join " " args)))}
                         {:path ["echo" "loud"] :needs :none :usage "usage: lume echo loud <word>"
                          :run (fn [ctx args] (str (contains? ctx :session) " " (str/upper-case (first args))))}]}]
    (binding [kernel/*features* (conj features/all echo) store/*session* s]
      (testing "the longest matching path runs, with what it needs in ctx and the rest as args"
        (is (= "true hi there" (kernel/run ["echo" "hi" "there"])))
        (is (= "false HI" (kernel/run ["echo" "loud" "hi"]))))
      (testing "-h/--help prints the usage"
        (is (= "usage: lume echo loud <word>" (kernel/run ["echo" "loud" "--help"])))
        (is (= "usage: lume echo <word>" (kernel/run ["echo" "-h"]))))
      (testing "lume help lists each :help in order, the extension's last"
        (doseq [args [[] ["help"] ["--help"]]]
          (let [h (kernel/run args)]
            (is (str/starts-with? h "lume install [--agent A]"))
            (is (str/ends-with? h "lume echo <word>   says it")))))
      (testing "a group with no such subcommand lists its subcommands; an unknown command is refused"
        (is (thrown-with-msg? Exception #"usage: lume item add\|gate\|close\|show\|measure \.\.\." (kernel/run ["item" "nope"])))
        (is (thrown-with-msg? Exception #"unknown command: nope" (kernel/run ["nope"])))))
    (testing "every built-in command has a usage and a help entry"
      (doseq [c (kernel/commands)]
        (is (str/starts-with? (:usage c) (str "usage: lume " (str/join " " (:path c)))) (pr-str (:path c)))
        (is (re-find (re-pattern (str "(?m)^lume " (str/join " " (:path c)) "\\b")) (:help c)) (pr-str (:path c)))))))

(deftest subitems-show-under-their-parent
  (let [s (new-session "tree")]
    (item/item-add s ["parent"])
    (item/item-add s ["sibling"])
    (is (= "i3" (item/item-add s ["emerged" "--parent" "i1"])))
    (item/item-add s ["--parent" "i3" "deeper"])
    (is (= "i1" (get-in (status/project (store/read-events s)) [:items "i3" :parent])))
    (testing "open children are indented under their parent"
      (is (= ["open    i1  parent" "          i3  emerged" "            i4  deeper" "        i2  sibling"]
             (->> (str/split-lines (status/render "sess" (store/read-events s) (Instant/now)))
                  (drop-while #(not (str/starts-with? % "open")))
                  (take 4)))))
    (testing "lume spec nests them; a child of a closed item stands alone"
      (is (= [{:intent "parent" :items [{:intent "emerged" :items [{:intent "deeper"}]}]} {:intent "sibling"}]
             (:items (edn/read-string (spec/show-spec s [])))))
      (item/item-close s ["i1" "dropped" "x"])
      (is (= [{:intent "sibling"} {:intent "emerged" :items [{:intent "deeper"}]}]
             (:items (edn/read-string (spec/show-spec s []))))))
    (testing "the parent must be an open item"
      (is (thrown-with-msg? Exception #"already dropped" (item/item-add s ["x" "--parent" "i1"])))
      (is (thrown-with-msg? Exception #"no such item" (item/item-add s ["x" "--parent" "i9"])))
      (is (thrown-with-msg? Exception #"usage" (item/item-add s ["x" "--parent"]))))))

(deftest init-sets-a-default-gate-and-allows
  (let [before (set (fs/list-dir *root*))
        out (session/init *root* ["ship it" "--gate" "bb test" "--allow" "jj" "--allow" "make" "--allow" "jj"])
        s (store/current-session *root*)]
    (is (str/includes? out "default gate bb test · workers may run jj · make"))
    (is (= before (set (fs/list-dir *root*))) "init writes nothing inside the repo")
    (is (= [:session/started :session/gated :session/allowed :session/allowed]
           (map :type (store/read-events s))))
    (testing "items added without --gate inherit the default; --gate overrides it"
      (item/item-add s ["inherits"])
      (item/item-add s ["own gate" "--gate" "true"])
      (let [{:keys [items allow default-gate]} (status/project (store/read-events s))]
        (is (= ["bb test" "true"] (map #(get-in items [% :gate]) ["i1" "i2"])))
        (is (= ["jj" "make"] allow))
        (is (= "bb test" default-gate))))
    (is (str/includes? (status/render "sess" (store/read-events s) (Instant/now))
                       "allow   default gate bb test · jj · make"))
    (testing "a default gate alone still gets an allow line; with none, items stay ungated"
      (let [s2 (do (session/init *root* ["other" "--gate" "make check"]) (store/current-session *root*))]
        (is (str/includes? (status/render "sess" (store/read-events s2) (Instant/now)) "allow   default gate make check\n"))
        (let [s3 (new-session "plain")]
          (item/item-add s3 ["loose"])
          (is (nil? (get-in (status/project (store/read-events s3)) [:items "i1" :gate]))))))
    (testing "bad flags are refused, and only the human may --allow"
      (doseq [args [["g" "--gate"] ["g" "--allow"] ["g" "--gate" "a" "--gate" "b"] ["g" "--gat" "x"] ["g" "h"] ["--gate" "x"]]]
        (is (thrown-with-msg? Exception #"usage" (session/init *root* args)) (pr-str args)))
      (is (thrown-with-msg? Exception #"usage" (item/item-add s ["x" "--gate"])) "a trailing --gate is not ungated")
      (binding [kernel/*actor* "d1"]
        (is (thrown-with-msg? Exception #"only the human" (session/init *root* ["g" "--allow" "rm"])))
        (is (str/includes? (session/init *root* ["g" "--gate" "bb test"]) "default gate bb test"))))))

(deftest sessions-in-the-same-second-are-distinct
  (let [ss (repeatedly 3 #(new-session "goal"))]
    (is (= 3 (count (distinct ss))))
    (is (= (last ss) (store/current-session *root*)))
    (is (every? #(= 1 (count (store/read-events %))) ss))))

(deftest ls-lists-sessions-and-status-reads-past-ones
  (is (str/includes? (inspect/ls *root* []) "no sessions"))
  (let [old (new-session "first goal")
        _ (item/item-add old ["only" "--gate" "true"])
        _ (item/item-close old ["i1" "done" "ok"])
        new (new-session "second goal")
        _ (item/item-add new ["open one"])
        _ (ask/ask new ["which db?"])
        [old-id new-id] (map store/session-id [old new])
        old-events (store/read-events old)
        [l1 l2 :as lines] (str/split-lines (inspect/ls *root* []))]
    (testing "newest first, one line each, current marked"
      (is (= 2 (count lines)))
      (is (str/starts-with? l1 (str "* " new-id " second goal · 1 open 0 closed · ")))
      (is (str/ends-with? l1 "· ask a1 open"))
      (is (str/starts-with? l2 (str "  " old-id " first goal · 0 open 1 closed · ")))
      (is (str/ends-with? l2 "· all closed")))
    (testing "status and log read a past session by id"
      (let [st (inspect/show-status *root* [old-id])]
        (is (str/starts-with? st (str "lume status · " old-id " (not current)")))
        (is (str/includes? st "first goal")))
      (is (not (str/includes? (inspect/show-status *root* []) "(not current)")))
      (is (not (str/includes? (inspect/show-status *root* [new-id]) "(not current)")))
      (is (= (str ";; +" (dec (count old-events)) " earlier events — -n " (count old-events) "\n"
                  (pr-str (last old-events)))
             (inspect/show-log *root* ["--session" old-id "-n" "1"])))
      (is (= (last (store/read-events new)) (edn/read-string (inspect/show-log *root* ["-n" "1"])))))
    (testing "reading writes nothing, and the current session stays current"
      (is (= old-events (store/read-events old)))
      (is (= new-id (store/session-id (store/current-session *root*)))))
    (testing "unknown ids and stray args are refused"
      (is (thrown-with-msg? Exception #"no session nope" (inspect/show-status *root* ["nope"])))
      (is (thrown-with-msg? Exception #"no session nope" (inspect/show-log *root* ["--session" "nope"])))
      (is (thrown-with-msg? Exception #"usage: lume status" (inspect/show-status *root* [old-id new-id])))
      (is (thrown-with-msg? Exception #"usage: lume log" (inspect/show-log *root* ["x"])))
      (is (thrown-with-msg? Exception #"usage: lume ls" (inspect/ls *root* ["x"]))))
    (testing "bounded with a +N hint; --all shows every one"
      (dotimes [_ 10] (new-session "more"))
      (let [lines (str/split-lines (inspect/ls *root* []))]
        (is (= 11 (count lines)))
        (is (= "+2 more — lume ls --all" (last lines))))
      (is (= 12 (count (str/split-lines (inspect/ls *root* ["--all"]))))))
    (testing "a suffix sorts as a number, so -10 is newer than -9"
      (let [dir (fs/path (store/repo-dir *root*) "sessions")]
        (doseq [id ["29990101-000000" "29990101-000000-9" "29990101-000000-10"]]
          (fs/create-dirs (fs/path dir id)))
        (is (= ["29990101-000000-10" "29990101-000000-9" "29990101-000000"]
               (map store/session-id (take 3 (store/sessions *root*))))))))
  (testing "the outcome is what the session is doing, else how it ended"
    (let [t (str (Instant/now))
          line #(inspect/session-line "x" (map (fn [e] (assoc e :t t)) %) (Instant/now) false)
          base [{:type :session/started :goal "g"} {:type :item/added :item/id "i1" :intent "a"}]]
      (is (str/ends-with? (line base) "· idle"))
      (is (str/ends-with? (line (take 1 base)) "· no items"))
      (is (str/ends-with? (line (conj base {:type :spawn/started :spawn/id "s1" :item/id "i1"})) "· running s1"))
      (is (str/ends-with? (line (conj base {:type :drive/started})) "· driving"))
      (is (str/ends-with? (line (conj base {:type :drive/started} {:type :drive/stopped :why "3 turns run"}))
                          "· stopped: 3 turns run")))))

(deftest spawn-logs-start-and-exit
  (let [s (new-session "goal")
        id (item/item-add s ["do the thing"])
        line (agent/spawn s *root* [id "--agent" "test"])
        {:keys [spawns items]} (status/project (store/read-events s))
        sp (get spawns "s1")]
    (is (str/starts-with? line "s1 i1 exit 3"))
    (is (= 3 (:exit sp)))
    (is (= "done by s1" (:result sp)) "worker sees its own spawn id as LUME_ACTOR")
    (is (= ["s1"] (get-in items ["i1" :spawns])))
    (is (fs/exists? (:out sp)))))

(defmethod agents/command :slow [_ _prompt _opts]
  ["sh" "-c" "echo started; while [ ! -f go ]; do sleep 0.05; done; echo finished"])

(deftest running-spawns-show-progress
  (binding [agent/*progress-ms* 0]
    (let [s (new-session "goal")
          id (item/item-add s ["take a while"])
          run (future (agent/spawn s *root* [id "--agent" "slow"]))
          progress #(get-in (status/project (store/read-events s)) [:spawns "s1" :progress])]
      (loop [n 0] (when (and (nil? (progress)) (< n 200)) (Thread/sleep 25) (recur (inc n))))
      (is (= {:steps 1 :last "started"} (select-keys (progress) [:steps :last])))
      (is (= "started\n" (slurp (get-in (status/project (store/read-events s)) [:spawns "s1" :out])))
          "out is written as the worker goes")
      (is (re-find #"s1 running · \d+s ago · 1 step · started \(\d+s ago\)"
                   (status/render "sess" (store/read-events s) (Instant/now))))
      (spit (str (fs/path *root* "go")) "")
      (is (re-find #"^s1 i1 exit 0 in \d+s · finished" @run))
      (is (= 2 (get-in (status/project (store/read-events s)) [:spawns "s1" :steps])))
      (is (re-find #"s1 exit 0 · \d+s ago · 2 steps · "
                   (status/render "sess" (store/read-events s) (Instant/now)))))))

(deftest claude-stream-json
  (let [lines [(json/generate-string {:type "system" :subtype "init"})
               (json/generate-string {:type "assistant" :message {:content [{:type "text" :text "Looking."}]}})
               (json/generate-string {:type "assistant"
                                      :message {:content [{:type "tool_use" :name "Bash" :input {:command "bb test"}}]}})
               (json/generate-string {:type "user" :message {:content [{:type "tool_result" :content "ok"}]}})
               (json/generate-string {:type "result" :result "all done" :session_id "abc" :total_cost_usd 0.5})]
        prog (reduce #(agents/progress :claude %1 %2) {} lines)]
    (is (= {:steps 1 :last "Bash bb test"} prog))
    (is (= {:result "all done" :agent-session "abc" :cost-usd 0.5}
           (agents/summarize :claude (str/join "\n" lines))))
    (testing "output that isn't stream-json falls back to the last line"
      (is (= {:result "boom"} (agents/summarize :claude "boom"))))))

;; ACP: a fake agent replays a real transcript (dev/acp_record.clj) and logs what lume sent.
(defn- fake-acp
  ([log] (fake-acp "test/data/acp-transcript.jsonl" log))
  ([transcript log] ["bb" (str (fs/absolutize "test/fake_acp.clj")) (str (fs/absolutize transcript)) log]))

(defn- acp-permission [{:keys [kind command file]}]
  {"toolCall" {"kind" kind "rawInput" (cond-> {} command (assoc "command" command) file (assoc "file_path" file))}})

(deftest acp-permissions-follow-the-allow-list
  (let [worker {:allow ["bb test"]}
        driver {:allow ["bb test"] :role :driver}]
    (are [opts req ok] (= ok (agents/acp-permit? opts (acp-permission req)))
      worker {:kind "execute" :command "bb test"} true
      worker {:kind "execute" :command "bb test 2>&1 | tail -20"} true
      worker {:kind "execute" :command "lume note hi"} true
      worker {:kind "execute" :command "bb testing"} false
      worker {:kind "execute" :command "bb test && rm -rf ."} false
      worker {:kind "execute" :command "bb test $(rm -rf .)"} false
      worker {:kind "execute" :command "bb test > out"} false
      worker {:kind "execute" :command "bb test & rm -rf ."} false
      worker {:kind "execute" :command "bb test&rm -rf ."} false
      worker {:kind "execute" :command "bb test 2>&1 & rm -rf ."} false
      worker {:kind "execute" :command "bb test || (rm -rf .)"} false
      worker {:kind "execute" :command "bb test (rm -rf .)"} false
      worker {:kind "execute" :command "bb test\rrm -rf ."} false
      worker {:kind "execute" :command "bb test &"} true
      worker {:kind "execute" :command "lume note \"x (y) <z> a; b | c && d & e\""} true
      worker {:kind "execute" :command "lume note 'x (y) $(z) `w` \"q\"'"} true
      worker {:kind "execute" :command "lume note \"say \\\"hi\\\" for \\$5\""} true
      worker {:kind "execute" :command "lume note x\\;y"} true
      worker {:kind "execute" :command "lume note \"$(rm -rf .)\""} false
      worker {:kind "execute" :command "lume note \"`rm -rf .`\""} false
      worker {:kind "execute" :command "lume note \"x\"; rm -rf ."} false
      worker {:kind "execute" :command "lume note \"x\\\"; rm -rf .; echo \""} true
      worker {:kind "execute" :command "lume note \\' ; rm -rf . ; echo '"} false
      worker {:kind "execute" :command "lume note \"x; rm -rf ."} false
      worker {:kind "execute" :command "lume note 'x; rm -rf ."} false
      worker {:kind "execute" :command "lume note \"x\nrm -rf .\""} false
      worker {:kind "execute" :command "jj st"} false
      {:allow ["test -f x && grep -q y x"]} {:kind "execute" :command "test -f x && grep -q y x"} true
      {:allow ["test -f x && grep -q y x"]} {:kind "execute" :command "test -f x && rm -rf ."} false
      worker {:kind "edit" :file "a.clj"} true
      worker {:kind "fetch"} false
      driver {:kind "execute" :command "jj st"} true
      driver {:kind "execute" :command "jj git push"} false
      driver {:kind "edit" :file "a.clj"} false
      driver {:kind "read" :file "a.clj"} true)))

(deftest acp-bridge-replays-a-real-transcript
  (doseq [[role option] [[nil "allow-once"] [:driver "reject"]]]
    (let [log (str (fs/path *root* (str "acp-" (name (or role :worker)) ".log")))
          out (with-out-str (is (= 0 (agents/acp-bridge {:agent (fake-acp log) :role role} "hi"))))
          sent (map json/parse-string (str/split-lines (slurp log)))]
      (testing (str (or role :worker) " answers the Write permission with " option)
        (is (= ["initialize" "session/new" "session/prompt" nil] (map #(get % "method") sent)))
        (is (= option (get-in (last sent) ["result" "outcome" "optionId"]))))
      (is (= {:steps 2 :last "I ran `echo hello` and saved its output, \"hello\", to `hello.txt`."}
             (reduce #(agents/progress :claude-acp %1 %2) {} (str/split-lines out))))
      (is (= {:result "I ran `echo hello` and saved its output, \"hello\", to `hello.txt`."
              :agent-session "a4b0ead2-46e4-40df-bb2e-fb6ce5336a71" :cost-usd 0.10858539999999998}
             (agents/summarize :claude-acp out)))))
  (testing "an agent that exits early is an error"
    (let [out (with-out-str (is (= 1 (agents/acp-bridge {:agent ["true"]} "hi"))))]
      (is (:agent-error (agents/summarize :claude-acp out))))))

;; Devin's permission requests carry only a toolCallId; the bridge reads them over
;; the tool_call that came first. Its message chunks have no messageId.
(deftest acp-bridge-replays-a-devin-transcript
  (doseq [[role answers] [[nil ["allow_once" "allow_once"]] [:driver ["allow_once" "reject_once"]]]]
    (let [log (str (fs/path *root* (str "devin-" (name (or role :worker)) ".log")))
          out (with-out-str (is (= 0 (agents/acp-bridge {:agent (fake-acp "test/data/devin-acp-transcript.jsonl" log)
                                                          :allow ["echo"] :role role} "hi"))))
          sent (map json/parse-string (str/split-lines (slurp log)))]
      (testing (str (or role :worker) " answers echo, then the write, from the allow list")
        (is (= answers (map #(get-in % ["result" "outcome" "optionId"]) (drop 3 sent)))))
      (is (= {:steps 2 :last "Created hello.txt with the output."}
             (reduce #(agents/progress :devin %1 %2) {} (str/split-lines out))))
      (is (= {:result "Created hello.txt with the output." :agent-session "ethereal-salsa"}
             (agents/summarize :devin out)))
      (is (not (str/includes? out "agent_thought_chunk")) "thinking stays out of the spawn's output"))))

(deftest devin-spawn-logs-permissions
  (binding [agents/*acp-agent* (fake-acp "test/data/devin-acp-transcript.jsonl" (str (fs/path *root* "devin.log")))]
    (let [s (new-session "goal")
          id (item/item-add s ["write hello"])]
      (is (re-find #"^s1 i1 exit 0 in \d+s · Created hello.txt" (agent/spawn s *root* [id "--agent" "devin"])))
      (is (= [["execute" "echo hello" :reject] ["edit" "/tmp/lume-acp-15701034023853507021/hello.txt" :allow]]
             (->> (store/read-events s) (filter #(= :spawn/permission (:type %))) (map (juxt :tool :command :disposition)))))
      (is (= :devin (:agent (first (filter #(= :spawn/started (:type %)) (store/read-events s)))))))))

(deftest acp-spawn-logs-permissions-and-exits
  (binding [agents/*acp-agent* (fake-acp (str (fs/path *root* "acp.log")))]
    (let [s (new-session "goal")
          id (item/item-add s ["write hello"])]
      (is (re-find #"^s1 i1 exit 0 in \d+s · I ran" (agent/spawn s *root* [id "--agent" "claude-acp"])))
      (let [events (store/read-events s)]
        (is (= {:type :spawn/permission :spawn/id "s1" :tool "Write" :disposition :allow :by "s1"
                :command "Write /tmp/lume-acp-5489780834565444210/hello.txt"}
               (dissoc (first (filter #(= :spawn/permission (:type %)) events)) :t)))
        (is (= {:exit 0 :steps 2 :cost-usd 0.10858539999999998 :agent-session "a4b0ead2-46e4-40df-bb2e-fb6ce5336a71"}
               (select-keys (last (filter #(= :spawn/exited (:type %)) events)) [:exit :steps :cost-usd :agent-session])))))))

(deftest gates-decide-done
  (let [s (new-session "goal")
        id (item/item-add s ["make ok exist" "--gate" "echo checking; test -f ok"])]
    (testing "a failing gate refuses done and is logged"
      (is (thrown-with-msg? Exception #"i1 not done: gate g1 fail exit 1 · checking"
                            (item/item-close s [id "done" "claimed"])))
      (let [item (get-in (status/project (store/read-events s)) [:items id])]
        (is (= :open (:state item)))
        (is (= 1 (get-in item [:last-gate :exit])))
        (is (str/includes? (slurp (get-in item [:last-gate :out])) "checking"))))
    (testing "the gate runs in the repo, and passing closes the item"
      (spit (str (fs/path *root* "ok")) "")
      (is (= "i1 done · g2 pass" (item/item-close s [id "done" "made it"])))
      (is (= :done (get-in (status/project (store/read-events s)) [:items id :state]))))
    (testing "dropping skips the gate"
      (let [id2 (item/item-add s ["never passes"])]
        (item/item-gate s [id2 "false"])
        (is (= "i2 dropped" (item/item-close s [id2 "dropped" "nope"])))))))

(deftest spawn-runs-the-gate
  (let [s (new-session "goal")
        pass (item/item-add s ["passes" "--gate" "true"])
        fail (item/item-add s ["fails" "--gate" "false"])]
    (is (str/ends-with? (agent/spawn s *root* [pass "--agent" "test"]) "i1 done · g1 pass"))
    (is (str/includes? (agent/spawn s *root* [fail "--agent" "test"]) "i2 not done: gate g2 fail exit 1"))
    (let [{:keys [items]} (status/project (store/read-events s))]
      (is (= :done (get-in items [pass :state])))
      (is (= :open (get-in items [fail :state]))))
    (is (str/includes? (status/render "sess" (store/read-events s) (Instant/now))
                       "gate false · g2 fail exit 1"))))

(deftest item-show-prints-what-status-clips
  (let [s (new-session "goal")
        intent (str "a long intent " (str/join " " (repeat 40 "word")) " END")
        id (item/item-add s [intent "--gate" "false"])
        other (item/item-add s ["unrelated"])]
    (note/note s ["about i1: it matters"])
    (note/note s ["about i11 and i10, not this one"])
    (agent/spawn s *root* [id "--agent" "test"])
    (agent/spawn s *root* [other "--agent" "test"])
    (let [out (item/item-show s [id])
          lines (str/split-lines out)]
      (is (= "i1 open" (first lines)))
      (is (str/includes? out (str "intent  " intent)) "the intent is not clipped")
      (is (str/includes? out "gate    false"))
      (is (str/includes? out "note/added n1 about i1"))
      (is (not (str/includes? out "n2")) "i11 and i10 don't mention i1")
      (is (str/includes? out "spawn/started s1"))
      (is (str/includes? out "spawn/exited s1 exit 3 done by s1"))
      (is (str/includes? out "gate/ran g1 exit 1 false"))
      (is (not (str/includes? out "s2")) "other items' spawns are left out"))
    (testing "a closed item shows its disposition"
      (item/item-close s [other "dropped" "not needed"])
      (is (str/starts-with? (item/item-show s [other]) "i2 dropped — not needed")))
    (testing "events are capped to the last 20"
      (dotimes [n 25] (note/note s [(str "i2 note " n)]))
      (let [lines (str/split-lines (item/item-show s [other]))]
        (is (= 24 (count lines)))
        (is (re-find #"^\+\d+ earlier events" (nth lines 3)))
        (is (str/includes? (last lines) "i2 note 24"))))
    (testing "unknown items are refused"
      (is (thrown-with-msg? Exception #"no such item" (item/item-show s ["i9"]))))))

(defmethod agents/command :allowed[_ _prompt {:keys [allow]}]
  ["echo" (str/join "|" allow)])

(deftest workers-may-run-allowed-commands-and-their-gate
  (let [s (new-session "goal")
        id (item/item-add s ["verify" "--gate" "true"])]
    (agent/allow s ["jj"])
    (agent/allow s ["bb test"])
    (agent/allow s ["jj"])
    (is (str/includes? (agent/spawn s *root* [id "--agent" "allowed"]) "· jj|bb test|true\n"))
    (is (str/includes? (status/render "sess" (store/read-events s) (Instant/now)) "allow   jj · bb test"))
    (testing "revoking removes a command; only the human changes permissions"
      (agent/allow s ["--revoke" "jj"])
      (is (= ["bb test"] (:allow (status/project (store/read-events s)))))
      (doseq [who ["s1" "d1"]]
        (binding [kernel/*actor* who]
          (is (thrown-with-msg? Exception #"only the human changes permissions" (agent/allow s ["rm"])))
          (is (thrown-with-msg? Exception #"only the human changes permissions" (agent/allow s ["--revoke" "bb test"]))))))))

(deftest only-the-exact-human-holds-policy
  (let [s (new-session "goal")]
    (ask/ask s ["ok?"])
    (doseq [who ["claude" "s1" "d1" "Human" "human " ""]]
      (binding [kernel/*actor* who]
        (is (thrown-with-msg? Exception #"only the human changes permissions" (agent/allow s ["rm"])) who)
        (is (thrown-with-msg? Exception #"only the human answers" (ask/answer s ["a1" "yes"])) who)))
    (is (empty? (:allow (status/project (store/read-events s)))))
    (is (= "a1 answered" (ask/answer s ["a1" "yes"])))))

(deftest human-only-needs-a-terminal-and-a-confirmation
  (let [s (new-session "goal")
        confirms (atom [])
        typed #(fn [prompt expected] (swap! confirms conj [prompt expected]) (= % expected))]
    (ask/ask s ["ok?"])
    (testing "a wrong confirmation is refused"
      (binding [kernel/*confirm* (typed "a2")]
        (is (thrown-with-msg? Exception #"need an interactive terminal and a typed confirmation"
                              (ask/answer s ["a1" "yes"])))
        (is (thrown-with-msg? Exception #"typed confirmation" (agent/allow s ["rm"])))
        (is (thrown-with-msg? Exception #"typed confirmation" (session/init *root* ["g" "--allow" "rm"])))))
    (is (= [["Answer a1: yes" "a1"] ["Workers may run: rm." "yes"]
            ["Workers will be allowed to run: rm." "yes"]] @confirms)
        "answer asks for the ask id, permissions for yes")
    (is (= :open (get-in (status/project (store/read-events s)) [:asks "a1" :state])))
    (is (= s (store/current-session *root*)) "a refused init starts no session")
    (testing "LUME_ACTOR is still checked first, before any prompt"
      (reset! confirms [])
      (binding [kernel/*actor* "s1" kernel/*confirm* (typed "a1")]
        (is (thrown-with-msg? Exception #"only the human answers" (ask/answer s ["a1" "yes"]))))
      (is (empty? @confirms)))
    (testing "the human at a terminal who types it passes"
      (binding [kernel/*confirm* (typed "a1")]
        (is (= "a1 answered" (ask/answer s ["a1" "yes"]))))
      (binding [kernel/*confirm* (typed "yes")]
        (is (= "workers may run rm" (agent/allow s ["rm"])))))
    (testing "with no tty (as in spawns and drives) the real check refuses, naming the rule"
      (ask/ask s ["again?"])
      (let [{:keys [exit err]} (p/shell {:dir *root* :in "" :out :string :err :string :continue true
                                         :extra-env {"LUME_HOME" store/*home* "LUME_SESSION" s
                                                     "LUME_ACTOR" "human" "LUME_SPAWNED_AS" ""}}
                                        "bb" "-cp" (str (fs/absolutize "src")) "-m" "lume.cli"
                                        "answer" "a2" "yes")]
        (is (not= 0 exit))
        (is (re-find #"human-only commands need an interactive terminal" err))
        (is (= 1 (count (str/split-lines (str/trim err)))) "a one-line reason")
        (is (= :open (get-in (status/project (store/read-events s)) [:asks "a2" :state])))))))

(deftest claude-args-from-env
  (is (= ["--allowedTools" "Bash(bb test)" "--allowedTools" "Bash(jj st:*)"]
         (agents/claude-args "[\"--allowedTools\" \"Bash(bb test)\"]" ["jj st"]))
      "EDN keeps arguments with spaces whole")
  (is (= ["--permission-mode" "acceptEdits" "--allowedTools" "Bash(lume:*),Bash(bin/lume:*),Bash(bb test:*)"]
         (agents/claude-args nil ["bb test"])))
  (is (thrown-with-msg? Exception #"must be an EDN vector of strings"
                        (agents/claude-args "--permission-mode acceptEdits" []))))

(deftest concurrent-writers-get-unique-ids
  (let [s (new-session "goal")
        procs (doall (for [n (range 8)]
                       (p/process ["bb" "-cp" "src" "-m" "lume.cli" "item" "add" (str "item " n)]
                                  {:extra-env {"LUME_SESSION" s} :out :string :err :string})))
        ids (map #(str/trim (:out @%)) procs)]
    (is (= (set (map #(str "i" %) (range 1 9))) (set ids)))
    (is (= 9 (count (store/read-events s))))))

(deftest status-stays-bounded
  (testing "every section full still fits in 40 lines"
    (let [s (new-session (apply str "goal " (repeat 200 "g")))]
      (dotimes [n 20] (agent/allow s [(str "cmd-" n " " (apply str (repeat 20 "a")))]))
      (dotimes [n 200] (item/item-add s [(str "item " n " " (apply str (repeat 50 "x"))) "--gate" "true"]))
      (dotimes [n 20] (item/item-close s [(str "i" (inc n)) "dropped" (apply str (repeat 60 "w"))]))
      (dotimes [n 50] (note/note s [(str "note " n)]))
      (dotimes [n 20] (note/note s ["--pin" (str "lesson " n " " (apply str (repeat 120 "y")))]))
      (dotimes [n 10] (ask/ask s [(str "question " n " " (apply str (repeat 120 "q")))]))
      (ask/answer s ["a1" (apply str (repeat 120 "z"))])
      (ask/answer s ["a2" "yes"])
      (policy/budget s ["--usd" "1000" "--spawns" "2000" "--minutes" "60000"])
      (let [out (status/render "sess" (store/read-events s) (Instant/now))
            lines (str/split-lines out)]
        (is (<= (count lines) 40) out)
        (is (every? #(<= (count %) 110) lines) out)
        (doseq [section ["goal" "asks" "allow" "budget" "pinned" "open" "closed" "notes" "recent" "next"]]
          (is (re-find (re-pattern (str "(?m)^" section " ")) out) section))
        (is (str/includes? out "+6 more asks — lume ask") "8 open asks of 3 lines each, 2 whole asks shown")
        (is (str/includes? out "+17 more") "20 pinned, 3 shown: long asks take the room of 3")
        (is (str/includes? out "+49 more") "50 notes and 2 answers, 3 shown beside pinned")
        (is (str/includes? out "+16 more") "20 closed, 4 shown")
        (is (str/includes? out "+176 more items") "180 open with gates, 4 whole items shown")
        (is (re-find #"(?m)^        i24  .*\n            gate true\n        \+176" out)
            "an item's gate line is never cut off from it")))))

(deftest status-notes-take-unused-room
  (testing "without pinned notes, notes get the room back"
    (let [s (new-session "goal")]
      (dotimes [n 50] (note/note s [(str "note " n)]))
      (is (str/includes? (status/render "sess" (store/read-events s) (Instant/now)) "+45 more")))))

(deftest status-and-prompts-are-data
  (let [s (new-session "frame")
        id (item/item-add s ["watched"])
        noisy {:name "noisy"
               :status [{:label "noisy" :order 55 :cap 2
                         :lines (fn [_ _ _] (map #(str "line " % " " (apply str (repeat 200 "z"))) (range 9)))}
                        {:label "plenty" :order 56 :lines (fn [_ _ _] (map #(str "u" %) (range 20)))}]
               :item-lines (fn [_ item _] [(str "watching " (:id item))])
               :prompt (fn [_ item] [(str "Noisy says: mind " (:id item))])}]
    (binding [kernel/*features* (conj features/all noisy)]
      (let [out (status/render "sess" (store/read-events s) (Instant/now))
            lines (str/split-lines out)]
        (testing "the frame caps a section that returns too many lines, with a +N more hint"
          (is (re-find #"(?m)^noisy   line 0 z+…\n        line 1 z+…\n        \+7 more — lume log$" out))
          (is (re-find #"(?m)^plenty  u0\n(        u\d\n){4}        \+15 more — lume log$" out) "no :cap is a default cap"))
        (testing "and clips every line to the width"
          (is (every? #(<= (count %) 100) lines) out))
        (testing "a feature's :item-lines sit under each open item, in feature order"
          (is (re-find #"(?m)^open    i1  watched\n            watching i1$" out)))
        (testing "sections sit in :order among the built-ins"
          (is (< (str/index-of out "noisy ") (str/index-of out "open "))))
        (is (<= (count lines) 40) out))
      (testing "a feature's :prompt joins the worker prompt"
        (agent/spawn s *root* [id "--agent" "prompt"])
        (let [prompt (slurp (get-in (status/project (store/read-events s)) [:spawns "s1" :out]))]
          (is (str/includes? prompt "\nNoisy says: mind i1\n")))))))

(deftest status-colour-is-for-humans-only
  (let [s (new-session "goal")
        t (str (Instant/now))]
    (ask/ask s ["which db?"])
    (item/item-add s ["passes" "--gate" "true"])
    (item/item-add s ["fails" "--gate" "false"])
    (store/append! s (constantly {:type :gate/ran :gate/id "g1" :item/id "i1" :gate "true" :exit 0 :t t}))
    (store/append! s (constantly {:type :gate/ran :gate/id "g2" :item/id "i2" :gate "false" :exit 1 :t t}))
    (store/append! s (constantly {:type :spawn/started :spawn/id "s1" :item/id "i2" :t t}))
    (let [events (store/read-events s)
          now (Instant/now)
          plain (status/render "sess" events now)
          coloured (status/render "sess" events now {:color true})
          esc "\u001b["]
      (is (not (str/includes? plain esc)) "drivers and tests get plain text")
      (is (= plain (status/strip-color coloured)) "colour changes no visible text, width or line count")
      (doseq [[what re] {"labels dim" #"(?m)^\e\[2mgoal\e\[0m    goal$"
                         "open asks yellow" #"\e\[33ma1 which db\?\e\[0m"
                         "gate pass green" #"g1 \e\[32mpass\e\[0m"
                         "gate fail red" #"g2 \e\[31mfail exit 1\e\[0m"
                         "running spawns cyan" #"s1 \e\[36mrunning\e\[0m"}]
        (is (re-find re coloured) what))
      (testing "the CLI colours only a terminal without NO_COLOR, or with --color"
        (is (inspect/color? false true nil))
        (is (not (inspect/color? false true "1")))
        (is (not (inspect/color? false false nil)))
        (is (inspect/color? true false "1"))
        (is (str/includes? (inspect/show-status *root* ["--color"]) esc))
        (is (thrown-with-msg? Exception #"usage: lume status" (inspect/show-status *root* ["--colour"])))))))

(defmethod agents/command :prompt [_ prompt _opts]
  ["printf" "%s" prompt])

(deftest asks-wait-for-the-human
  (let [s (new-session "goal")
        id (item/item-add s ["use the answer"])]
    (binding [kernel/*actor* "s1"]
      (is (= "a1" (ask/ask s ["ship on fridays?"]))))
    (is (= "a2" (ask/ask s ["which db?"])))
    (let [out (status/render "sess" (store/read-events s) (Instant/now))]
      (is (re-find #"(?m)^goal    goal\nasks    a1 ship on fridays\?\n        a2 which db\?$" out)
          "open asks come right after the goal"))
    (testing "only the human answers"
      (doseq [who ["s1" "d1"]]
        (binding [kernel/*actor* who]
          (is (thrown-with-msg? Exception #"only the human answers" (ask/answer s ["a1" "sure"]))))))
    (testing "answers move from asks to notes"
      (is (= "a1 answered" (ask/answer s ["a1" "never"])))
      (is (thrown-with-msg? Exception #"a1 is already answered" (ask/answer s ["a1" "ok fine"])))
      (is (thrown-with-msg? Exception #"no such ask: a9" (ask/answer s ["a9" "x"])))
      (let [out (status/render "sess" (store/read-events s) (Instant/now))]
        (is (re-find #"(?m)^asks    a2 which db\?\nopen " out) "a1 has left asks")
        (is (re-find #"(?m)^notes   \[\d\d:\d\d human\] a1 → never$" out))))
    (testing "workers see answered asks with the notes"
      (agent/spawn s *root* [id "--agent" "prompt"])
      (let [prompt (slurp (get-in (status/project (store/read-events s)) [:spawns "s1" :out]))]
        (is (str/includes? prompt "- a1 ship on fridays? → never\n"))
        (is (not (str/includes? prompt "which db")))))))

(deftest replay-renders-from-the-log-alone
  (let [s (new-session "replay me")
        sid (store/session-id s)
        _ (item/item-add s ["first" "--gate" "true"])
        _ (store/append! s (constantly {:type :drive/turn :drive/id "d1" :n 1 :by "human"}))
        _ (store/append! s (constantly {:type :drive/exited :drive/id "d1" :exit 0 :result "spawn i1 next"}))
        _ (agent/spawn s *root* ["i1" "--agent" "test"])
        _ (note/note s ["after the spawn"])
        events (store/read-events s)
        m (count events)
        [d1 s1] (keep-indexed #(when (status/decision? %2) (inc %1)) events)
        fresh (str (fs/create-temp-dir {:prefix "lume-replay"}))]
    (try
      ;; Only log.edn crosses over: no current file, no spawn output, no lock.
      (binding [store/*home* fresh]
        (let [dir (fs/path (store/repo-dir *root*) "sessions" sid)]
          (fs/create-dirs dir)
          (fs/copy (fs/path s "log.edn") (fs/path dir "log.edn")))
        (let [replay #(status/strip-color (inspect/replay *root* (cons sid %)))]
          (testing "no flags: the decisions, each with the event that replays it"
            (let [[a b footer] (str/split-lines (replay []))]
              (is (str/starts-with? a (str d1 " ")))
              (is (str/ends-with? a "d1 exit 0 · spawn i1 next"))
              (is (str/starts-with? b (str s1 " ")))
              (is (str/ends-with? b "s1 exit 3 · done by s1"))
              (is (str/starts-with? footer (str "lume replay --at N: the screen after event N (of " m ")"))))
            (let [[hint row :as lines] (str/split-lines (replay ["-n" "1"]))]
              (is (= 3 (count lines)))
              (is (= "+1 earlier decisions — -n 2" hint))
              (is (str/starts-with? row (str s1 " ")))))
          (testing "--at N: status as it stood, with the decision that ended there"
            (let [out (replay ["--at" (str s1)])]
              (is (str/starts-with? out (str "lume replay · " sid " · event " s1 " of " m " · ")))
              (is (str/includes? out "decided s1 exit 3 · done by s1"))
              (is (str/includes? out "s1 exit 3 · 0s ago") "ages read as they did at event N")
              (is (not (str/includes? out "after the spawn")) "later events aren't in it")
              (is (not (str/includes? out "lume item add")) "no next commands in a replay"))
            (let [out (replay ["--at" "1"])]
              (is (str/includes? out "goal    replay me"))
              (is (not (str/includes? out "i1"))))
            (is (str/includes? (replay ["--at" (str m)]) "after the spawn"))
            (is (not (str/includes? (replay ["--at" (str m)]) "decided")) "the last event is a note"))
          (testing "--step: the screen after each decision, at a terminal"
            (let [shown (atom [])]
              (binding [kernel/*read-line* (fn [screen prompt] (swap! shown conj [screen prompt]) "")]
                (is (= "end of replay: 2 decisions" (replay ["--step"]))))
              (is (= [(str "decision 1 of 2 · event " d1 " of " m) (str "decision 2 of 2 · event " s1 " of " m)]
                     (map #(re-find #"decision \d of \d · event \d+ of \d+" (second %)) @shown)))
              (is (str/includes? (status/strip-color (first (second @shown))) "decided s1 exit 3")))
            (binding [kernel/*read-line* (constantly "q")]
              (is (= (str "stopped at event " d1 " — lume replay --at " d1) (replay ["--step"]))))
            (binding [kernel/*read-line* (constantly "")]
              (is (= "end of replay: 1 decision" (replay ["--step" "--at" (str (inc d1))])) "--at sets where stepping starts"))
            (is (= (replay []) (replay ["--step"])) "no terminal: the list instead"))
          (testing "out of range or stray args are refused"
            (is (thrown-with-msg? Exception #"outside the log: events 1\.\." (replay ["--at" "0"])))
            (is (thrown-with-msg? Exception #"outside the log" (replay ["--at" (str (inc m))])))
            (is (thrown-with-msg? Exception #"usage: lume replay" (replay ["--at" "x"])))
            (is (thrown-with-msg? Exception #"usage: lume replay" (replay ["extra"])))
            (is (thrown-with-msg? Exception #"no session nope" (inspect/replay *root* ["nope"]))))))
      (finally (fs/delete-tree fresh)))
    (is (= events (store/read-events s)) "replay writes nothing")))

(defn- nrepl-eval
  "Evaluate code in ns over nREPL at port: the value, or the error's message."
  [port ns code]
  ;; Not require: see kernel/ns-lock.
  (let [write (requiring-resolve 'bencode.core/write-bencode)
        read (requiring-resolve 'bencode.core/read-bencode)
        text #(if (bytes? %) (String. ^bytes %) %)]
    (with-open [sock (java.net.Socket. "127.0.0.1" (int port))]
      (let [out (.getOutputStream sock)
            in (java.io.PushbackInputStream. (.getInputStream sock))]
        (write out {"op" "eval" "code" code "ns" (str ns) "id" "1"})
        (.flush out)
        (loop [acc {}]
          (let [msg (update-vals (read in) text)
                acc (merge-with str acc (select-keys msg ["value" "err" "ex"]))]
            (if (some #(= "done" (text %)) (get msg "status"))
              (or (get acc "value") (get acc "err") (get acc "ex"))
              (recur acc))))))))

(deftest repl-serves-a-session-and-logs-only-that-it-opened
  (let [s (new-session "prototype here")
        _ (item/item-add s ["first" "--gate" "true"])
        _ (note/note s ["a note"])
        events (store/read-events s)
        files #(set (map str (fs/list-dir s)))
        before (files)
        ;; A namespace of its own: tests run in parallel, and user is shared.
        ns (symbol (str "lume.repl-test-" (System/nanoTime)))
        {:keys [server port text]} (inspect/start-repl s {:ns ns})
        ev (partial nrepl-eval port ns)
        opened (last (store/read-events s))
        events (conj events opened)]
    (try
      (testing "it logs that it opened, on which port and by whom"
        (is (= {:type :repl/opened :port port :by "human"} (dissoc opened :t))))
      (testing "it says where it listens and how to connect"
        (is (pos? port))
        (is (str/starts-with? text (str "lume repl · " (store/session-id s) " · " (count events) " events · nREPL on 127.0.0.1:" port)))
        (is (str/includes? text (str "clj-nrepl-eval -p " port))))
      (testing "events, state, render and the lume namespaces are in scope"
        (is (= (str (count events)) (ev "(count events)")))
        (is (= ":repl/opened" (ev "(:type (last events))")))
        (is (= "\"prototype here\"" (ev "(:goal state)")))
        (is (= "true" (ev "(str/includes? (render) \"i1  first\")")))
        (is (= "true" (ev "(str/starts-with? (render 1) \"lume replay\")")))
        (is (= "true" (ev "(= state (status/project (store/read-events session)))")))
        (is (= "true" (ev "(fn? spec/problems)"))))
      (testing "measure runs a candidate as a spec would, in the repo, and logs nothing"
        (is (= "[0 {\"a\" 1, \"b\" 2.5} true]"
               (ev "(let [r (measure \"echo a=1 b=2.5; pwd\")] [(:exit r) (:values r) (str/includes? (:output r) (:repo state))])")))
        (is (= "[3 1 {\"n\" [4 4]} true]"
               (ev "(let [r (measure \"echo n=4\" {:runs 3 :drop 1})] [(:runs r) (:drop r) (:spread r) (int? (:ms r))])")))
        (is (= "[2 nil]" (ev "((juxt :exit :values) (measure \"exit 2\"))")))
        (is (= (str (count events)) (ev "(count (store/read-events session))"))))
      (testing "refresh! picks up what a live session logged since"
        (note/note s ["later"])
        (is (= (str (count events)) (ev "(count events)")))
        (is (= (str (inc (count events))) (ev "(refresh!)")))
        (is (= "\"later\"" (ev "(:text (last events))"))))
      (finally ((requiring-resolve 'babashka.nrepl.server/stop-server!) server)
               (remove-ns ns)))
    (is (= before (files)) "no files added to the session")
    (is (= events (pop (store/read-events s))) "the repl logged repl/opened and nothing else; then the test's note")
    (is (thrown-with-msg? Exception #"usage: lume repl" (inspect/repl *root* ["a" "b"])))
    (is (thrown-with-msg? Exception #"no session nope" (inspect/repl *root* ["nope"])))))

(deftest log-queries-filter-then-bound
  (let [s (new-session "goal")
        root *root*
        i1 (item/item-add s ["first"])
        i2 (item/item-add s ["second"])
        run #(->> (str/split-lines (inspect/show-log root %))
                  (remove (fn [l] (or (str/blank? l) (str/starts-with? l ";;"))))
                  (mapv edn/read-string))]
    (store/append! s (constantly {:type :spawn/started :spawn/id "s1" :item/id i1 :by "human"}))
    (store/append! s (constantly {:type :spawn/progress :spawn/id "s1" :text "editing"}))
    (binding [kernel/*actor* "s1"]
      (note/note s ["about i1: done soon"])
      (note/note s ["unrelated (i10)"]))
    (note/note s ["see i2"])
    (testing "--type takes a prefix of ns/name"
      (is (= [:note/added :note/added :note/added] (map :type (run ["--type" "note"]))))
      (is (= [:spawn/started :spawn/progress] (map :type (run ["--type" "spawn/"]))))
      (is (= [:item/added :item/added] (map :type (run ["--type" "item/added"])))))
    (testing "--item: its events, its spawns' (progress too), notes naming it"
      (is (= [:item/added :spawn/started :spawn/progress :note/added]
             (map :type (run ["--item" i1]))))
      (is (= ["see i2"] (keep :text (run ["--item" i2]))))
      (is (thrown-with-msg? Exception #"no such item: i9" (inspect/show-log root ["--item" "i9"]))))
    (testing "--by, --grep, and filters combine with -n"
      (is (= 2 (count (run ["--by" "s1"]))))
      (is (= ["unrelated (i10)"] (keep :text (run ["--by" "s1" "--grep" "i1\\d"]))))
      (is (= ["see i2"] (keep :text (run ["--type" "note" "-n" "1"]))))
      (is (= ";; +2 earlier events — -n 3"
             (first (str/split-lines (inspect/show-log root ["--type" "note" "-n" "1"])))))
      (is (= "" (inspect/show-log root ["--by" "nobody"]))))
    (testing "bad queries are refused"
      (is (thrown-with-msg? Exception #"bad --grep regex" (inspect/show-log root ["--grep" "("])))
      (is (thrown-with-msg? Exception #"usage: lume log" (inspect/show-log root ["--by" "a" "--by" "b"])))
      (is (thrown-with-msg? Exception #"usage: lume log" (inspect/show-log root ["--type"]))))))

(deftest ask-show-reads-an-ask-in-full
  (let [s (new-session "goal")
        id (item/item-add s ["pick a db"])
        long-q (str "which db? " (str/join " " (repeat 60 "word")) " the end")]
    (store/append! s (constantly {:type :spawn/started :spawn/id "s1" :item/id id}))
    (binding [kernel/*actor* "s1"] (ask/ask s [long-q]))
    (ask/ask s ["short?"])
    (testing "status wraps a long question to 3 lines, clips the rest and points at ask show"
      (let [out (status/render "sess" (store/read-events s) (Instant/now))]
        (is (re-find #"(?m)^asks    a1 which db\? (word ?)+\n {11}(word ?)+\n {11}(word ?)+… — lume ask show a1$" out) out)
        (is (re-find #"(?m)^        a2 short\?$" out) "a question that fits has no hint")))
    (testing "lume ask with no question: open asks in full, each with its answer command"
      (let [out (ask/ask s [])]
        (is (str/includes? (str/replace out #"\s+" " ") long-q) "the whole question, unclipped")
        (is (re-find #"(?m)^a1 · asked \S+ by s1 · item i1$" out))
        (is (re-find #"(?m)^  → lume answer a1 " out))
        (is (re-find #"(?m)^a2 · asked \S+ by human$" out))
        (is (every? #(<= (count %) 100) (str/split-lines out)) "wrapped to the terminal (100 without one)"))
      (is (thrown-with-msg? Exception #"usage: lume ask" (ask/ask s ["a" "b"]))))
    (testing "the question in full, who asked and when, the asking worker's item"
      (let [out (ask/ask-show s ["a1"])]
        (is (str/includes? out (str "question " long-q)))
        (is (re-find #"(?m)^a1 open\n" out))
        (is (re-find #"(?m)^asked    \d{4}-\d\d-\d\dT\d\d:\d\d by s1$" out))
        (is (re-find #"(?m)^item     i1$" out))
        (is (str/includes? out "answer   none yet; only the human answers: lume answer a1"))))
    (testing "the answer once there is one; a human's ask concerns no item"
      (ask/answer s ["a2" "yes"])
      (let [out (ask/ask-show s ["a2"])]
        (is (re-find #"(?m)^a2 answered\n" out))
        (is (not (str/includes? out "item")))
        (is (re-find #"(?m)^answer   yes\nanswered \S+ by human$" out))))
    (testing "routing and errors"
      (binding [store/*session* s]
        (is (str/starts-with? (kernel/run ["ask" "show" "a2"]) "a2 answered")))
      (is (thrown-with-msg? Exception #"no such ask: a9" (ask/ask-show s ["a9"])))
      (is (thrown-with-msg? Exception #"usage: lume ask show <id>" (ask/ask-show s []))))
    (testing "no open asks"
      (ask/answer s ["a1" "postgres"])
      (is (= "no open asks" (ask/ask s []))))))

(deftest answer-prompts-at-a-terminal
  (let [s (new-session "goal")
        shown (atom nil)]
    (ask/ask s [(str "which db? " (str/join " " (repeat 40 "word")) " END")])
    (testing "no text and no terminal: refused, and the ask stays open"
      (is (thrown-with-msg? Exception #"need an interactive terminal" (ask/answer s ["a1"])))
      (is (= :open (get-in (status/project (store/read-events s)) [:asks "a1" :state]))))
    (testing "an agent is refused before any prompt"
      (binding [kernel/*actor* "d1" kernel/*read-line* (fn [& _] (throw (ex-info "prompted" {})))]
        (is (thrown-with-msg? Exception #"only the human answers" (ask/answer s ["a1"])))))
    (testing "an unknown or answered ask is refused before any prompt"
      (binding [kernel/*read-line* (fn [& _] (throw (ex-info "prompted" {})))]
        (is (thrown-with-msg? Exception #"no such ask: a9" (ask/answer s ["a9"])))))
    (testing "at a terminal: the full question, then the typed answer, then the typed id"
      (binding [kernel/*read-line* (fn [q prompt] (reset! shown [q prompt]) "  postgres ")
                kernel/*confirm* (fn [prompt expected] (and (= "Answer a1: postgres" prompt) (= "a1" expected)))]
        (is (= "a1 answered" (ask/answer s ["a1"]))))
      (is (str/includes? (str/replace (first @shown) #"\s+" " ") "which db? word"))
      (is (str/includes? (first @shown) "END") "unclipped")
      (is (= "postgres" (get-in (status/project (store/read-events s)) [:notes 0 :answer])))
      (binding [kernel/*read-line* (fn [& _] (throw (ex-info "prompted" {})))]
        (is (thrown-with-msg? Exception #"a1 is already answered" (ask/answer s ["a1"])))))
    (testing "an empty typed answer is refused"
      (ask/ask s ["again?"])
      (binding [kernel/*read-line* (constantly "")]
        (is (thrown-with-msg? Exception #"usage: lume answer" (ask/answer s ["a2"])))))))

;; A fake driver: each turn runs the next shell script, which can call lume.
(defmethod agents/command :driver [_ prompt opts]
  (swap! *driver-calls* conj (assoc opts :prompt prompt))
  (let [script (or (first @*driver-turns*) "true")]
    (swap! *driver-turns* #(vec (rest %)))
    ["sh" "-c" script]))

(defn- lume-cmd [& args]
  (str/join " " (concat ["bb" "-cp" (str (fs/absolutize "src")) "-m" "lume.cli"]
                        (map #(str "'" % "'") args))))

;; A worker whose agent settings set LUME_ACTOR=claude, as Claude Code's do (i46).
(defmethod agents/command :clobbered [_ _prompt _opts]
  ["sh" "-c" (str "LUME_ACTOR=claude " (lume-cmd "note" "from the worker"))])

(deftest spawn-actor-survives-agent-settings
  (let [s (new-session "goal")
        id (item/item-add s ["do the thing"])]
    (agent/spawn s *root* [id "--agent" "clobbered"])
    (is (= "s1" (:by (last (filter #(= :note/added (:type %)) (store/read-events s)))))
        "a worker's lume calls are logged as its spawn id even when its settings set LUME_ACTOR")))

(defmethod agents/command :writes [_ _prompt _opts]
  ["sh" "-c" "echo more >> made"])

(defmethod agents/command :asks [_ _prompt _opts]
  ["sh" "-c" (str "echo more >> made; " (lume-cmd "ask" "which way?"))])

(defmethod agents/command :commits [_ _prompt _opts]
  ["sh" "-c" "git -c user.name=t -c user.email=t@t commit -qm x"])

;; i59: a passing gate says nothing about a spawn that did no work.
(deftest idle-spawns-leave-items-open
  (let [s (new-session "goal")
        sh (fn [& args] (apply p/shell {:dir *root* :out :string :err :string} "git" args))
        state #(get-in (status/project (store/read-events s)) [:items % :state])]
    (sh "init" "-q")
    (spit (str (fs/path *root* "made")) "start\n")
    (testing "a spawn that changes nothing, even in an already dirty repo, leaves the item open with a note"
      (let [id (item/item-add s ["no-op" "--gate" "true"])]
        (is (re-find #"\ni1 left open: s1 changed nothing in the repo · noted n1$" (agent/spawn s *root* [id "--agent" "test"])))
        (is (= :open (state id)))
        (is (= "i1 left open, not gated: s1 changed nothing in the repo"
               (:text (last (filter #(= :note/added (:type %)) (store/read-events s))))))))
    (testing "a spawn that changes a file is gated as before"
      (let [id (item/item-add s ["write" "--gate" "true"])]
        (is (str/ends-with? (agent/spawn s *root* [id "--agent" "writes"]) "i2 done · g1 pass"))
        (is (= :done (state id)))))
    (testing "a spawn that asks leaves the item open, though it changed files"
      (let [id (item/item-add s ["ask" "--gate" "true"])]
        (is (str/includes? (agent/spawn s *root* [id "--agent" "asks"]) "i3 left open: s3 asked a1"))
        (is (= :open (state id)))))
    (testing "a commit is a change"
      (sh "add" "-A")
      (let [id (item/item-add s ["commit" "--gate" "true"])]
        (is (str/ends-with? (agent/spawn s *root* [id "--agent" "commits"]) "i4 done · g2 pass"))))
    (testing "a measured item stays open after a passing gate, for the human or driver to decide"
      (let [id (item/item-add s ["faster" "--gate" "true"])]
        (measure/item-measure s [id "echo t=2"])
        (measure/measure s [id])
        (let [out (agent/spawn s *root* [id "--agent" "writes"])]
          (is (re-find #"\nm2 i5 t=2 · \d+\.\ds · vs m1\ni5 left open to decide · g3 pass · lume item close i5 done\|dropped \"<why>\"$" out) out))
        (is (= :open (state id)))))))

;; i69: an experiment's rows compare with the code before the spawn's edits.
(deftest the-default-agent-is-lume-agent
  (let [s (new-session "goal")]
    (item/item-add s ["do it"])
    (binding [kernel/*default-agent* "test"]
      (agent/spawn s *root* ["i1"]))
    (is (= :test (:agent (first (filter #(= :spawn/started (:type %)) (store/read-events s))))))))

(defmethod agents/command :noter [_ _prompt _opts]
  ["sh" "-c" (lume-cmd "note" "from the worker")])

;; i90: reactions run after append!, in the appending process, except in
;; workers and drivers, whose parent runs them when they exit.
(deftest reactions-follow-appends
  (let [fired (atom [])
        pinger {:name "pinger"
                :events {:pinger/ping {:req #{:n}}}
                :reactions [{:on :note
                             :run (fn [e] (swap! fired conj [(:text e) (:by e) (store/session-id kernel/*session*)]))}
                            {:on :pinger/ping
                             :run (fn [e] (kernel/append! kernel/*session* (constantly {:type :pinger/ping :n (inc (:n e))})))}]}]
    (binding [kernel/*features* (conj features/all pinger)]
      (let [s (new-session "goal")
            sid (store/session-id s)
            id (item/item-add s ["note from a worker"])]
        (testing "a direct append reacts once; :on a namespace matches its types"
          (note/note s ["by hand"])
          (is (= [["by hand" "human" sid]] @fired)))
        (testing "a worker's append reacts once, in its spawn, when it exits"
          (agent/spawn s *root* [id "--agent" "noter"])
          (is (= [["by hand" "human" sid] ["from the worker" "s1" sid]] @fired)))
        (testing "a worker or driver process runs no reactions"
          (binding [kernel/*spawned-as* "s9"] (note/note s ["inside a worker"]))
          (is (= 2 (count @fired))))
        (testing "reactions nest at most max-depth deep"
          (kernel/append! s (constantly {:type :pinger/ping :n 1}))
          (is (= (range 1 (+ 2 kernel/max-depth))
                 (keep #(when (= :pinger/ping (:type %)) (:n %)) (store/read-events s)))))))))

;; Fake drivers summarize like claude, so a turn can report a cost on a result line.
(defmethod agents/summarize :driver [_ out] (agents/summarize :claude out))

(defn- session-from-spec
  "A session started from a spec's events, as lume run will."
  [sp]
  (let [s (store/new-session! *root*)]
    (doseq [e (spec/events sp *root*)]
      (store/append! s (constantly (assoc e :by "human"))))
    s))

(defn- spec-file [sp]
  (let [f (str (fs/path store/*home* (str (gensym "spec") ".edn")))]
    (spit f (if (string? sp) sp (pr-str sp)))
    f))

(deftest specs-round-trip-and-check
  (let [sp {:goal "cut cold build time; tests must stay green"
            :gate "bb test"
            :allow ["bb test" "jj"]
            :budget {:usd 20 :spawns 30 :minutes 120}
            :cadence {:batch 3 :gate-fail true :plateau 4}
            :items [{:intent "cut cold build time" :measure "bb bench" :runs 10 :drop 1}
                    {:intent "document the cache" :gate "bb lint" :items [{:intent "note eviction"}]}
                    {:intent "just measured" :measure "bb size" :higher ["hits"]}]}]
    (is (empty? (spec/problems sp)))
    (testing "spec -> session -> spec"
      (let [s (session-from-spec sp)
            state (status/project (store/read-events s))]
        (is (= sp (edn/read-string (spec/show-spec s []))))
        (is (= ["bb test" "bb lint" "bb test" "bb test"] (map :gate (sort-by :order (vals (:items state)))))
            "items without a gate get the default")
        (is (= {:measure "bb bench" :runs 10 :drop 1} (select-keys (get-in state [:items "i1"]) [:measure :runs :drop])))
        (testing "closed items are left out unless --all"
          (item/item-close s ["i2" "dropped" "not needed"])
          (let [out (spec/show-spec s [])]
            (is (= ["cut cold build time" "note eviction" "just measured"] (map :intent (:items (edn/read-string out)))))
            (is (str/ends-with? out ";; 1 closed item left out — lume spec --all")))
          (is (= sp (edn/read-string (spec/show-spec s ["--all"])))))))
    (testing "a cadence comes back as written: the default :batch 1 isn't added"
      (doseq [c [{} {:gate-fail true} {:plateau 2} {:batch 1 :plateau 2} {:batch 2 :gate-fail true}]]
        (let [sp {:goal "cadence only" :cadence c}
              s (session-from-spec sp)
              state (status/project (store/read-events s))]
          (is (= sp (edn/read-string (spec/show-spec s []))) (pr-str c))
          (is (= (merge {:batch 1} c) (policy/cadence state) (policy/cadence-ceiling state))
              "the default fills in on read"))))
    (testing "session -> spec -> session: a hand-built session's harness is reusable"
      (session/init *root* ["by hand" "--gate" "bb test" "--allow" "jj"])
      (let [s (store/current-session *root*)]
        (item/item-add s ["first"])
        (measure/item-measure s ["i1" "bb bench" "--runs" "5" "--drop" "1"])
        (item/item-add s ["second" "--gate" "make check"])
        (policy/budget s ["--usd" "10"])
        (policy/cadence-cmd s ["batch 2 on plateau 3"])
        (let [sp (edn/read-string (spec/show-spec s []))]
          (is (= {:goal "by hand" :gate "bb test" :allow ["jj"] :budget {:usd 10.0}
                  :cadence {:batch 2 :plateau 3}
                  :items [{:intent "first" :measure "bb bench" :runs 5 :drop 1}
                          {:intent "second" :gate "make check"}]}
                 sp))
          (is (= "ok" (re-find #"ok" (spec/check [(spec-file (spec/show-spec s []))]))))
          (is (= sp (edn/read-string (spec/show-spec (session-from-spec sp) [])))))))
    (testing "a spec prints one key and one item per line"
      (is (= 9 (count (str/split-lines (spec/pretty sp)))))
      (is (= ["{:items [{:intent \"a\"}"
              "         {:intent \"b\" :items"
              "          [{:intent \"c\" :items"
              "            [{:intent \"d\"}"
              "             {:intent \"e\"}]}]}]}"]
             (str/split-lines (spec/pretty {:items [{:intent "a"}
                                                    {:intent "b" :items [{:intent "c" :items [{:intent "d"} {:intent "e"}]}]}]})))))
    (testing "lume check prints ok with a count"
      (let [f (spec-file sp)] (is (= (str f ": ok · 4 items · 2 measured") (spec/check [f])))))
    (testing "lume check lists every problem, with where it is"
      (let [f (spec-file '{:alow [] :gate 3 :budget {:usd -1 :tokens 3}
                           :cadence {:batch 0 :gate-fail "yes"}
                           :items [{:intent "x" :runs 3 :drop 3} "y" {:measure "bb bench" :drop 2 :higher "score"}
                                   {:intent "z" :items [{:runs 2}]}]})
            msg (try (spec/check [f]) nil (catch clojure.lang.ExceptionInfo e (ex-message e)))]
        (is (= [(str f ": 15 problems")
                "  [:alow] unknown key; expected one of :allow :budget :cadence :gate :goal :items"
                "  [:goal] is required"
                "  [:gate] must be a command (non-blank string), got 3"
                "  [:budget :tokens] unknown key; expected one of :minutes :spawns :usd"
                "  [:budget :usd] must be a non-negative number, got -1"
                "  [:cadence :batch] must be a positive whole number, got 0"
                "  [:cadence :gate-fail] must be true or false, got \"yes\""
                "  [:items 0] :runs, :drop, :higher and :before need a :measure"
                "  [:items 0 :drop] must be less than :runs (3), got 3"
                "  [:items 1] must be a map with :intent, got \"y\""
                "  [:items 2 :intent] is required"
                "  [:items 2 :higher] must be a vector of number names, got \"score\""
                "  [:items 2 :drop] must be less than :runs (1), got 2"
                "  [:items 3 :items 0 :intent] is required"
                "  [:items 3 :items 0] :runs, :drop, :higher and :before need a :measure"]
               (str/split-lines msg)))))
    (testing "the list is bounded"
      (let [f (spec-file {:goal "g" :items (vec (repeat 30 {}))})
            lines (str/split-lines (try (spec/check [f]) (catch clojure.lang.ExceptionInfo e (ex-message e))))]
        (is (= (str f ": 30 problems") (first lines)))
        (is (= 22 (count lines)))
        (is (= "  +10 more" (last lines)))))
    (testing "an unreadable file is one problem"
      (doseq [[content problem] [["" "is empty"] ["{:goal" "is not EDN"] ["{:goal \"a\"} {}" "more than one form"]
                                 ["[1 2]" "a spec must be a map"]]]
        (is (thrown-with-msg? Exception (re-pattern (str ": 1 problem\n  .*" problem)) (spec/check [(spec-file content)]))
            content))
      (is (thrown-with-msg? Exception #"no such file" (spec/check ["/no/such/spec.edn"]))))
    (testing "check and spec write nothing in the repo"
      (is (empty? (fs/list-dir *root*))))
    (testing "usage"
      (is (thrown-with-msg? Exception #"usage" (spec/check [])))
      (is (thrown-with-msg? Exception #"usage" (spec/show-spec (store/current-session *root*) ["--open"])))
      (is (= "usage: lume check <spec.edn>" (kernel/run ["check" "--help"]))))))

(defn- run! [& args]
  (let [ret (atom nil)
        out (with-out-str (reset! ret (spec/run-spec *root* (vec args))))]
    [@ret out]))

(deftest run-starts-a-session-from-a-spec-and-drives
  (let [refused #(try (apply run! %&) nil (catch clojure.lang.ExceptionInfo e (ex-message e)))
        sp {:goal "make t small" :gate "true" :budget {:spawns 5}
            :cadence {:batch 1 :gate-fail true :plateau 3}
            :items [{:intent "shrink t" :measure "cat t" :runs 3 :drop 1} {:intent "write it up"}]}]
    (spit (str (fs/path *root* "t")) "t=5")
    (testing "a spec with any problem starts nothing"
      (is (re-find #": 2 problems\n  \[:goal\] is required" (refused (spec-file {:items [{}]}))))
      (is (re-find #"no such file" (refused "/no/such/spec.edn")))
      (is (nil? (store/current-session *root*))))
    (testing "usage"
      (doseq [args [[] ["f" "g" "h"] ["f" "--turns" "-1"] ["f" "--turns" "x"] ["f" "--turns"] ["f" ""]]]
        (is (re-find #"^usage: lume run" (apply refused args)) (pr-str args)))
      (is (re-find #"^usage: lume run" (kernel/run ["run" "--help"]))))
    (testing "it starts the session, baselines measurements, then drives"
      (reset! *driver-turns* [(str (lume-cmd "spawn" "i1" "--agent" "test")) (lume-cmd "item" "close" "i2" "done" "written")
                              (lume-cmd "item" "close" "i1" "done" "t is smaller")])
      (let [[ret out] (run! (spec-file sp) "--agent" "driver" "--turns" "3")
            s (store/current-session *root*)
            events (store/read-events s)
            types (map :type events)]
        (is (= "drive stopped: no open items" ret))
        (is (re-find #"^session \S+ started from \S+ · 2 items · 1 measured\nbaseline m1 i1 t=5 " out))
        (is (= (map :type (spec/events sp *root*)) (take 7 types)) "the spec's events, in order")
        (is (every? #(= "human" (:by %)) (take 7 events)))
        (is (= [:measure/ran :drive/started] (take 2 (drop 7 types))) "the baseline comes before the drive")
        (is (= sp (edn/read-string (spec/show-spec s ["--all"]))) "the session's harness is the spec")
        (is (= {:batch 1 :gate-fail true :plateau 3} (:cadence-ceiling (status/project events)))
            "the human's cadence is the ceiling")
        (is (= ["d1" "d2" "d3"] (keep :drive/id (filter #(= :drive/turn (:type %)) events))))
        (is (= :done (get-in (status/project events) [:items "i1" :state])) "the measured item was the driver's to close")
        (is (= ["m1" "m2"] (keep :measure/id events)) "the spawn on i1 was measured against the baseline")))
    (testing "the goal on the command line replaces the spec's; --turns 0 doesn't drive"
      (let [[ret out] (run! (spec-file sp) "cut t further" "--turns" "0")
            events (store/read-events (store/current-session *root*))]
        (is (str/starts-with? ret "--turns 0: not driving"))
        (is (str/includes? out "baseline m1 i1"))
        (is (= "cut t further" (:goal (status/project events))))
        (is (not-any? #(= :drive/started (:type %)) events))))
    (testing "run inside a session, it starts one alongside and leaves the current one be"
      (let [outer (store/current-session *root*)
            before (store/read-events outer)
            [ret out] (binding [store/*session* outer] (run! (spec-file sp) "--turns" "0"))
            [_ id] (re-find #"^session (\S+) started" out)
            inner (store/find-session *root* id)]
        (is (str/starts-with? ret "--turns 0"))
        (is (str/includes? out (str "not made current: run inside " (store/session-id outer))))
        (is (= outer (store/current-session *root*)))
        (is (not= outer inner))
        (is (= ["m1"] (keep :measure/id (store/read-events inner))) "the baseline went to the new session")
        (is (= before (store/read-events outer)) "and nothing to the outer one")))
    (testing "an agent runs the same harness, policy aside"
      (binding [kernel/*actor* "d7"]
        (let [[ret] (run! (spec-file sp) "--turns" "0")
              events (store/read-events (store/current-session *root*))]
          (is (str/starts-with? ret "--turns 0"))
          (is (every? #(= "d7" (:by %)) events))
          (is (= sp (edn/read-string (spec/show-spec (store/current-session *root*) ["--all"]))))
          (is (= policy/default-cadence (policy/cadence-ceiling (status/project events)))
              "an agent's cadence is no ceiling")))
      (let [before (store/current-session *root*)]
        (doseq [[extra why] [[{:allow ["jj"]} #"workers may run jj"]
                             [{:cadence {:batch 3}} #"the cadence ceiling is raised: batch 1 → 3"]]]
          (binding [kernel/*actor* "s4"]
            (is (re-find (re-pattern (str "^only the human runs a spec that sets policy \\(.*" why))
                         (refused (spec-file (merge sp extra)) "--turns" "0"))))
          (testing "and the human must confirm it at a terminal"
            (binding [kernel/*confirm* (constantly false)]
              (is (re-find #"interactive terminal" (refused (spec-file (merge sp extra)) "--turns" "0"))))))
        (is (= before (store/current-session *root*)) "a refused run starts no session")))
    (testing "a human's confirmed policy is logged as theirs"
      (run! (spec-file (assoc sp :allow ["jj"] :cadence {:batch 3})) "--turns" "0")
      (let [state (status/project (store/read-events (store/current-session *root*)))]
        (is (= ["jj"] (:allow state)))
        (is (= 3 (:batch (policy/cadence-ceiling state))))))))

(deftest claude-driver-args
  (is (= ["--model" "opus" "--tools" "Bash,Read"
          "--allowedTools" "Bash(lume:*),Bash(bin/lume:*),Bash(jj:*),Bash(bb test:*),Read"
          "--disallowedTools" "Bash(jj git push:*),Bash(git push:*)"]
         (agents/claude-driver-args "[\"--model\" \"opus\"]" ["bb test"]))))

(deftest claude-runs-get-no-mcp-servers
  (doseq [role [:worker :driver]]
    (is (some #{"--strict-mcp-config"} (agents/command :claude "p" {:role role :allow []}))
        (str (name role) " runs must not load the user's MCP servers"))))

(deftest install-writes-the-skill-and-prints-the-settings-line
  (let [claude agents/*claude-home*
        skill (str (fs/path claude "skills" "lume" "SKILL.md"))
        settings (str (fs/path claude "settings.json"))]
    (is (str/starts-with? claude store/*home*) "tests never touch the real ~/.claude")
    (let [out (install/install [])]
      (is (str/includes? out (str "wrote     " skill)))
      (is (str/includes? out "\"LUME_ACTOR\": \"claude\""))
      (is (not (fs/exists? settings)) "settings are printed, never applied"))
    (testing "the skill teaches the lume loop and the human's boundaries"
      (let [text (slurp skill)]
        (is (str/starts-with? text "---\nname: lume\ndescription: "))
        (doseq [s ["lume status" "lume item add \"<what and why>\" --gate" "lume spawn" "lume drive"
                   "lume note" "lume ask" "lume allow" "lume answer" "Never push"]]
          (is (str/includes? text s) s))))
    (testing "re-running changes nothing"
      (is (str/includes? (install/install []) (str "unchanged " skill))))
    (testing "an edited skill is restored"
      (spit skill "old")
      (is (str/includes? (install/install []) (str "updated   " skill))))
    (testing "a settings file that already sets the actor is reported, not edited"
      (spit settings "{\"env\": {\"LUME_ACTOR\": \"claude\"}}")
      (let [out (install/install ["--agent" "claude"])]
        (is (str/includes? out "LUME_ACTOR=claude is set"))
        (is (not (str/includes? out "todo"))))
      (is (= "{\"env\": {\"LUME_ACTOR\": \"claude\"}}" (slurp settings))))
    (testing "devin: the same skill in devin's skills dir, lume's devin config, and the shell lines"
      (let [out (install/install ["--agent" "devin"])
            devin-skill (str (fs/path agents/*devin-home* "skills" "lume" "SKILL.md"))
            config (str (fs/path store/*home* "agents" "devin.json"))]
        (is (str/includes? out (str "wrote     " devin-skill)))
        (is (= (slurp skill) (slurp devin-skill)))
        (is (= ["exec" "edit" "Write(/**)"] (get-in (json/parse-string (slurp config)) ["permissions" "ask"])))
        (is (str/includes? out "alias devin='LUME_ACTOR=devin devin'"))))
    (is (thrown-with-msg? Exception #"knows no agent" (install/install ["--agent" "nope"])))
    (is (empty? (fs/list-dir *root*)) "nothing is written in the repo")))

(defn- tree
  "Every path under dir with its bytes (:dir for a directory), to compare two
  trees byte for byte."
  [dir]
  (into (sorted-map)
        (for [f (rest (file-seq (fs/file dir)))]
          [(str (fs/relativize dir f)) (if (fs/directory? f) :dir (vec (fs/read-all-bytes f)))])))

(defn- differing
  "The paths whose bytes differ between two trees, or that only one has."
  [a b]
  (sort (filter #(not= (get a %) (get b %)) (distinct (concat (keys a) (keys b))))))

(deftest clean-leaves-the-repo-as-it-was
  (let [env {"GIT_AUTHOR_NAME" "t" "GIT_AUTHOR_EMAIL" "t@t" "GIT_COMMITTER_NAME" "t" "GIT_COMMITTER_EMAIL" "t@t"
             "JJ_USER" "t" "JJ_EMAIL" "t@t" "JJ_CONFIG" (str (fs/path store/*home* "jj.toml"))}
        sh #(apply p/shell {:dir *root* :out :string :err :string :extra-env env} %&)
        control (str (fs/path store/*home* "control"))
        dir (store/repo-dir *root*)]
    (spit (str (fs/path *root* "README")) "a repo\n")
    (sh "git" "init" "-q")
    (sh "git" "add" "README")
    (sh "git" "commit" "-q" "-m" "first")
    (sh "jj" "git" "init" "--colocate")
    (fs/copy-tree *root* control)
    (testing "a harness run: init, items, a measurement, a spawned worker"
      (let [s (new-session "goal")
            id (item/item-add s ["do the thing" "--gate" "true"])]
        (measure/item-measure s [id "echo build_s=1"])
        (measure/measure s [id])
        (is (str/includes? (agent/spawn s *root* [id "--agent" "test"]) "done"))))
    ;; Lume makes neither yet; clean must remove them when it does.
    (sh "git" "update-ref" "refs/lume/spawns/s1" "HEAD")
    (sh "git" "worktree" "add" "-q" "--detach" (str (fs/path dir "worktrees" "s1")))
    (is (seq (differing (tree control) (tree *root*))))
    (testing "workers and drivers may not clean"
      (binding [kernel/*actor* "s1"]
        (is (thrown-with-msg? Exception #"don't delete the log" (clean/clean *root* ["--yes"])))))
    (testing "without --yes it only lists what it would remove"
      (let [e (is (thrown? Exception (clean/clean *root* [])))]
        (is (str/starts-with? (ex-message e) "refused without --yes"))
        (is (str/includes? (ex-message e) "  ref refs/lume/spawns/s1"))
        (is (str/includes? (ex-message e) (str "  dir " dir))))
      (is (fs/exists? dir)))
    (testing "--yes removes it all and says what"
      (let [out (clean/clean *root* ["--yes"])]
        (is (= [(str "removed worktree " (fs/path dir "worktrees" "s1"))
                "removed ref refs/lume/spawns/s1"
                (str "removed dir " (fs/path *root* ".git" "refs" "lume"))
                (str "removed dir " dir)]
               (str/split-lines out))))
      (is (not (fs/exists? dir)))
      (is (empty? (differing (tree control) (tree *root*))) "the repo is byte-identical to the control"))
    (is (str/starts-with? (clean/clean *root* []) "nothing to clean"))))

(deftest clean-one-session
  (let [old (new-session "old")
        cur (new-session "current")
        dir (store/repo-dir *root*)]
    (is (thrown-with-msg? Exception #"no session nope" (clean/clean *root* ["--session" "nope" "--yes"])))
    (is (= (str "removed dir " old) (clean/clean *root* ["--session" (store/session-id old) "--yes"])))
    (is (= cur (store/current-session *root*)) "the current session is untouched")
    (testing "the last session takes the repo's state with it"
      (is (= (str "removed dir " dir) (clean/clean *root* ["--session" (store/session-id cur) "--yes"])))
      (is (not (fs/exists? dir))))
    (testing "cleaning the current session, with others left, unsets current"
      (let [_ (new-session "a")
            b (new-session "b")]
        (is (= [(str "removed dir " b) (str "removed file " (fs/path dir "current"))]
               (str/split-lines (clean/clean *root* ["--session" (store/session-id b) "--yes"]))))
        (is (nil? (store/current-session *root*)))))))
