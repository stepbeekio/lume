(ns lume.kernel-test
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [are deftest is testing use-fixtures]]
            [lume.agents :as agents]
            [lume.cli :as cli]
            [lume.spec :as spec]
            [lume.status :as status]
            [lume.store :as store]
            [org.httpkit.server :as httpkit])
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
        (binding [store/*home* home store/*session* nil cli/*actor* "human" *root* root
                  cli/*confirm* (constantly true) cli/*read-line* (constantly nil)
                  cli/*source* nil cli/*ntfy* nil cli/*reexec* (fn [& _] (throw (ex-info "no exec in tests" {})))
                  *driver-turns* (atom []) *driver-calls* (atom [])
                  agents/*claude-home* (str (fs/path home "claude"))
                  agents/*devin-home* (str (fs/path home "devin"))]
          (t))
        (finally (fs/delete-tree home) (fs/delete-tree root))))))

(defn- new-session [goal]
  (cli/init *root* [goal])
  (store/current-session *root*))

(defmethod agents/command :test [_ _prompt _opts]
  ["sh" "-c" "printf 'working\\ndone by %s\\n' \"$LUME_ACTOR\"; exit 3"])

(deftest items-and-notes
  (let [s (new-session "build the kernel")]
    (is (= "i1" (cli/item-add s ["first"])))
    (is (= "i2" (cli/item-add s ["second"])))
    (cli/item-close s ["i1" "done" "works"])
    (cli/note s ["keep it small"])
    (let [{:keys [goal items notes]} (status/project (store/read-events s))]
      (is (= "build the kernel" goal))
      (is (= :done (get-in items ["i1" :state])))
      (is (= :open (get-in items ["i2" :state])))
      (is (= ["keep it small"] (map :text notes))))
    (testing "closing twice is refused"
      (is (thrown-with-msg? Exception #"already done" (cli/item-close s ["i1" "dropped" "x"]))))
    (testing "unknown items are refused"
      (is (thrown-with-msg? Exception #"no such item" (cli/item-close s ["i9" "done" "x"]))))
    (testing "flags may come before the intent"
      (let [id (cli/item-add s ["--gate" "bb test" "gated first"])
            item (get-in (status/project (store/read-events s)) [:items id])]
        (is (= ["gated first" "bb test"] [(:intent item) (:gate item)]))))
    (testing "an intent that looks like a flag, or a stray argument, is refused"
      (is (thrown-with-msg? Exception #"usage" (cli/item-add s ["--gat" "bb test" "x"])))
      (is (thrown-with-msg? Exception #"usage" (cli/item-add s ["--gate" "bb test"])))
      (is (thrown-with-msg? Exception #"usage" (cli/item-add s ["one" "two"]))))))

(deftest init-sets-a-default-gate-and-allows
  (let [before (set (fs/list-dir *root*))
        out (cli/init *root* ["ship it" "--gate" "bb test" "--allow" "jj" "--allow" "make" "--allow" "jj"])
        s (store/current-session *root*)]
    (is (str/includes? out "default gate bb test · workers may run jj · make"))
    (is (= before (set (fs/list-dir *root*))) "init writes nothing inside the repo")
    (is (= [:session/started :session/gated :session/allowed :session/allowed]
           (map :type (store/read-events s))))
    (testing "items added without --gate inherit the default; --gate overrides it"
      (cli/item-add s ["inherits"])
      (cli/item-add s ["own gate" "--gate" "true"])
      (let [{:keys [items allow default-gate]} (status/project (store/read-events s))]
        (is (= ["bb test" "true"] (map #(get-in items [% :gate]) ["i1" "i2"])))
        (is (= ["jj" "make"] allow))
        (is (= "bb test" default-gate))))
    (is (str/includes? (status/render "sess" (store/read-events s) (Instant/now))
                       "allow   default gate bb test · jj · make"))
    (testing "a default gate alone still gets an allow line; with none, items stay ungated"
      (let [s2 (do (cli/init *root* ["other" "--gate" "make check"]) (store/current-session *root*))]
        (is (str/includes? (status/render "sess" (store/read-events s2) (Instant/now)) "allow   default gate make check\n"))
        (let [s3 (new-session "plain")]
          (cli/item-add s3 ["loose"])
          (is (nil? (get-in (status/project (store/read-events s3)) [:items "i1" :gate]))))))
    (testing "bad flags are refused, and only the human may --allow"
      (doseq [args [["g" "--gate"] ["g" "--allow"] ["g" "--gate" "a" "--gate" "b"] ["g" "--gat" "x"] ["g" "h"] ["--gate" "x"]]]
        (is (thrown-with-msg? Exception #"usage" (cli/init *root* args)) (pr-str args)))
      (is (thrown-with-msg? Exception #"usage" (cli/item-add s ["x" "--gate"])) "a trailing --gate is not ungated")
      (binding [cli/*actor* "d1"]
        (is (thrown-with-msg? Exception #"only the human" (cli/init *root* ["g" "--allow" "rm"])))
        (is (str/includes? (cli/init *root* ["g" "--gate" "bb test"]) "default gate bb test"))))))

(deftest sessions-in-the-same-second-are-distinct
  (let [ss (repeatedly 3 #(new-session "goal"))]
    (is (= 3 (count (distinct ss))))
    (is (= (last ss) (store/current-session *root*)))
    (is (every? #(= 1 (count (store/read-events %))) ss))))

(deftest ls-lists-sessions-and-status-reads-past-ones
  (is (str/includes? (cli/ls *root* []) "no sessions"))
  (let [old (new-session "first goal")
        _ (cli/item-add old ["only" "--gate" "true"])
        _ (cli/item-close old ["i1" "done" "ok"])
        new (new-session "second goal")
        _ (cli/item-add new ["open one"])
        _ (cli/ask new ["which db?"])
        [old-id new-id] (map store/session-id [old new])
        old-events (store/read-events old)
        [l1 l2 :as lines] (str/split-lines (cli/ls *root* []))]
    (testing "newest first, one line each, current marked"
      (is (= 2 (count lines)))
      (is (str/starts-with? l1 (str "* " new-id " second goal · 1 open 0 closed · ")))
      (is (str/ends-with? l1 "· ask a1 open"))
      (is (str/starts-with? l2 (str "  " old-id " first goal · 0 open 1 closed · ")))
      (is (str/ends-with? l2 "· all closed")))
    (testing "status and log read a past session by id"
      (let [st (cli/show-status *root* [old-id])]
        (is (str/starts-with? st (str "lume status · " old-id " (not current)")))
        (is (str/includes? st "first goal")))
      (is (not (str/includes? (cli/show-status *root* []) "(not current)")))
      (is (not (str/includes? (cli/show-status *root* [new-id]) "(not current)")))
      (is (= (str ";; +" (dec (count old-events)) " earlier events — -n " (count old-events) "\n"
                  (pr-str (last old-events)))
             (cli/show-log *root* ["--session" old-id "-n" "1"])))
      (is (= (last (store/read-events new)) (edn/read-string (cli/show-log *root* ["-n" "1"])))))
    (testing "reading writes nothing, and the current session stays current"
      (is (= old-events (store/read-events old)))
      (is (= new-id (store/session-id (store/current-session *root*)))))
    (testing "unknown ids and stray args are refused"
      (is (thrown-with-msg? Exception #"no session nope" (cli/show-status *root* ["nope"])))
      (is (thrown-with-msg? Exception #"no session nope" (cli/show-log *root* ["--session" "nope"])))
      (is (thrown-with-msg? Exception #"usage: lume status" (cli/show-status *root* [old-id new-id])))
      (is (thrown-with-msg? Exception #"usage: lume log" (cli/show-log *root* ["x"])))
      (is (thrown-with-msg? Exception #"usage: lume ls" (cli/ls *root* ["x"]))))
    (testing "bounded with a +N hint; --all shows every one"
      (dotimes [_ 10] (new-session "more"))
      (let [lines (str/split-lines (cli/ls *root* []))]
        (is (= 11 (count lines)))
        (is (= "+2 more — lume ls --all" (last lines))))
      (is (= 12 (count (str/split-lines (cli/ls *root* ["--all"]))))))
    (testing "a suffix sorts as a number, so -10 is newer than -9"
      (let [dir (fs/path (store/repo-dir *root*) "sessions")]
        (doseq [id ["29990101-000000" "29990101-000000-9" "29990101-000000-10"]]
          (fs/create-dirs (fs/path dir id)))
        (is (= ["29990101-000000-10" "29990101-000000-9" "29990101-000000"]
               (map store/session-id (take 3 (store/sessions *root*))))))))
  (testing "the outcome is what the session is doing, else how it ended"
    (let [t (str (Instant/now))
          line #(status/session-line "x" (map (fn [e] (assoc e :t t)) %) (Instant/now) false)
          base [{:type :session/started :goal "g"} {:type :item/added :item/id "i1" :intent "a"}]]
      (is (str/ends-with? (line base) "· idle"))
      (is (str/ends-with? (line (take 1 base)) "· no items"))
      (is (str/ends-with? (line (conj base {:type :spawn/started :spawn/id "s1" :item/id "i1"})) "· running s1"))
      (is (str/ends-with? (line (conj base {:type :drive/started})) "· driving"))
      (is (str/ends-with? (line (conj base {:type :drive/started} {:type :drive/stopped :why "3 turns run"}))
                          "· stopped: 3 turns run")))))

(deftest spawn-logs-start-and-exit
  (let [s (new-session "goal")
        id (cli/item-add s ["do the thing"])
        line (cli/spawn s *root* [id "--agent" "test"])
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
  (binding [cli/*progress-ms* 0]
    (let [s (new-session "goal")
          id (cli/item-add s ["take a while"])
          run (future (cli/spawn s *root* [id "--agent" "slow"]))
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
          id (cli/item-add s ["write hello"])]
      (is (re-find #"^s1 i1 exit 0 in \d+s · Created hello.txt" (cli/spawn s *root* [id "--agent" "devin"])))
      (is (= [["execute" "echo hello" :reject] ["edit" "/tmp/lume-acp-15701034023853507021/hello.txt" :allow]]
             (->> (store/read-events s) (filter #(= :spawn/permission (:type %))) (map (juxt :tool :command :disposition)))))
      (is (= :devin (:agent (first (filter #(= :spawn/started (:type %)) (store/read-events s)))))))))

(deftest acp-spawn-logs-permissions-and-exits
  (binding [agents/*acp-agent* (fake-acp (str (fs/path *root* "acp.log")))]
    (let [s (new-session "goal")
          id (cli/item-add s ["write hello"])]
      (is (re-find #"^s1 i1 exit 0 in \d+s · I ran" (cli/spawn s *root* [id "--agent" "claude-acp"])))
      (let [events (store/read-events s)]
        (is (= {:type :spawn/permission :spawn/id "s1" :tool "Write" :disposition :allow :by "s1"
                :command "Write /tmp/lume-acp-5489780834565444210/hello.txt"}
               (dissoc (first (filter #(= :spawn/permission (:type %)) events)) :t)))
        (is (= {:exit 0 :steps 2 :cost-usd 0.10858539999999998 :agent-session "a4b0ead2-46e4-40df-bb2e-fb6ce5336a71"}
               (select-keys (last (filter #(= :spawn/exited (:type %)) events)) [:exit :steps :cost-usd :agent-session])))))))

(deftest gates-decide-done
  (let [s (new-session "goal")
        id (cli/item-add s ["make ok exist" "--gate" "echo checking; test -f ok"])]
    (testing "a failing gate refuses done and is logged"
      (is (thrown-with-msg? Exception #"i1 not done: gate g1 fail exit 1 · checking"
                            (cli/item-close s [id "done" "claimed"])))
      (let [item (get-in (status/project (store/read-events s)) [:items id])]
        (is (= :open (:state item)))
        (is (= 1 (get-in item [:last-gate :exit])))
        (is (str/includes? (slurp (get-in item [:last-gate :out])) "checking"))))
    (testing "the gate runs in the repo, and passing closes the item"
      (spit (str (fs/path *root* "ok")) "")
      (is (= "i1 done · g2 pass" (cli/item-close s [id "done" "made it"])))
      (is (= :done (get-in (status/project (store/read-events s)) [:items id :state]))))
    (testing "dropping skips the gate"
      (let [id2 (cli/item-add s ["never passes"])]
        (cli/item-gate s [id2 "false"])
        (is (= "i2 dropped" (cli/item-close s [id2 "dropped" "nope"])))))))

(deftest spawn-runs-the-gate
  (let [s (new-session "goal")
        pass (cli/item-add s ["passes" "--gate" "true"])
        fail (cli/item-add s ["fails" "--gate" "false"])]
    (is (str/ends-with? (cli/spawn s *root* [pass "--agent" "test"]) "i1 done · g1 pass"))
    (is (str/includes? (cli/spawn s *root* [fail "--agent" "test"]) "i2 not done: gate g2 fail exit 1"))
    (let [{:keys [items]} (status/project (store/read-events s))]
      (is (= :done (get-in items [pass :state])))
      (is (= :open (get-in items [fail :state]))))
    (is (str/includes? (status/render "sess" (store/read-events s) (Instant/now))
                       "gate false · g2 fail exit 1"))))

(deftest measurements-are-ledger-rows
  (testing "named numbers, else the last number"
    (is (= {"build_s" 9.5 "hits" 40 "tests" 21} (cli/measure-values "build_s=12 hits: 40\ntests=21\nbuild_s=9.5")))
    (is (= {"value" 0} (cli/measure-values "Ran 21 tests containing 197 assertions. 0 failures")))
    (is (nil? (cli/measure-values "nothing here"))))
  (testing "numbers with no digit before or after the point, as bc prints them"
    (is (= {"secs" 0.915} (cli/measure-values "secs=.915 915454175")))
    (is (= {"a" -0.5 "b" 5.0} (cli/measure-values "a=-.5 b=5.")))
    (is (= {"value" 0.25} (cli/measure-values "took .25")))
    (is (= {"value" -0.5} (cli/measure-values "delta -.5")))
    (is (= {"value" 5.0} (cli/measure-values "done in 5."))))
  (let [s (new-session "goal")
        id (cli/item-add s ["bc" "--gate" "false"])]
    (cli/item-measure s [id "echo secs=.915 915454175"])
    (testing "a bc-style row logs secs, not the trailing number as value"
      (is (re-matches #"m1 i1 secs=0\.915 · \d+\.\ds" (cli/measure s [id])))
      (is (= {"secs" 0.915}
             (:values (get-in (status/project (store/read-events s)) [:items id :last-measure]))))))
  (let [s (new-session "goal")
        id (cli/item-add s ["faster" "--gate" "false"])]
    (is (thrown-with-msg? Exception #"no measurement" (cli/measure s [id])))
    (is (thrown-with-msg? Exception #"usage" (cli/item-measure s [id])))
    (cli/item-measure s [id "pwd; echo build_s=12.5 hits=3"])
    (testing "lume measure prints the row, not the output, and runs in the repo"
      (is (re-matches #"m1 i1 build_s=12\.5 hits=3 · \d+\.\ds" (cli/measure s [id])))
      (let [{:keys [measure last-measure]} (get-in (status/project (store/read-events s)) [:items id])]
        (is (= "pwd; echo build_s=12.5 hits=3" measure))
        (is (= {"build_s" 12.5 "hits" 3} (:values last-measure)))
        (is (str/includes? (slurp (:out last-measure)) (str (fs/file-name *root*))))))
    (testing "a spawn measures what its worker left, before the gate"
      (let [out (cli/spawn s *root* [id "--agent" "test"])]
        (is (re-find #"\nm2 i1 build_s=12\.5 hits=3 · \d+\.\ds · vs m1\ni1 left open to decide · g1 fail exit 1" out) out)))
    (testing "a failing measurement is still a row, with where its output is"
      (cli/item-measure s [id "echo oops; exit 2"])
      (let [out (cli/measure s [id])]
        (is (str/starts-with? out "m3 i1 no numbers · "))
        (is (re-find #"fail exit 2\n.*measures/m3\.out$" out))))
    (testing "status shows the latest row on the item; item show has them all"
      (let [st (status/render "sess" (store/read-events s) (Instant/now))]
        (is (re-find #"(?m)^            gate false .*\n            measure echo oops; exit 2 · m3 exit 2 · no numbers · " st) st))
      (let [shown (cli/item-show s [id])]
        (is (str/includes? shown "measure echo oops; exit 2"))
        (is (re-find #"measure/ran m1 build_s=12\.5 hits=3 · " shown))
        (is (re-find #"measure/ran m3 no numbers · \d+\.\ds · fail exit 2" shown))))
    (testing "a long command with many numbers stays within width"
      (cli/item-measure s [id (str (apply str (repeat 30 "echo x; ")) (str/join " " (map #(str "echo n" % "=" %) (range 30))) "; exit 127")])
      (cli/measure s [id])
      (let [line (some #(when (str/includes? % "measure ") %)
                       (str/split-lines (status/render "sess" (store/read-events s) (Instant/now))))]
        (is (re-find #"m4 exit 127 · n0=0 n1=1 n10=10 .*…" line) line)
        (is (<= (count line) 110) line)))
    (testing "a row shows its change against the latest passing row of the same command"
      (let [f (str (fs/create-temp-file))
            run! #(do (spit f %) (cli/measure s [id]))]
        (cli/item-measure s [id (str "sh " f)])
        (is (re-matches #"m5 i1 build_s=12\.5 hits=3 · \d+\.\ds" (run! "echo build_s=12.5 hits=3")) "a new command has no baseline")
        (is (re-matches #"m6 i1 build_s=9\.6\(-2\.9\) hits=3 new=1 · \d+\.\ds · vs m5" (run! "echo build_s=9.6 hits=3 new=1")))
        (is (re-find #"^m7 i1 build_s=10\(\+0\.4\) hits=3 · \d+\.\ds · fail exit 1 · vs m6\n" (run! "echo build_s=10 hits=3; exit 1"))
            "a failed row is compared, but is not a baseline")
        (is (re-matches #"m8 i1 build_s=9\.5\(-0\.1\) hits=3 · \d+\.\ds · vs m6" (run! "echo build_s=9.5 hits=3")))
        (let [line (some #(when (str/includes? % "measure ") %)
                         (str/split-lines (status/render "sess" (store/read-events s) (Instant/now))))]
          (is (re-find #"m8 vs m6 build_s=9\.5\(-0\.1\) hits=3 · \d+\.\ds · " line) line))
        (testing "a row is compared with the latest that shares a number's name"
          (is (re-matches #"m9 i1 value=915454175 · \d+\.\ds" (run! "echo value=915454175")) "a misparse has no baseline")
          (is (re-matches #"m10 i1 build_s=9\.12\(-0\.377\) hits=3 · \d+\.\ds · vs m8"
                          (run! "echo build_s=9.1234567 hits=3"))
              "skips the misparse; numbers round to 3 significant digits"))))
    (testing "spreads round too, but never a digit before the point"
      (is (= "n=1235 secs=0.764[0.699..0.773] · 0.0s"
             (status/measure-text {:values {"secs" 0.7635452455 "n" 1234.567} :spread {"secs" [0.699478972 0.772594053]}
                                   :ms 0 :exit 0}))))
    (testing "a change is marked ~ within noise, ! outside it, only when both rows have spreads"
      (let [base {:measure/id "m1" :values {"a" 41.2 "b" 10 "c" 5} :spread {"a" [40.1 42.0] "b" [9 11]}}]
        (is (= "a=37.9(-3.3 !)[37..38.5] b=10.5(+0.5 ~)[10..11] c=4(-1) · 0.0s · vs m1"
               (status/measure-text {:values {"a" 37.9 "b" 10.5 "c" 4} :spread {"a" [37.0 38.5] "b" [10 11] "c" [4 4]}
                                     :ms 0 :exit 0 :base base})))
        (is (= "a=37.9(-3.3) · 0.0s · vs m1"
               (status/measure-text {:values {"a" 37.9} :ms 0 :exit 0 :base base}))
            "a single-run row gets no mark")))
    (testing "lume measure --help prints usage"
      (is (= "usage: lume measure <id>" (cli/run ["measure" "--help"]))))
    (testing "a closed item can still be measured"
      (cli/item-close s [id "dropped" "x"])
      (is (str/starts-with? (cli/measure s [id]) "m11 i1 ")))
    (testing "lume ledger: every row, closed items' too, each against its base, newest last"
      (let [other (cli/item-add s ["other" "--gate" "true"])]
        (is (= "i2 has no measurement rows" (cli/ledger *root* [other])))
        (cli/item-measure s [other "echo q=4"])
        (cli/measure s [other])
        (let [lines (str/split-lines (cli/ledger *root* []))]
          (is (= 12 (count lines)))
          (is (re-matches #"m1 i1 \d{4}-\d\d-\d\dT\d\d:\d\d build_s=12\.5 hits=3 · \d+\.\ds · pwd; echo build_s=12\.5 hits=3" (first lines)))
          (is (re-matches #"m2 i1 \S+ build_s=12\.5 hits=3 · \d+\.\ds · vs m1" (nth lines 1)) "the same spec isn't repeated")
          (is (re-find #"^m3 i1 \S+ no numbers · \d+\.\ds · fail exit 2 · echo oops; exit 2$" (nth lines 2)))
          (is (re-find #"^m8 i1 \S+ build_s=9\.5\(-0\.1\) hits=3 · \d+\.\ds · vs m6$" (nth lines 7)))
          (is (re-matches #"m12 i2 \S+ q=4 · \d+\.\ds · echo q=4" (last lines))))
        (let [lines (str/split-lines (cli/ledger *root* [id "-n" "3"]))]
          (is (= "+8 earlier rows — lume ledger i1 -n 11" (first lines)))
          (is (str/starts-with? (second lines) "m9 i1 ") "a bounded view names the spec on its first row")
          (is (str/includes? (second lines) (str " · sh ")))
          (is (str/starts-with? (last lines) "m11 i1 ")))
        (testing "a past session's, by id"
          (let [old-id (store/session-id s)]
            (new-session "next")
            (is (= "no measurement rows" (cli/ledger *root* [])))
            (is (= (str "+11 earlier rows — lume ledger --session " old-id " -n 12")
                   (first (str/split-lines (cli/ledger *root* ["--session" old-id "-n" "1"])))))))
        (is (thrown-with-msg? Exception #"no such item" (cli/ledger *root* ["i9"])))
        (doseq [args [["-n" "x"] ["i1" "i2"] ["-n"] ["--session" "a" "--session" "b"]]]
          (is (thrown-with-msg? Exception #"usage" (cli/ledger *root* args)) (pr-str args)))))))

(deftest measurements-repeat
  (testing "a row of runs is each number's median and its spread"
    (is (= {:ms 20 :values {"a" 2 "b" 0.15} :spread {"a" [1 3] "b" [0.1 0.2]}}
           (cli/summarize-runs [{:ms 10 :output "a=1 b=0.1"} {:ms 30 :output "a=3 b=0.2"}
                                {:ms 20 :output "a=2"}]))))
  (let [s (new-session "goal")
        id (cli/item-add s ["faster" "--gate" "true"])
        f (str (fs/create-temp-file))
        n (str (fs/create-temp-file))
        ;; Each run reads and bumps a counter, so runs differ: the first is a slow warm-up.
        script! #(spit f (str "i=$(cat " n "); echo $((i+1)) > " n "; " %))
        rows #(mapv (fn [_] (cli/measure s [id])) (range %))]
    (script! "if [ $i = 0 ]; then echo t=100; else echo t=$((10 + i % 3)) k=7; fi")
    (spit n "0")
    (doseq [args [["--runs" "0"] ["--runs" "2" "--drop" "2"] ["--runs" "x"] ["--drop" "-1"] ["--runs" "2" "--runs" "3"]]]
      (is (thrown-with-msg? Exception #"usage" (cli/item-measure s (into [id "sh f"] args))) (pr-str args)))
    (is (= "i1 measure sh x ×5 drop 1" (cli/item-measure s [id "sh x" "--runs" "5" "--drop" "1"])))
    (cli/item-measure s [id (str "sh " f) "--runs" "5" "--drop" "1"])
    (testing "runs N times, drops the first K, and the warm-up never counts"
      (is (re-matches #"m1 i1 k=7 t=11\[10\.\.12\] · \d+\.\ds · 4 of 5 runs" (first (rows 1))))
      (is (= "5\n" (slurp n)))
      (let [out (slurp (:out (last (store/read-events s))))]
        (is (= 5 (count (re-seq #"(?m)^--- run \d/5 · exit 0 · " out))) out)
        (is (str/includes? out "t=100"))))
    (testing "the same spec compares with its last passing row"
      (is (re-matches #"m2 i1 k=7 t=10\.5\(-0\.5 ~\)\[10\.\.12\] · \d+\.\ds · 4 of 5 runs · vs m1" (first (rows 1)))))
    (testing "changing runs or drop is a new spec, so a new baseline"
      (cli/item-measure s [id (str "sh " f) "--runs" "3"])
      (is (re-matches #"m3 i1 k=7 t=11\[10\.\.12\] · \d+\.\ds · 3 runs" (first (rows 1))))
      (cli/item-measure s [id (str "sh " f)])
      (is (re-matches #"m4 i1 k=7 t=1[012] · \d+\.\ds" (first (rows 1))) "one run: no spread, no runs")
      (cli/item-measure s [id (str "sh " f) "--runs" "3"])
      (is (str/ends-with? (first (rows 1)) " · 3 runs · vs m3")))
    (testing "a failing run ends the measurement"
      (spit n "0")
      (script! "echo t=$i; [ $i -lt 1 ]")
      (cli/item-measure s [id (str "sh " f) "--runs" "4"])
      (let [out (first (rows 1))]
        (is (re-find #"^m6 i1 t=0\.5\[0\.\.1\] · \d+\.\ds · fail exit 1 in run 2 of 4\n.*m6\.out$" out) out))
      (is (= "2\n" (slurp n))))
    (testing "status shows the spec and stays within width; item show has the environment"
      (script! "echo t=1")
      (cli/item-measure s [id (str "sh " f " " (apply str (repeat 20 "x"))) "--runs" "10" "--drop" "1"])
      (rows 1)
      (let [line (some #(when (str/includes? % "measure ") %)
                       (str/split-lines (status/render "sess" (store/read-events s) (Instant/now))))]
        (is (re-find #"measure sh .*… ×10 drop 1 · m7 t=1 · " line) line)
        (is (<= (count line) 110) line))
      (let [shown (cli/item-show s [id])
            {:keys [env]} (last (store/read-events s))]
        (is (string? (:host env)))
        (is (number? (:load env)))
        (is (re-find #"(?m)^measure sh .* ×10 drop 1$" shown) shown)
        (is (re-find #"item/measuring sh .* ×10 drop 1 \[human\]" shown) shown)
        (is (re-find #"measure/ran m7 t=1 · \d+\.\ds · 9 of 10 runs · on \S+ load [\d.]+ \[human\]" shown) shown)))
    (testing "in a repo, the row records the commit it measured"
      (p/shell {:dir *root* :out :string :err :string} "git" "init" "-q")
      (p/shell {:dir *root* :out :string :err :string}
               "git" "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-q" "--allow-empty" "-m" "x")
      (let [head (str/trim (:out (p/shell {:dir *root* :out :string} "git" "rev-parse" "--short" "HEAD")))]
        (rows 1)
        (is (= head (:commit (:env (last (store/read-events s))))))
        (is (str/includes? (cli/item-show s [id]) (str " at " head " [human]")))))))

(deftest item-show-prints-what-status-clips
  (let [s (new-session "goal")
        intent (str "a long intent " (str/join " " (repeat 40 "word")) " END")
        id (cli/item-add s [intent "--gate" "false"])
        other (cli/item-add s ["unrelated"])]
    (cli/note s ["about i1: it matters"])
    (cli/note s ["about i11 and i10, not this one"])
    (cli/spawn s *root* [id "--agent" "test"])
    (cli/spawn s *root* [other "--agent" "test"])
    (let [out (cli/item-show s [id])
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
      (cli/item-close s [other "dropped" "not needed"])
      (is (str/starts-with? (cli/item-show s [other]) "i2 dropped — not needed")))
    (testing "events are capped to the last 20"
      (dotimes [n 25] (cli/note s [(str "i2 note " n)]))
      (let [lines (str/split-lines (cli/item-show s [other]))]
        (is (= 24 (count lines)))
        (is (re-find #"^\+\d+ earlier events" (nth lines 3)))
        (is (str/includes? (last lines) "i2 note 24"))))
    (testing "unknown items are refused"
      (is (thrown-with-msg? Exception #"no such item" (cli/item-show s ["i9"]))))))

(defmethod agents/command :allowed[_ _prompt {:keys [allow]}]
  ["echo" (str/join "|" allow)])

(deftest workers-may-run-allowed-commands-and-their-gate
  (let [s (new-session "goal")
        id (cli/item-add s ["verify" "--gate" "true"])]
    (cli/allow s ["jj"])
    (cli/allow s ["bb test"])
    (cli/allow s ["jj"])
    (is (str/includes? (cli/spawn s *root* [id "--agent" "allowed"]) "· jj|bb test|true\n"))
    (is (str/includes? (status/render "sess" (store/read-events s) (Instant/now)) "allow   jj · bb test"))
    (testing "revoking removes a command; only the human changes permissions"
      (cli/allow s ["--revoke" "jj"])
      (is (= ["bb test"] (:allow (status/project (store/read-events s)))))
      (doseq [who ["s1" "d1"]]
        (binding [cli/*actor* who]
          (is (thrown-with-msg? Exception #"only the human changes permissions" (cli/allow s ["rm"])))
          (is (thrown-with-msg? Exception #"only the human changes permissions" (cli/allow s ["--revoke" "bb test"]))))))))

(deftest only-the-exact-human-holds-policy
  (let [s (new-session "goal")]
    (cli/ask s ["ok?"])
    (doseq [who ["claude" "s1" "d1" "Human" "human " ""]]
      (binding [cli/*actor* who]
        (is (thrown-with-msg? Exception #"only the human changes permissions" (cli/allow s ["rm"])) who)
        (is (thrown-with-msg? Exception #"only the human answers" (cli/answer s ["a1" "yes"])) who)))
    (is (empty? (:allow (status/project (store/read-events s)))))
    (is (= "a1 answered" (cli/answer s ["a1" "yes"])))))

(deftest human-only-needs-a-terminal-and-a-confirmation
  (let [s (new-session "goal")
        confirms (atom [])
        typed #(fn [prompt expected] (swap! confirms conj [prompt expected]) (= % expected))]
    (cli/ask s ["ok?"])
    (testing "a wrong confirmation is refused"
      (binding [cli/*confirm* (typed "a2")]
        (is (thrown-with-msg? Exception #"need an interactive terminal and a typed confirmation"
                              (cli/answer s ["a1" "yes"])))
        (is (thrown-with-msg? Exception #"typed confirmation" (cli/allow s ["rm"])))
        (is (thrown-with-msg? Exception #"typed confirmation" (cli/init *root* ["g" "--allow" "rm"])))))
    (is (= [["Answer a1: yes" "a1"] ["Workers may run: rm." "yes"]
            ["Workers will be allowed to run: rm." "yes"]] @confirms)
        "answer asks for the ask id, permissions for yes")
    (is (= :open (get-in (status/project (store/read-events s)) [:asks "a1" :state])))
    (is (= s (store/current-session *root*)) "a refused init starts no session")
    (testing "LUME_ACTOR is still checked first, before any prompt"
      (reset! confirms [])
      (binding [cli/*actor* "s1" cli/*confirm* (typed "a1")]
        (is (thrown-with-msg? Exception #"only the human answers" (cli/answer s ["a1" "yes"]))))
      (is (empty? @confirms)))
    (testing "the human at a terminal who types it passes"
      (binding [cli/*confirm* (typed "a1")]
        (is (= "a1 answered" (cli/answer s ["a1" "yes"]))))
      (binding [cli/*confirm* (typed "yes")]
        (is (= "workers may run rm" (cli/allow s ["rm"])))))
    (testing "with no tty (as in spawns and drives) the real check refuses, naming the rule"
      (cli/ask s ["again?"])
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
      (dotimes [n 20] (cli/allow s [(str "cmd-" n " " (apply str (repeat 20 "a")))]))
      (dotimes [n 200] (cli/item-add s [(str "item " n " " (apply str (repeat 50 "x"))) "--gate" "true"]))
      (dotimes [n 20] (cli/item-close s [(str "i" (inc n)) "dropped" (apply str (repeat 60 "w"))]))
      (dotimes [n 50] (cli/note s [(str "note " n)]))
      (dotimes [n 20] (cli/note s ["--pin" (str "lesson " n " " (apply str (repeat 120 "y")))]))
      (dotimes [n 10] (cli/ask s [(str "question " n " " (apply str (repeat 120 "q")))]))
      (cli/answer s ["a1" (apply str (repeat 120 "z"))])
      (cli/answer s ["a2" "yes"])
      (cli/budget s ["--usd" "1000" "--spawns" "2000" "--minutes" "60000"])
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
      (dotimes [n 50] (cli/note s [(str "note " n)]))
      (is (str/includes? (status/render "sess" (store/read-events s) (Instant/now)) "+45 more")))))

(deftest status-colour-is-for-humans-only
  (let [s (new-session "goal")
        t (str (Instant/now))]
    (cli/ask s ["which db?"])
    (cli/item-add s ["passes" "--gate" "true"])
    (cli/item-add s ["fails" "--gate" "false"])
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
        (is (cli/color? false true nil))
        (is (not (cli/color? false true "1")))
        (is (not (cli/color? false false nil)))
        (is (cli/color? true false "1"))
        (is (str/includes? (cli/show-status *root* ["--color"]) esc))
        (is (thrown-with-msg? Exception #"usage: lume status" (cli/show-status *root* ["--colour"])))))))

(defmethod agents/command :prompt [_ prompt _opts]
  ["printf" "%s" prompt])

(deftest asks-wait-for-the-human
  (let [s (new-session "goal")
        id (cli/item-add s ["use the answer"])]
    (binding [cli/*actor* "s1"]
      (is (= "a1" (cli/ask s ["ship on fridays?"]))))
    (is (= "a2" (cli/ask s ["which db?"])))
    (let [out (status/render "sess" (store/read-events s) (Instant/now))]
      (is (re-find #"(?m)^goal    goal\nasks    a1 ship on fridays\?\n        a2 which db\?$" out)
          "open asks come right after the goal"))
    (testing "only the human answers"
      (doseq [who ["s1" "d1"]]
        (binding [cli/*actor* who]
          (is (thrown-with-msg? Exception #"only the human answers" (cli/answer s ["a1" "sure"]))))))
    (testing "answers move from asks to notes"
      (is (= "a1 answered" (cli/answer s ["a1" "never"])))
      (is (thrown-with-msg? Exception #"a1 is already answered" (cli/answer s ["a1" "ok fine"])))
      (is (thrown-with-msg? Exception #"no such ask: a9" (cli/answer s ["a9" "x"])))
      (let [out (status/render "sess" (store/read-events s) (Instant/now))]
        (is (re-find #"(?m)^asks    a2 which db\?\nopen " out) "a1 has left asks")
        (is (re-find #"(?m)^notes   \[\d\d:\d\d human\] a1 → never$" out))))
    (testing "workers see answered asks with the notes"
      (cli/spawn s *root* [id "--agent" "prompt"])
      (let [prompt (slurp (get-in (status/project (store/read-events s)) [:spawns "s1" :out]))]
        (is (str/includes? prompt "- a1 ship on fridays? → never\n"))
        (is (not (str/includes? prompt "which db")))))))

(deftest replay-renders-from-the-log-alone
  (let [s (new-session "replay me")
        sid (store/session-id s)
        _ (cli/item-add s ["first" "--gate" "true"])
        _ (store/append! s (constantly {:type :drive/turn :drive/id "d1" :n 1 :by "human"}))
        _ (store/append! s (constantly {:type :drive/exited :drive/id "d1" :exit 0 :result "spawn i1 next"}))
        _ (cli/spawn s *root* ["i1" "--agent" "test"])
        _ (cli/note s ["after the spawn"])
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
        (let [replay #(status/strip-color (cli/replay *root* (cons sid %)))]
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
              (binding [cli/*read-line* (fn [screen prompt] (swap! shown conj [screen prompt]) "")]
                (is (= "end of replay: 2 decisions" (replay ["--step"]))))
              (is (= [(str "decision 1 of 2 · event " d1 " of " m) (str "decision 2 of 2 · event " s1 " of " m)]
                     (map #(re-find #"decision \d of \d · event \d+ of \d+" (second %)) @shown)))
              (is (str/includes? (status/strip-color (first (second @shown))) "decided s1 exit 3")))
            (binding [cli/*read-line* (constantly "q")]
              (is (= (str "stopped at event " d1 " — lume replay --at " d1) (replay ["--step"]))))
            (binding [cli/*read-line* (constantly "")]
              (is (= "end of replay: 1 decision" (replay ["--step" "--at" (str (inc d1))])) "--at sets where stepping starts"))
            (is (= (replay []) (replay ["--step"])) "no terminal: the list instead"))
          (testing "out of range or stray args are refused"
            (is (thrown-with-msg? Exception #"outside the log: events 1\.\." (replay ["--at" "0"])))
            (is (thrown-with-msg? Exception #"outside the log" (replay ["--at" (str (inc m))])))
            (is (thrown-with-msg? Exception #"usage: lume replay" (replay ["--at" "x"])))
            (is (thrown-with-msg? Exception #"usage: lume replay" (replay ["extra"])))
            (is (thrown-with-msg? Exception #"no session nope" (cli/replay *root* ["nope"]))))))
      (finally (fs/delete-tree fresh)))
    (is (= events (store/read-events s)) "replay writes nothing")))

(defn- nrepl-eval
  "Evaluate code in ns over nREPL at port: the value, or the error's message."
  [port ns code]
  (require 'bencode.core)
  (let [write (resolve 'bencode.core/write-bencode)
        read (resolve 'bencode.core/read-bencode)
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
        _ (cli/item-add s ["first" "--gate" "true"])
        _ (cli/note s ["a note"])
        events (store/read-events s)
        files #(set (map str (fs/list-dir s)))
        before (files)
        ;; A namespace of its own: tests run in parallel, and user is shared.
        ns (symbol (str "lume.repl-test-" (System/nanoTime)))
        {:keys [server port text]} (cli/start-repl s {:ns ns})
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
        (cli/note s ["later"])
        (is (= (str (count events)) (ev "(count events)")))
        (is (= (str (inc (count events))) (ev "(refresh!)")))
        (is (= "\"later\"" (ev "(:text (last events))"))))
      (finally ((requiring-resolve 'babashka.nrepl.server/stop-server!) server)
               (remove-ns ns)))
    (is (= before (files)) "no files added to the session")
    (is (= events (pop (store/read-events s))) "the repl logged repl/opened and nothing else; then the test's note")
    (is (thrown-with-msg? Exception #"usage: lume repl" (cli/repl *root* ["a" "b"])))
    (is (thrown-with-msg? Exception #"no session nope" (cli/repl *root* ["nope"])))))

(deftest log-queries-filter-then-bound
  (let [s (new-session "goal")
        root *root*
        i1 (cli/item-add s ["first"])
        i2 (cli/item-add s ["second"])
        run #(->> (str/split-lines (cli/show-log root %))
                  (remove (fn [l] (or (str/blank? l) (str/starts-with? l ";;"))))
                  (mapv edn/read-string))]
    (store/append! s (constantly {:type :spawn/started :spawn/id "s1" :item/id i1 :by "human"}))
    (store/append! s (constantly {:type :spawn/progress :spawn/id "s1" :text "editing"}))
    (binding [cli/*actor* "s1"]
      (cli/note s ["about i1: done soon"])
      (cli/note s ["unrelated (i10)"]))
    (cli/note s ["see i2"])
    (testing "--type takes a prefix of ns/name"
      (is (= [:note/added :note/added :note/added] (map :type (run ["--type" "note"]))))
      (is (= [:spawn/started :spawn/progress] (map :type (run ["--type" "spawn/"]))))
      (is (= [:item/added :item/added] (map :type (run ["--type" "item/added"])))))
    (testing "--item: its events, its spawns' (progress too), notes naming it"
      (is (= [:item/added :spawn/started :spawn/progress :note/added]
             (map :type (run ["--item" i1]))))
      (is (= ["see i2"] (keep :text (run ["--item" i2]))))
      (is (thrown-with-msg? Exception #"no such item: i9" (cli/show-log root ["--item" "i9"]))))
    (testing "--by, --grep, and filters combine with -n"
      (is (= 2 (count (run ["--by" "s1"]))))
      (is (= ["unrelated (i10)"] (keep :text (run ["--by" "s1" "--grep" "i1\\d"]))))
      (is (= ["see i2"] (keep :text (run ["--type" "note" "-n" "1"]))))
      (is (= ";; +2 earlier events — -n 3"
             (first (str/split-lines (cli/show-log root ["--type" "note" "-n" "1"])))))
      (is (= "" (cli/show-log root ["--by" "nobody"]))))
    (testing "bad queries are refused"
      (is (thrown-with-msg? Exception #"bad --grep regex" (cli/show-log root ["--grep" "("])))
      (is (thrown-with-msg? Exception #"usage: lume log" (cli/show-log root ["--by" "a" "--by" "b"])))
      (is (thrown-with-msg? Exception #"usage: lume log" (cli/show-log root ["--type"]))))))

(deftest note-show-reads-a-note-in-full
  (let [s (new-session "goal")
        long-text (str "start " (str/join " " (repeat 80 "word")) " the end")]
    (binding [cli/*actor* "s1"] (cli/note s [long-text]))
    (cli/note s ["--pin" "lasting"])
    (cli/note s ["--pin" "was lasting"])
    (cli/note s ["--unpin" "n3"])
    (let [out (cli/note-show s ["n1"])]
      (is (re-find #"^n1\nnoted    \d{4}-\d\d-\d\dT\d\d:\d\d by s1\n" out))
      (is (str/ends-with? out (str "\n" long-text))))
    (is (str/starts-with? (cli/note-show s ["n2"]) "n2 pinned\n"))
    (is (re-find #"^n3 unpinned\nnoted .*\nunpinned \S+ by human\nwas lasting$" (cli/note-show s ["n3"])))
    (binding [store/*session* s]
      (is (str/starts-with? (cli/run ["note" "show" "n2"]) "n2 pinned"))
      (is (= "noted n4" (cli/run ["note" "show"])) "a note that just says show is still a note"))
    (is (thrown-with-msg? Exception #"no such note: n9" (cli/note-show s ["n9"])))
    (is (thrown-with-msg? Exception #"usage: lume note show <id>" (cli/note-show s [])))))

(deftest ask-show-reads-an-ask-in-full
  (let [s (new-session "goal")
        id (cli/item-add s ["pick a db"])
        long-q (str "which db? " (str/join " " (repeat 60 "word")) " the end")]
    (store/append! s (constantly {:type :spawn/started :spawn/id "s1" :item/id id}))
    (binding [cli/*actor* "s1"] (cli/ask s [long-q]))
    (cli/ask s ["short?"])
    (testing "status wraps a long question to 3 lines, clips the rest and points at ask show"
      (let [out (status/render "sess" (store/read-events s) (Instant/now))]
        (is (re-find #"(?m)^asks    a1 which db\? (word ?)+\n {11}(word ?)+\n {11}(word ?)+… — lume ask show a1$" out) out)
        (is (re-find #"(?m)^        a2 short\?$" out) "a question that fits has no hint")))
    (testing "lume ask with no question: open asks in full, each with its answer command"
      (let [out (cli/ask s [])]
        (is (str/includes? (str/replace out #"\s+" " ") long-q) "the whole question, unclipped")
        (is (re-find #"(?m)^a1 · asked \S+ by s1 · item i1$" out))
        (is (re-find #"(?m)^  → lume answer a1 " out))
        (is (re-find #"(?m)^a2 · asked \S+ by human$" out))
        (is (every? #(<= (count %) 100) (str/split-lines out)) "wrapped to the terminal (100 without one)"))
      (is (thrown-with-msg? Exception #"usage: lume ask" (cli/ask s ["a" "b"]))))
    (testing "the question in full, who asked and when, the asking worker's item"
      (let [out (cli/ask-show s ["a1"])]
        (is (str/includes? out (str "question " long-q)))
        (is (re-find #"(?m)^a1 open\n" out))
        (is (re-find #"(?m)^asked    \d{4}-\d\d-\d\dT\d\d:\d\d by s1$" out))
        (is (re-find #"(?m)^item     i1$" out))
        (is (str/includes? out "answer   none yet; only the human answers: lume answer a1"))))
    (testing "the answer once there is one; a human's ask concerns no item"
      (cli/answer s ["a2" "yes"])
      (let [out (cli/ask-show s ["a2"])]
        (is (re-find #"(?m)^a2 answered\n" out))
        (is (not (str/includes? out "item")))
        (is (re-find #"(?m)^answer   yes\nanswered \S+ by human$" out))))
    (testing "routing and errors"
      (binding [store/*session* s]
        (is (str/starts-with? (cli/run ["ask" "show" "a2"]) "a2 answered")))
      (is (thrown-with-msg? Exception #"no such ask: a9" (cli/ask-show s ["a9"])))
      (is (thrown-with-msg? Exception #"usage: lume ask show <id>" (cli/ask-show s []))))
    (testing "no open asks"
      (cli/answer s ["a1" "postgres"])
      (is (= "no open asks" (cli/ask s []))))))

(deftest answer-prompts-at-a-terminal
  (let [s (new-session "goal")
        shown (atom nil)]
    (cli/ask s [(str "which db? " (str/join " " (repeat 40 "word")) " END")])
    (testing "no text and no terminal: refused, and the ask stays open"
      (is (thrown-with-msg? Exception #"need an interactive terminal" (cli/answer s ["a1"])))
      (is (= :open (get-in (status/project (store/read-events s)) [:asks "a1" :state]))))
    (testing "an agent is refused before any prompt"
      (binding [cli/*actor* "d1" cli/*read-line* (fn [& _] (throw (ex-info "prompted" {})))]
        (is (thrown-with-msg? Exception #"only the human answers" (cli/answer s ["a1"])))))
    (testing "an unknown or answered ask is refused before any prompt"
      (binding [cli/*read-line* (fn [& _] (throw (ex-info "prompted" {})))]
        (is (thrown-with-msg? Exception #"no such ask: a9" (cli/answer s ["a9"])))))
    (testing "at a terminal: the full question, then the typed answer, then the typed id"
      (binding [cli/*read-line* (fn [q prompt] (reset! shown [q prompt]) "  postgres ")
                cli/*confirm* (fn [prompt expected] (and (= "Answer a1: postgres" prompt) (= "a1" expected)))]
        (is (= "a1 answered" (cli/answer s ["a1"]))))
      (is (str/includes? (str/replace (first @shown) #"\s+" " ") "which db? word"))
      (is (str/includes? (first @shown) "END") "unclipped")
      (is (= "postgres" (get-in (status/project (store/read-events s)) [:notes 0 :answer])))
      (binding [cli/*read-line* (fn [& _] (throw (ex-info "prompted" {})))]
        (is (thrown-with-msg? Exception #"a1 is already answered" (cli/answer s ["a1"])))))
    (testing "an empty typed answer is refused"
      (cli/ask s ["again?"])
      (binding [cli/*read-line* (constantly "")]
        (is (thrown-with-msg? Exception #"usage: lume answer" (cli/answer s ["a2"])))))))

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
        id (cli/item-add s ["do the thing"])]
    (cli/spawn s *root* [id "--agent" "clobbered"])
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
      (let [id (cli/item-add s ["no-op" "--gate" "true"])]
        (is (re-find #"\ni1 left open: s1 changed nothing in the repo · noted n1$" (cli/spawn s *root* [id "--agent" "test"])))
        (is (= :open (state id)))
        (is (= "i1 left open, not gated: s1 changed nothing in the repo"
               (:text (last (filter #(= :note/added (:type %)) (store/read-events s))))))))
    (testing "a spawn that changes a file is gated as before"
      (let [id (cli/item-add s ["write" "--gate" "true"])]
        (is (str/ends-with? (cli/spawn s *root* [id "--agent" "writes"]) "i2 done · g1 pass"))
        (is (= :done (state id)))))
    (testing "a spawn that asks leaves the item open, though it changed files"
      (let [id (cli/item-add s ["ask" "--gate" "true"])]
        (is (str/includes? (cli/spawn s *root* [id "--agent" "asks"]) "i3 left open: s3 asked a1"))
        (is (= :open (state id)))))
    (testing "a commit is a change"
      (sh "add" "-A")
      (let [id (cli/item-add s ["commit" "--gate" "true"])]
        (is (str/ends-with? (cli/spawn s *root* [id "--agent" "commits"]) "i4 done · g2 pass"))))
    (testing "a measured item stays open after a passing gate, for the human or driver to decide"
      (let [id (cli/item-add s ["faster" "--gate" "true"])]
        (cli/item-measure s [id "echo t=2"])
        (cli/measure s [id])
        (let [out (cli/spawn s *root* [id "--agent" "writes"])]
          (is (re-find #"\nm2 i5 t=2 · \d+\.\ds · vs m1\ni5 left open to decide · g3 pass · lume item close i5 done\|dropped \"<why>\"$" out) out))
        (is (= :open (state id)))))))

;; i69: an experiment's rows compare with the code before the spawn's edits.
(deftest a-spawn-has-a-before-row
  (let [s (new-session "goal")
        id (cli/item-add s ["fewer lines" "--gate" "true"])
        item #(get-in (status/project (store/read-events s)) [:items id])]
    (spit (str (fs/path *root* "made")) "start\n")
    (is (= "i1 measure echo t=$(wc -l < made) before" (cli/item-measure s [id "echo t=$(wc -l < made)" "--before"])))
    (is (re-matches #"m1 i1 t=1 · \d+\.\ds" (cli/measure s [id])))
    (is (re-matches #"m2 i1 t=1 · \d+\.\ds · vs m1" (cli/measure s [id])) "before any spawn, the row before is the base")
    (testing "--before: a spawn measures first, then after, a pair"
      (let [out (cli/spawn s *root* [id "--agent" "writes"])]
        (is (re-find #"^m3 i1 t=1 · \d+\.\ds · vs m2 · before s1\ns1 i1 exit 0" out) out)
        (is (re-find #"\nm4 i1 t=2\(\+1\) · \d+\.\ds · vs m3\ni1 left open to decide · g1 pass" out) out))
      (is (= "s1" (:before-spawn (first (filter #(= "m3" (:measure/id %)) (store/read-events s)))))))
    (testing "every later row compares with the before row, not the row before it"
      (spit (str (fs/path *root* "made")) "more\n" :append true)
      (is (re-matches #"m5 i1 t=3\(\+2\) · \d+\.\ds · vs m3" (cli/measure s [id]))))
    (testing "a before row isn't counted towards a plateau"
      (is (= "m5" (:counted (:plateau (item)))))
      (is (= 3 (:stale (:plateau (item)))) "m2, m4 and m5 after the best, m1"))
    (testing "without --before, the base is the latest row before the spawn"
      (is (= "i1 measure echo t=$(wc -l < made)" (cli/item-measure s [id "echo t=$(wc -l < made)"])))
      (let [out (cli/spawn s *root* [id "--agent" "writes"])]
        (is (re-find #"^s2 i1 exit 0" out) out)
        (is (re-find #"\nm6 i1 t=4\(\+1\) · \d+\.\ds · vs m5\n" out) out))
      (is (re-matches #"m7 i1 t=4\(\+1\) · \d+\.\ds · vs m5" (cli/measure s [id]))))
    (testing ":before in a spec: checked, folded back, and started from"
      (is (= [[[:items 0 :before] "must be true or false, got 1"]]
             (spec/problems {:goal "g" :items [{:intent "x" :measure "m" :before 1}]})))
      (is (= [[[:items 0] ":runs, :drop, :higher and :before need a :measure"]]
             (spec/problems {:goal "g" :items [{:intent "x" :before true}]})))
      (is (= {:type :item/measuring :item/id "i1" :measure "m" :before true}
             (second (filter #(= "i1" (:item/id %)) (spec/events {:goal "g" :items [{:intent "x" :measure "m" :before true}]} "/r")))))
      (cli/item-measure s [id "echo t=1" "--before"])
      (is (= [{:intent "fewer lines" :gate "true" :measure "echo t=1" :before true}]
             (:items (spec/from-state (status/project (store/read-events s)))))))))

;; i71: dropped --revert puts the working copy back at the before row.
(def ^:private vcs-env
  {"GIT_AUTHOR_NAME" "t" "GIT_AUTHOR_EMAIL" "t@t" "GIT_COMMITTER_NAME" "t" "GIT_COMMITTER_EMAIL" "t@t"
   "JJ_USER" "t" "JJ_EMAIL" "t@t"})

(defn- vcs! [& cmd]
  (apply p/shell {:dir *root* :out :string :err :string :extra-env vcs-env} cmd))

(defn- made [] (slurp (str (fs/path *root* "made"))))

(defn- closed [s id] (last (filter #(and (= :item/closed (:type %)) (= id (:item/id %))) (store/read-events s))))

(defn- measured-spawn
  "An item measured once, then a spawn that appends to made: its before row is m1."
  [s]
  (let [id (cli/item-add s ["fewer lines" "--gate" "true"])]
    (cli/item-measure s [id "echo t=$(wc -l < made)"])
    (cli/measure s [id])
    (cli/spawn s *root* [id "--agent" "writes"])
    id))

(deftest dropped-revert-restores-the-jj-commit
  (spit (str (fs/path *root* "made")) "start\n")
  (vcs! "jj" "git" "init")
  (vcs! "jj" "commit" "-m" "start")
  (let [s (new-session "goal")]
    (testing "refused without a before row, and only with dropped"
      (let [id (cli/item-add s ["unspawned" "--gate" "true"])]
        (cli/item-measure s [id "echo t=1"])
        (cli/measure s [id])
        (is (thrown-with-msg? Exception #"i1 has no before row" (cli/item-close s [id "dropped" "--revert" "no"])))
        (is (thrown-with-msg? Exception #"--revert goes with dropped" (cli/item-close s [id "done" "--revert" "no"])))
        (is (= :open (get-in (status/project (store/read-events s)) [:items id :state])))))
    (let [id (measured-spawn s)
          c (:jj-commit (:env (first (filter #(= "m2" (:measure/id %)) (store/read-events s)))))]
      (is (= "start\nmore\n" (made)))
      (is (re-matches #"[0-9a-f]{12}" (str c)))
      (is (= (str id " dropped · restored jj " c " from m2") (cli/item-close s [id "dropped" "--revert" "slower"])))
      (is (= "start\n" (made)))
      (is (= {:disposition :dropped :why "slower" :measure/id "m2" :jj-commit c}
             (select-keys (closed s id) [:disposition :why :measure/id :jj-commit]))))))

(deftest dropped-revert-restores-the-git-commit
  (spit (str (fs/path *root* "made")) "start\n")
  (vcs! "git" "init" "-q")
  (vcs! "git" "add" "made")
  (vcs! "git" "commit" "-qm" "start")
  (let [s (new-session "goal")
        head (str/trim (:out (vcs! "git" "rev-parse" "--short" "HEAD")))]
    (testing "without --revert, dropped changes nothing"
      (let [id (measured-spawn s)]
        (is (= (str id " dropped") (cli/item-close s [id "dropped" "slower"])))
        (is (= "start\nmore\n" (made)))
        (is (nil? (:commit (closed s id))))))
    (testing "--revert restores the before row's commit, a committed change too"
      (let [id (measured-spawn s)]
        (vcs! "git" "commit" "-qam" "more")
        (is (= "start\nmore\nmore\n" (made)))
        (is (= (str id " dropped · restored git " head " from m3") (cli/item-close s [id "dropped" "--revert" "slower"])))
        (is (= "start\n" (made)))
        (is (= ["m3" head] ((juxt :measure/id :commit) (closed s id))))))
    (testing "a failed restore is refused and the item stays open"
      (let [id (measured-spawn s)]
        (fs/delete-tree (fs/path *root* ".git"))
        (vcs! "git" "init" "-q")
        (is (thrown-with-msg? Exception #"not reverted: git restore .* failed" (cli/item-close s [id "dropped" "--revert" "x"])))
        (is (= :open (get-in (status/project (store/read-events s)) [:items id :state])))))))

(deftest the-default-agent-is-lume-agent
  (let [s (new-session "goal")]
    (cli/item-add s ["do it"])
    (binding [cli/*default-agent* "test"]
      (cli/spawn s *root* ["i1"]))
    (is (= :test (:agent (first (filter #(= :spawn/started (:type %)) (store/read-events s))))))))

(defn- drive! [s & turns]
  (reset! *driver-turns* (vec turns))
  (reset! *driver-calls* [])
  (with-out-str (is (string? (cli/drive s *root* ["--agent" "driver" "--turns" "3"]))))
  (last (keep :why (store/read-events s))))

(defmethod agents/command :asker [_ _prompt _opts]
  ["sh" "-c" (lume-cmd "ask" "from the worker?")])

(deftest hand-backs-notify-the-human
  (let [got (atom [])
        status (atom 200)
        server (httpkit/run-server (fn [r]
                                     (swap! got conj [(get-in r [:headers "title"]) (slurp (:body r))])
                                     {:status @status})
                                   {:port 0 :legacy-return-value? false})
        url (str "http://localhost:" (httpkit/server-port server) "/secret-topic")
        notices (fn [s] (filter #(#{:notify/sent :notify/failed} (:type %)) (store/read-events s)))]
    (try
      (let [s (new-session "goal")
            id (cli/item-add s ["ask from a worker"])]
        (testing "unset LUME_NTFY sends and logs nothing"
          (cli/ask s ["quiet?"])
          (is (empty? @got))
          (is (empty? (notices s))))
        (binding [cli/*ntfy* url]
          (testing "an ask is sent and logged, without the URL"
            (is (= "a2" (cli/ask s ["which db?"])))
            (is (= [["lume: ask a2 from human" (str "which db?\nlume answer a2\n" (store/session-id s))]] @got))
            (is (= {:type :notify/sent :on :ask/opened :ask/id "a2" :text "which db?\nlume answer a2" :by "human"}
                   (select-keys (last (notices s)) [:type :on :ask/id :item/id :text :by])))
            (is (not (str/includes? (slurp (str (fs/path s "log.edn"))) "secret-topic"))))
          (testing "a worker's ask names its item: lume sends it, since LUME_NTFY doesn't reach the spawn"
            (cli/spawn s *root* [id "--agent" "asker"])
            (is (= "lume: ask a3 on i1 from s1" (first (last @got))))
            (is (= {:ask/id "a3" :item/id "i1" :by "human"} (select-keys (last (notices s)) [:ask/id :item/id :by]))))
          (testing "a failed send is logged and never fails the command"
            (reset! status 500)
            (is (= "a4" (cli/ask s ["still there?"])))
            (is (= {:type :notify/failed :ask/id "a4" :error "HTTP 500"}
                   (select-keys (last (notices s)) [:type :ask/id :error])))
            (binding [cli/*ntfy* "http://localhost:1/nothing-listens"]
              (is (= "a5" (cli/ask s ["anyone?"])))
              (is (= :notify/failed (:type (last (notices s)))))))))
      (testing "a drive's stop is sent"
        (reset! status 200)
        (let [s (new-session "goal")]
          (cli/item-add s ["do it" "--gate" "false"])
          (binding [cli/*ntfy* url]
            (drive! s (lume-cmd "note" "looked")))
          (is (re-find #"^lume: drive stopped$" (first (last @got))))
          (is (= {:type :notify/sent :on :drive/stopped :text "d2 logged no events"}
                 (select-keys (last (notices s)) [:type :on :text])))))
      (finally (httpkit/server-stop! server)))))

(defmethod agents/command :env-peek [_ _prompt _opts]
  ["sh" "-c" "echo \"ntfy=[$LUME_NTFY$LUME_NTFY_REPLY$LUME_NTFY_OTHER]\""])

;; i74: a stub ntfy server. POSTs to /notify are recorded; GET /reply/json
;; streams whatever replies the test queues, a poll at a time.
(deftest asks-are-answered-from-the-phone
  (let [got (atom [])
        gets (atom [])
        replies (java.util.concurrent.LinkedBlockingQueue.)
        server (httpkit/run-server
                (fn [r]
                  (if (= :post (:request-method r))
                    (do (swap! got conj {:title (get-in r [:headers "title"]) :actions (get-in r [:headers "actions"])
                                         :body (slurp (:body r))})
                        {:status 200})
                    (do (swap! gets conj (str (:uri r) "?" (:query-string r)))
                        {:status 200
                         :body (str/join "\n" (cons "{\"event\":\"open\"}"
                                                    (for [m (remove nil? [(.poll replies 300 java.util.concurrent.TimeUnit/MILLISECONDS)])]
                                                      (json/generate-string {:id (str (random-uuid)) :event "message" :message m}))))})))
                {:port 0 :legacy-return-value? false})
        base (str "http://localhost:" (httpkit/server-port server))
        reply-url (str base "/reply?auth=tok")
        sent (fn [n] (loop [i 0] (cond (>= (count @got) n) (nth @got (dec n))
                                       (< i 200) (do (Thread/sleep 100) (recur (inc i))))))
        code-of #(second (re-find #"\ncode (\w+)\n" (:body %)))
        log #(slurp (str (fs/path % "log.edn")))]
    (try
      (binding [cli/*ntfy* (str base "/notify") cli/*ntfy-reply* reply-url cli/*reconnect-ms* 50]
        (testing "a spawn's env has no LUME_NTFY*"
          (let [s (new-session "goal")]
            (cli/item-add s ["peek"])
            (cli/spawn s *root* ["i1" "--agent" "env-peek"])
            (is (str/includes? (slurp (:out (first (filter #(= :spawn/started (:type %)) (store/read-events s)))))
                               "ntfy=[]"))))
        (testing "a reply that names the ask and quotes its code answers it, and the drive carries on"
          (let [s (new-session "goal")
                _ (cli/item-add s ["do it"])
                drive (future (reset! *driver-turns* [(lume-cmd "ask" "ship it? (y/n)")])
                              (binding [*out* (java.io.StringWriter.)]
                                (cli/drive s *root* ["--agent" "driver" "--turns" "3" "--wait-on-ask"])))
                {:keys [title actions] :as n} (sent 1)
                code (code-of n)]
            (is (= "lume: ask a1 from d1" title) "sent once, by the drive, with a code")
            (is (re-matches #"[a-z2-9]{8}" code))
            (is (= (str "http, Yes, " reply-url ", method=POST, body=a1 " code " yes, clear=true; "
                        "http, No, " reply-url ", method=POST, body=a1 " code " no, clear=true")
                   actions))
            (doseq [m ["a1 wrong yes" (str "a2 " code " yes") "just yes" (str " A1 " (str/upper-case code) "  yes, ship it\n")]]
              (.put replies m))
            (is (= "drive stopped: d2 logged no events" (deref drive 30000 "timed out")))
            (is (= [{:ask/id "a1" :answer "yes, ship it" :by "human" :via :ntfy}]
                   (map #(select-keys % [:ask/id :answer :by :via]) (filter #(= :ask/answered (:type %)) (store/read-events s)))))
            (is (re-find #"^/reply/json\?auth=tok&since=\d+$" (first @gets)) "the token stays in the query")
            (testing "the code and the URLs are never logged"
              (is (not-any? #(str/includes? (log s) %) [code "tok" "/reply" "/notify"])))))
        (testing "an answer at the terminal ends the wait, and spends the code"
          (let [s (new-session "goal")
                _ (cli/item-add s ["do it"])
                before (count @got)
                drive (future (reset! *driver-turns* [(lume-cmd "ask" "which db?")])
                              (binding [*out* (java.io.StringWriter.)]
                                (cli/drive s *root* ["--agent" "driver" "--turns" "3" "--wait-on-ask"])))
                n (sent (inc before))
                code (code-of n)]
            (is (nil? (:actions n)) "free text: no buttons")
            (cli/answer s ["a1" "postgres"])
            (is (= "drive stopped: d2 logged no events" (deref drive 30000 "timed out")))
            (.put replies (str "a1 " code " sqlite"))
            (Thread/sleep 500)
            (is (= ["postgres"] (keep #(when (= :ask/answered (:type %)) (:answer %)) (store/read-events s)))))))
      (testing "--wait-on-ask needs both URLs"
        (let [s (new-session "goal")]
          (binding [cli/*ntfy* "http://localhost:1/x"]
            (is (thrown-with-msg? Exception #"needs LUME_NTFY .* and LUME_NTFY_REPLY"
                                  (cli/drive s *root* ["--wait-on-ask"]))))))
      (finally (httpkit/server-stop! server)))))

(deftest a-stalled-reply-stream-reconnects
  (let [gets (atom [])
        open (atom [])
        server (httpkit/run-server
                (fn [r]
                  (swap! gets conj (:query-string r))
                  (httpkit/as-channel r {:on-open
                                         (fn [ch]
                                           (swap! open conj ch)
                                           ;; First connection: a message, then silence with the socket held open.
                                           ;; Second: the reply, then silence again.
                                           (httpkit/send! ch {:status 200
                                                              :body (str (json/generate-string
                                                                          (if (= 1 (count @gets))
                                                                            {:id "m1" :event "message" :message "first"}
                                                                            {:id "m2" :event "message" :message "second"}))
                                                                         "\n")}
                                                          false))}))
                {:port 0 :legacy-return-value? false})
        {:keys [queue stop]} (binding [cli/*idle-ms* 300 cli/*reconnect-ms* 10]
                               (#'cli/ntfy-subscribe! (str "http://localhost:" (httpkit/server-port server) "/reply?auth=tok")))
        take! #(.poll ^java.util.concurrent.LinkedBlockingQueue queue 5 java.util.concurrent.TimeUnit/SECONDS)]
    (try
      (is (= {:message "first"} (take!)))
      (is (= {:error "no data for 0s, reconnecting"} (take!)) "the silent stream is dropped")
      (is (= {:message "second"} (take!)) "and the next connection delivers")
      (is (re-find #"^auth=tok&since=\d+$" (first @gets)))
      (is (= "auth=tok&since=m1" (second @gets)) "resuming after the last message, so none is lost")
      (finally (stop)
               (run! #(httpkit/close %) @open)
               (httpkit/server-stop! server)))))

(deftest drive-runs-driver-turns
  (testing "a driver's spawn is a request, run after its turn"
    (let [s (new-session "goal")]
      (cli/item-add s ["do it" "--gate" "true"])
      ;; LUME_ACTOR=claude is what Claude Code's settings.json env does to the
      ;; driver's commands (i46): the drive's id must still win.
      (is (= "no open items"
             (drive! s (str "LUME_ACTOR=claude " (lume-cmd "spawn" "i1" "--agent" "test") "; echo \"me $LUME_ACTOR\""))))
      (let [events (store/read-events s)
            {:keys [items spawns]} (status/project events)]
        (is (= [:drive/started :drive/turn :spawn/requested :drive/exited :spawn/started]
               (take 5 (drop 2 (map :type events)))))
        (is (= "d1" (:by (first (filter #(= :spawn/requested (:type %)) events)))))
        (is (= "done by s1" (get-in spawns ["s1" :result])))
        (is (= :done (get-in items ["i1" :state])) "the spawn's gate closed it"))
      (let [[{:keys [prompt role]} closing] @*driver-calls*]
        (is (= :driver role))
        (is (str/includes? prompt "turn d1"))
        (is (str/includes? prompt "open    i1  do it") "the prompt carries status")
        (is (not (str/includes? prompt "closing")))
        (is (str/includes? (:prompt closing) "s1 exited after the last driver turn, so this is the closing")
            "the spawn closed the last item, so a closing turn reviews its diff, though it logged nothing")
        (is (= 2 (count @*driver-calls*))))))
  (testing "with no turn left for the closing turn, the stop line says the diff is uncommitted"
    (let [s (new-session "goal")]
      (cli/item-add s ["do it" "--gate" "true"])
      (reset! *driver-turns* [(lume-cmd "spawn" "i1" "--agent" "test")])
      (with-out-str (cli/drive s *root* ["--agent" "driver" "--turns" "1"]))
      (is (= "no open items, and no turn left to review s1's changes: they're uncommitted"
             (last (keep :why (store/read-events s)))))))
  (testing "a request that names no agent runs the drive's"
    (let [s (new-session "goal")]
      (cli/item-add s ["do it" "--gate" "true"])
      (drive! s (lume-cmd "spawn" "i1") "echo worked")
      (is (= :driver (:agent (first (filter #(= :spawn/started (:type %)) (store/read-events s))))))
      (is (= [:driver nil] (take 2 (map :role @*driver-calls*))) "the worker ran the drive's agent, as a worker")))
  (testing "it stops when a turn logs nothing, when turns run out, and when an ask is open"
    (let [s (new-session "goal")]
      (cli/item-add s ["never done"])
      (is (= "d1 logged no events" (drive! s "true")))
      (is (= "3 turns run" (drive! s (lume-cmd "note" "a") (lume-cmd "note" "b") (lume-cmd "note" "c"))))
      (is (= ["d2" "d3" "d4"] (map :by (:notes (status/project (store/read-events s)))))
          "driver ids keep counting across drives")
      (is (re-find #"^ask a1 is open: which\? " (drive! s (lume-cmd "ask" "which?") (lume-cmd "note" "never"))))
      (is (= 1 (count @*driver-calls*)) "no turn runs while an ask is open"))))

;; i49: a drive started before a commit ran the old code for every later turn.
(deftest drive-reloads-changed-source
  (let [src (str (fs/path *root* "lume-src"))
        _ (fs/create-dirs (fs/path src "lume"))
        cli-file (str (fs/path src "lume" "cli.clj"))
        write-cli (fn [code] (str "printf '" code "' > " cli-file))
        execs (atom [])
        drive (fn [& opts]
                (with-out-str (cli/drive (store/current-session *root*) *root*
                                         (into ["--agent" "driver" "--turns" "3"] opts))))
        s (new-session "goal")]
    (spit cli-file "(ns lume.cli)")
    (cli/item-add s ["never done"])
    (binding [cli/*source* src
              cli/*reexec* (fn [src args] (swap! execs conj [src args]) "re-exec'd")]
      (testing "when the source changes under it and loads, the drive re-execs on it from the next turn"
        (reset! *driver-turns* [(str (write-cli "(ns lume.cli) ;; new") "; " (lume-cmd "note" "a"))])
        (drive)
        (is (= [[src ["drive" "--turns" "3" "--agent" "driver" "--from" "2"]]] @execs))
        (is (= [:drive/turn :note/added :drive/exited :drive/reloaded] (map :type (take-last 4 (store/read-events s)))))
        (is (= 1 (count @*driver-calls*)) "turn 2 runs in the new process"))
      (testing "the new process carries on from that turn, without starting another drive"
        (reset! *driver-turns* ["true"])
        (drive "--from" "2")
        (let [events (store/read-events s)]
          (is (= 1 (count (filter #(= :drive/started (:type %)) events))))
          (is (= 2 (:n (last (filter #(= :drive/turn (:type %)) events)))))))
      (testing "when the new source doesn't load, the drive notes it and keeps running the old code"
        (reset! execs [])
        (reset! *driver-turns* [(str (write-cli "(ns lume.cli") "; " (lume-cmd "note" "b")) (lume-cmd "note" "c") "true"])
        (drive)
        (is (empty? @execs))
        (is (= 3 (count (filter #(= :drive/turn (:type %)) (take-while #(not= :drive/started (:type %)) (reverse (store/read-events s)))))))
        (is (some #(str/includes? % "doesn't load, so turn 2 runs the code the drive started with")
                  (map :text (:notes (status/project (store/read-events s))))))))))

;; Fake drivers summarize like claude, so a turn can report a cost on a result line.
(defmethod agents/summarize :driver [_ out] (agents/summarize :claude out))

(defn- result-line [usd]
  (str "echo '" (json/generate-string {:type "result" :result "ok" :total_cost_usd usd}) "'"))

(deftest budgets-bound-spend
  (let [s (new-session "goal")
        spent #(:spent (status/project (store/read-events s)))]
    (cli/item-add s ["work" "--gate" "false"])
    (testing "with no flags it prints spent against limits"
      (is (= "budget $0.00 · 0 spawns · 0 min" (cli/budget s []))))
    (testing "bad values are usage errors"
      (doseq [args [["--usd"] ["--usd" "x"] ["--spawns" "1.5"] ["--minutes" "-1"] ["10"] ["--usd" "1" "--usd" "2"]]]
        (is (thrown-with-msg? Exception #"usage" (cli/budget s args)) (pr-str args))))
    (testing "agents may set and lower limits, never raise them"
      (binding [cli/*actor* "d1"]
        (is (= "budget $0.00/$5.00 · 0/3 spawns · 0 min" (cli/budget s ["--usd" "5" "--spawns" "3"])))
        (cli/budget s ["--spawns" "2"])
        (is (thrown-with-msg? Exception #"only the human raises" (cli/budget s ["--spawns" "4"])))
        (is (thrown-with-msg? Exception #"only the human raises" (cli/budget s ["--usd" "1" "--spawns" "9"]))
            "a raise refuses the whole command, lowering included"))
      (is (= {:usd 5.0 :spawns 2} (:budget (status/project (store/read-events s))))))
    (testing "the human raises at a terminal, with a typed confirmation"
      (binding [cli/*confirm* (constantly false)]
        (is (thrown-with-msg? Exception #"interactive terminal" (cli/budget s ["--spawns" "4"]))))
      (cli/budget s ["--spawns" "4"])
      (is (= 4 (:spawns (:budget (status/project (store/read-events s)))))))
    (testing "spawns and drive turns are spent: count, dollars and minutes"
      (cli/spawn s *root* ["i1" "--agent" "test"])
      (store/append! s (constantly {:type :spawn/exited :spawn/id "s9" :cost-usd 1.25 :ms 90000}))
      (is (= "d2 logged no events" (drive! s (str (lume-cmd "note" "a") "; " (result-line 0.5)))))
      (let [{:keys [ms cost-usd]} (first (filter #(= :drive/exited (:type %)) (store/read-events s)))]
        (is (= 0.5 cost-usd) "a drive turn's cost comes from its result line")
        (is (nat-int? ms)))
      (is (= 1 (:spawns (spent))))
      (is (= 1.75 (:usd (spent)))))
    (testing "status shows spent/limit on one line"
      (is (re-find #"(?m)^budget  \$1\.75/\$5\.00 · 1/4 spawns · 1\.\d min$"
                   (status/render "sess" (store/read-events s) (Instant/now)))))
    (testing "past a limit, spawns are refused and drive stops with the reason"
      (cli/budget s ["--spawns" "1"])
      (cli/item-add s ["more" "--gate" "true"])
      (is (thrown-with-msg? Exception #"^budget reached: 1 of 1 spawns — only the human raises it, with lume budget --spawns N$"
                            (cli/spawn s *root* ["i2" "--agent" "test"])))
      (binding [cli/*actor* "d9"]
        (is (thrown-with-msg? Exception #"budget reached" (cli/spawn s *root* ["i2"])) "a driver's request too"))
      (is (re-find #"^budget reached: 1 of 1 spawns" (drive! s (lume-cmd "note" "never"))))
      (is (empty? @*driver-calls*) "no turn starts past a limit")
      (is (str/includes? (status/render "sess" (store/read-events s) (Instant/now)) "reached: no new spawns or turns")))
    (testing "dollars and minutes are limits too"
      (cli/budget s ["--spawns" "100" "--usd" "1.75"])
      (is (re-find #"^budget reached: \$1\.75 of \$1\.75 spent" (drive! s (lume-cmd "note" "never"))))
      (cli/budget s ["--usd" "100" "--minutes" "1"])
      (is (re-find #"^budget reached: 1\.\d of 1 minutes" (drive! s (lume-cmd "note" "never")))))))

(defn- spawn-cmds [& ids]
  (str/join "; " (map #(lume-cmd "spawn" % "--agent" "test") ids)))

;; Cadence tests: one deftest per rule, so the drive-heavy ones run in parallel.
(defn- started [s] (map :item/id (filter #(= :spawn/started (:type %)) (store/read-events s))))
(defn- handed-back [s] (first (filter #(= :drive/handed-back (:type %)) (store/read-events s))))

(defn- cadence-session [cadence & gates]
  (let [s (new-session "goal")]
    (when cadence (cli/cadence s [cadence]))
    (doseq [g gates] (cli/item-add s ["work" "--gate" g]))
    s))

(deftest cadence-rules-are-data
  (testing "rules are data: no rules prints them, bad rules are usage errors"
    (let [s (cadence-session nil)
          render #(status/render "sess" (store/read-events s) (Instant/now))
          cad #(status/cadence (status/project (store/read-events s)))]
      (is (= "cadence every-spawn · ceiling every-spawn" (cli/cadence s [])))
      (is (not (re-find #"(?m)^allow" (render))) "an unset cadence isn't shown")
      (doseq [args [["batch"] ["batch" "0"] ["on"] ["on" "plateau"] ["on" "fail"] ["sometimes"]]]
        (is (thrown-with-msg? Exception #"usage: lume cadence" (cli/cadence s args)) (pr-str args)))
      (is (= "cadence batch 3 on gate-fail on plateau 4 · ceiling batch 3 on gate-fail on plateau 4"
             (cli/cadence s ["batch 3" "on" "gate-fail" "on plateau 4"])))
      (is (= {:batch 3 :gate-fail true :plateau 4} (cad)))
      (testing "a driver may hand back more often, within the human's ceiling, never less"
        (binding [cli/*actor* "d1"]
          (cli/cadence s ["batch 2 on gate-fail on plateau 2"])
          (doseq [rules ["batch 4 on gate-fail on plateau 4" "batch 2 on plateau 2" "batch 2 on gate-fail"
                         "batch 2 on gate-fail on plateau 5"]]
            (is (thrown-with-msg? Exception #"^only the human hands back less often \(" (cli/cadence s [rules])) rules)))
        (is (= {:batch 2 :gate-fail true :plateau 2} (cad)))
        (is (re-find #"(?m)^allow   cadence batch 2 on gate-fail on plateau 2 \(ceiling batch 3 on gate-fail on plateau 4\)$"
                     (render))))
      (testing "the human raises at a terminal, and their cadence is the new ceiling"
        (binding [cli/*confirm* (constantly false)]
          (is (thrown-with-msg? Exception #"interactive terminal" (cli/cadence s ["batch 5"]))))
        (cli/cadence s ["every-spawn"])
        (binding [cli/*actor* "d1"]
          (is (thrown-with-msg? Exception #"batch 1 → 2" (cli/cadence s ["batch 2"]))))
        (is (re-find #"(?m)^allow   cadence every-spawn$" (render)))))))

(deftest cadence-every-spawn-and-batch
  (testing "every-spawn: a turn may request one spawn"
    (let [s (cadence-session nil "false" "false")]
      (is (= "d2 logged no events" (drive! s (spawn-cmds "i1" "i2"))))
      (is (= ["i1"] (started s)))))
  (testing "batch N: up to N requests a turn, run in order, failed gates and all"
    (let [s (cadence-session "batch 3" "false" "false" "false" "false")]
      (binding [cli/*actor* "d9"]
        (is (re-find #"\(1 of 3 this turn\)" (cli/spawn s *root* ["i1"]))))
      (is (= "d2 logged no events" (drive! s (spawn-cmds "i3" "i2" "i1" "i4"))))
      (is (= ["i3" "i2" "i1"] (started s)))
      (is (nil? (handed-back s))))))

(deftest cadence-on-gate-fail
  (testing "on gate-fail: a failed gate ends the batch and hands back"
    (let [s (cadence-session "batch 3 on gate-fail" "true" "false" "true")]
      (is (= "d2 logged no events" (drive! s (spawn-cmds "i1" "i2" "i3"))))
      (is (= ["i1" "i2"] (started s)) "a passing gate carries on")
      (is (= {:drive/id "d1" :why "on gate-fail: i2 gate g2 failed" :skipped ["i3"]}
             (select-keys (handed-back s) [:drive/id :why :skipped]))))))

(deftest cadence-on-plateau
  (testing "on plateau K: K rows without a new best ask the human, and drive stops"
    (let [s (cadence-session "batch 5 on plateau 2" "false")]
      (spit (str (fs/path *root* "t")) "t=5")
      (cli/item-measure s ["i1" "cat t"])
      (is (re-find #"^ask a1 is open: i1 plateaued: 2 measurement rows without a new best \(best m1, lower is better\)"
                   (drive! s (spawn-cmds "i1" "i1" "i1" "i1") (lume-cmd "note" "never"))))
      (is (= ["i1" "i1" "i1"] (started s)))
      (is (= ["i1"] (:skipped (handed-back s))))
      (is (= "an ask is open" (:why (handed-back s))))
      (is (= 1 (count @*driver-calls*)))
      (testing "one ask at a time per item, and a new best resets the count"
        (cli/measure s ["i1"])
        (is (not (str/includes? (cli/measure s ["i1"]) "ask")) "m5 is 4 rows on, but a1 is open")
        (cli/answer s ["a1" "keep going"])
        (cli/measure s ["i1"])
        (is (str/includes? (cli/measure s ["i1"]) "\nask a2 i1 plateaued: 6 measurement rows"))
        (cli/answer s ["a2" "keep going"])
        (spit (str (fs/path *root* "t")) "t=4")
        (is (re-find #"^m8 i1 t=4\(-1\)" (cli/measure s ["i1"])))
        (is (= {:stale 0 :best-id "m8"}
               (select-keys (get-in (status/project (store/read-events s)) [:items "i1" :plateau]) [:stale :best-id])))))))

(deftest cadence-plateau-counts-only-evidence
  (testing "failed rows and rows without numbers don't count toward a plateau"
    (let [s (cadence-session "batch 5 on plateau 2" "false")
          t #(spit (str (fs/path *root* "t")) %)
          row #(cli/measure s ["i1"])
          plateau #(select-keys (get-in (status/project (store/read-events s)) [:items "i1" :plateau]) [:stale :best-id])]
      (cli/item-measure s ["i1" "cat t"])
      (row)
      (is (= {} (plateau)) "a failed first row starts nothing")
      (t "secs=5") (row)
      (t "secs=6") (row)
      (is (= {:stale 1 :best-id "m2"} (plateau)))
      (fs/delete (fs/path *root* "t"))
      (is (not (str/includes? (row) "ask")) "a failed row is no evidence")
      (t "no numbers") (row)
      (is (= {:stale 1 :best-id "m2"} (plateau)))
      (testing "a row whose number names differ from the best's starts over"
        (t "value=915454175") (row)
        (is (= {:stale 0 :best-id "m6"} (plateau)))
        (t "secs=9") (row)
        (is (= {:stale 0 :best-id "m7"} (plateau)) "secs isn't compared with the misparsed value"))
      (testing "the row that makes K asks, but a failed row after it doesn't ask again"
        (row)
        (is (str/includes? (row) "\nask a1 i1 plateaued: 2 measurement rows"))
        (cli/answer s ["a1" "keep going"])
        (fs/delete (fs/path *root* "t"))
        (is (not (str/includes? (row) "ask"))))
      (testing "a closed item opens no plateau ask"
        (t "secs=9") (row)
        (cli/item-close s ["i1" "dropped" "enough"])
        (is (not (str/includes? (row) "ask")))
        (is (= {:stale 4 :best-id "m7"} (plateau)))))))

(deftest cadence-plateau-higher-is-better
  (testing "--higher: a score's new best is a higher number, and rows mark it name↑"
    (let [s (cadence-session "batch 5 on plateau 2" "false")
          row #(cli/measure s ["i1"])
          plateau #(select-keys (get-in (status/project (store/read-events s)) [:items "i1" :plateau]) [:stale :best-id])]
      (spit (str (fs/path *root* "t")) "score=5 ms=10")
      (is (= "i1 measure cat t higher score" (cli/item-measure s ["i1" "cat t" "--higher" "score"])))
      (is (re-find #"^m1 i1 ms=10 score↑=5 · " (row)))
      (spit (str (fs/path *root* "t")) "score=7 ms=10")
      (is (re-find #"^m2 i1 ms=10 score↑=7\(\+2\) · " (row)))
      (is (= {:stale 0 :best-id "m2"} (plateau)) "higher score is a new best")
      (spit (str (fs/path *root* "t")) "score=6 ms=11")
      (row)
      (is (re-find #"\nask a1 i1 plateaued: 2 measurement rows without a new best \(best m2, higher is better for score, lower for the rest\)"
                   (row)))
      (testing "a change of direction starts the count over"
        (cli/item-measure s ["i1" "cat t"])
        (row)
        (is (= {:stale 0 :best-id "m5"} (plateau)))))))

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
                    {:intent "document the cache" :gate "bb lint"}
                    {:intent "just measured" :measure "bb size" :higher ["hits"]}]}]
    (is (empty? (spec/problems sp)))
    (testing "spec -> session -> spec"
      (let [s (session-from-spec sp)
            state (status/project (store/read-events s))]
        (is (= sp (edn/read-string (cli/show-spec s []))))
        (is (= ["bb test" "bb lint" "bb test"] (map :gate (sort-by :order (vals (:items state)))))
            "items without a gate get the default")
        (is (= {:measure "bb bench" :runs 10 :drop 1} (select-keys (get-in state [:items "i1"]) [:measure :runs :drop])))
        (testing "closed items are left out unless --all"
          (cli/item-close s ["i2" "dropped" "not needed"])
          (let [out (cli/show-spec s [])]
            (is (= ["cut cold build time" "just measured"] (map :intent (:items (edn/read-string out)))))
            (is (str/ends-with? out ";; 1 closed item left out — lume spec --all")))
          (is (= sp (edn/read-string (cli/show-spec s ["--all"])))))))
    (testing "a cadence comes back as written: the default :batch 1 isn't added"
      (doseq [c [{} {:gate-fail true} {:plateau 2} {:batch 1 :plateau 2} {:batch 2 :gate-fail true}]]
        (let [sp {:goal "cadence only" :cadence c}
              s (session-from-spec sp)
              state (status/project (store/read-events s))]
          (is (= sp (edn/read-string (cli/show-spec s []))) (pr-str c))
          (is (= (merge {:batch 1} c) (status/cadence state) (status/cadence-ceiling state))
              "the default fills in on read"))))
    (testing "session -> spec -> session: a hand-built session's harness is reusable"
      (cli/init *root* ["by hand" "--gate" "bb test" "--allow" "jj"])
      (let [s (store/current-session *root*)]
        (cli/item-add s ["first"])
        (cli/item-measure s ["i1" "bb bench" "--runs" "5" "--drop" "1"])
        (cli/item-add s ["second" "--gate" "make check"])
        (cli/budget s ["--usd" "10"])
        (cli/cadence s ["batch 2 on plateau 3"])
        (let [sp (edn/read-string (cli/show-spec s []))]
          (is (= {:goal "by hand" :gate "bb test" :allow ["jj"] :budget {:usd 10.0}
                  :cadence {:batch 2 :plateau 3}
                  :items [{:intent "first" :measure "bb bench" :runs 5 :drop 1}
                          {:intent "second" :gate "make check"}]}
                 sp))
          (is (= "ok" (re-find #"ok" (cli/check [(spec-file (cli/show-spec s []))]))))
          (is (= sp (edn/read-string (cli/show-spec (session-from-spec sp) [])))))))
    (testing "a spec prints one key and one item per line"
      (is (= 8 (count (str/split-lines (spec/pretty sp))))))
    (testing "lume check prints ok with a count"
      (let [f (spec-file sp)] (is (= (str f ": ok · 3 items · 2 measured") (cli/check [f])))))
    (testing "lume check lists every problem, with where it is"
      (let [f (spec-file '{:alow [] :gate 3 :budget {:usd -1 :tokens 3}
                           :cadence {:batch 0 :gate-fail "yes"}
                           :items [{:intent "x" :runs 3 :drop 3} "y" {:measure "bb bench" :drop 2 :higher "score"}]})
            msg (try (cli/check [f]) nil (catch clojure.lang.ExceptionInfo e (ex-message e)))]
        (is (= [(str f ": 13 problems")
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
                "  [:items 2 :drop] must be less than :runs (1), got 2"]
               (str/split-lines msg)))))
    (testing "the list is bounded"
      (let [f (spec-file {:goal "g" :items (vec (repeat 30 {}))})
            lines (str/split-lines (try (cli/check [f]) (catch clojure.lang.ExceptionInfo e (ex-message e))))]
        (is (= (str f ": 30 problems") (first lines)))
        (is (= 22 (count lines)))
        (is (= "  +10 more" (last lines)))))
    (testing "an unreadable file is one problem"
      (doseq [[content problem] [["" "is empty"] ["{:goal" "is not EDN"] ["{:goal \"a\"} {}" "more than one form"]
                                 ["[1 2]" "a spec must be a map"]]]
        (is (thrown-with-msg? Exception (re-pattern (str ": 1 problem\n  .*" problem)) (cli/check [(spec-file content)]))
            content))
      (is (thrown-with-msg? Exception #"no such file" (cli/check ["/no/such/spec.edn"]))))
    (testing "check and spec write nothing in the repo"
      (is (empty? (fs/list-dir *root*))))
    (testing "usage"
      (is (thrown-with-msg? Exception #"usage" (cli/check [])))
      (is (thrown-with-msg? Exception #"usage" (cli/show-spec (store/current-session *root*) ["--open"])))
      (is (= "usage: lume check <spec.edn>" (cli/run ["check" "--help"]))))))

(defn- run! [& args]
  (let [ret (atom nil)
        out (with-out-str (reset! ret (cli/run-spec *root* (vec args))))]
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
      (is (re-find #"^usage: lume run" (cli/run ["run" "--help"]))))
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
        (is (= sp (edn/read-string (cli/show-spec s ["--all"]))) "the session's harness is the spec")
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
      (binding [cli/*actor* "d7"]
        (let [[ret] (run! (spec-file sp) "--turns" "0")
              events (store/read-events (store/current-session *root*))]
          (is (str/starts-with? ret "--turns 0"))
          (is (every? #(= "d7" (:by %)) events))
          (is (= sp (edn/read-string (cli/show-spec (store/current-session *root*) ["--all"]))))
          (is (= status/default-cadence (status/cadence-ceiling (status/project events)))
              "an agent's cadence is no ceiling")))
      (let [before (store/current-session *root*)]
        (doseq [[extra why] [[{:allow ["jj"]} #"workers may run jj"]
                             [{:cadence {:batch 3}} #"the cadence ceiling is raised: batch 1 → 3"]]]
          (binding [cli/*actor* "s4"]
            (is (re-find (re-pattern (str "^only the human runs a spec that sets policy \\(.*" why))
                         (refused (spec-file (merge sp extra)) "--turns" "0"))))
          (testing "and the human must confirm it at a terminal"
            (binding [cli/*confirm* (constantly false)]
              (is (re-find #"interactive terminal" (refused (spec-file (merge sp extra)) "--turns" "0"))))))
        (is (= before (store/current-session *root*)) "a refused run starts no session")))
    (testing "a human's confirmed policy is logged as theirs"
      (run! (spec-file (assoc sp :allow ["jj"] :cadence {:batch 3})) "--turns" "0")
      (let [state (status/project (store/read-events (store/current-session *root*)))]
        (is (= ["jj"] (:allow state)))
        (is (= 3 (:batch (status/cadence-ceiling state))))))))

(deftest claude-driver-args
  (is (= ["--model" "opus" "--tools" "Bash,Read"
          "--allowedTools" "Bash(lume:*),Bash(bin/lume:*),Bash(jj:*),Bash(bb test:*),Read"
          "--disallowedTools" "Bash(jj git push:*),Bash(git push:*)"]
         (agents/claude-driver-args "[\"--model\" \"opus\"]" ["bb test"]))))

(deftest claude-runs-get-no-mcp-servers
  (doseq [role [:worker :driver]]
    (is (some #{"--strict-mcp-config"} (agents/command :claude "p" {:role role :allow []}))
        (str (name role) " runs must not load the user's MCP servers"))))

(deftest pinned-notes-outlive-the-window
  (let [s (new-session "goal")
        id (cli/item-add s ["read the notes"])]
    (is (= "pinned n1" (cli/note s ["--pin" "run bb test before done"])))
    (dotimes [n 12] (cli/note s [(str "chatter " n)]))
    (is (= "noted n14" (cli/note s ["latest"])))
    (let [out (status/render "sess" (store/read-events s) (Instant/now))]
      (is (re-find #"(?m)^pinned  n1 run bb test before done$" out))
      (is (not (re-find #"\] run bb test" out)) "pinned notes are left out of notes")
      (is (< (str/index-of out "pinned  n1") (str/index-of out "open    i1"))))
    (testing "workers see every pinned note and the last 10 others"
      (cli/spawn s *root* [id "--agent" "prompt"])
      (let [prompt (slurp (get-in (status/project (store/read-events s)) [:spawns "s1" :out]))]
        (is (str/includes? prompt "Pinned notes:\n- run bb test before done\n"))
        (is (str/includes? prompt "- chatter 3\n"))
        (is (not (str/includes? prompt "- chatter 2\n")))
        (is (str/includes? prompt "- latest\n"))))
    (testing "unpinning returns a note to the notes section"
      (is (= "unpinned n1" (cli/note s ["--unpin" "n1"])))
      (is (thrown-with-msg? Exception #"no pinned note: n1" (cli/note s ["--unpin" "n1"])))
      (is (thrown-with-msg? Exception #"no pinned note: n99" (cli/note s ["--unpin" "n99"])))
      (let [out (status/render "sess" (store/read-events s) (Instant/now))]
        (is (not (re-find #"(?m)^pinned" out)))
        (is (str/includes? out "+9 more") "14 notes, 5 shown")))))

(deftest install-writes-the-skill-and-prints-the-settings-line
  (let [claude agents/*claude-home*
        skill (str (fs/path claude "skills" "lume" "SKILL.md"))
        settings (str (fs/path claude "settings.json"))]
    (is (str/starts-with? claude store/*home*) "tests never touch the real ~/.claude")
    (let [out (cli/install [])]
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
      (is (str/includes? (cli/install []) (str "unchanged " skill))))
    (testing "an edited skill is restored"
      (spit skill "old")
      (is (str/includes? (cli/install []) (str "updated   " skill))))
    (testing "a settings file that already sets the actor is reported, not edited"
      (spit settings "{\"env\": {\"LUME_ACTOR\": \"claude\"}}")
      (let [out (cli/install ["--agent" "claude"])]
        (is (str/includes? out "LUME_ACTOR=claude is set"))
        (is (not (str/includes? out "todo"))))
      (is (= "{\"env\": {\"LUME_ACTOR\": \"claude\"}}" (slurp settings))))
    (testing "devin: the same skill in devin's skills dir, lume's devin config, and the shell lines"
      (let [out (cli/install ["--agent" "devin"])
            devin-skill (str (fs/path agents/*devin-home* "skills" "lume" "SKILL.md"))
            config (str (fs/path store/*home* "agents" "devin.json"))]
        (is (str/includes? out (str "wrote     " devin-skill)))
        (is (= (slurp skill) (slurp devin-skill)))
        (is (= ["exec" "edit" "Write(/**)"] (get-in (json/parse-string (slurp config)) ["permissions" "ask"])))
        (is (str/includes? out "alias devin='LUME_ACTOR=devin devin'"))))
    (is (thrown-with-msg? Exception #"knows no agent" (cli/install ["--agent" "nope"])))
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
            id (cli/item-add s ["do the thing" "--gate" "true"])]
        (cli/item-measure s [id "echo build_s=1"])
        (cli/measure s [id])
        (is (str/includes? (cli/spawn s *root* [id "--agent" "test"]) "done"))))
    ;; Lume makes neither yet; clean must remove them when it does.
    (sh "git" "update-ref" "refs/lume/spawns/s1" "HEAD")
    (sh "git" "worktree" "add" "-q" "--detach" (str (fs/path dir "worktrees" "s1")))
    (is (seq (differing (tree control) (tree *root*))))
    (testing "workers and drivers may not clean"
      (binding [cli/*actor* "s1"]
        (is (thrown-with-msg? Exception #"don't delete the log" (cli/clean *root* ["--yes"])))))
    (testing "without --yes it only lists what it would remove"
      (let [e (is (thrown? Exception (cli/clean *root* [])))]
        (is (str/starts-with? (ex-message e) "refused without --yes"))
        (is (str/includes? (ex-message e) "  ref refs/lume/spawns/s1"))
        (is (str/includes? (ex-message e) (str "  dir " dir))))
      (is (fs/exists? dir)))
    (testing "--yes removes it all and says what"
      (let [out (cli/clean *root* ["--yes"])]
        (is (= [(str "removed worktree " (fs/path dir "worktrees" "s1"))
                "removed ref refs/lume/spawns/s1"
                (str "removed dir " (fs/path *root* ".git" "refs" "lume"))
                (str "removed dir " dir)]
               (str/split-lines out))))
      (is (not (fs/exists? dir)))
      (is (empty? (differing (tree control) (tree *root*))) "the repo is byte-identical to the control"))
    (is (str/starts-with? (cli/clean *root* []) "nothing to clean"))))

(deftest clean-one-session
  (let [old (new-session "old")
        cur (new-session "current")
        dir (store/repo-dir *root*)]
    (is (thrown-with-msg? Exception #"no session nope" (cli/clean *root* ["--session" "nope" "--yes"])))
    (is (= (str "removed dir " old) (cli/clean *root* ["--session" (store/session-id old) "--yes"])))
    (is (= cur (store/current-session *root*)) "the current session is untouched")
    (testing "the last session takes the repo's state with it"
      (is (= (str "removed dir " dir) (cli/clean *root* ["--session" (store/session-id cur) "--yes"])))
      (is (not (fs/exists? dir))))
    (testing "cleaning the current session, with others left, unsets current"
      (let [_ (new-session "a")
            b (new-session "b")]
        (is (= [(str "removed dir " b) (str "removed file " (fs/path dir "current"))]
               (str/split-lines (cli/clean *root* ["--session" (store/session-id b) "--yes"]))))
        (is (nil? (store/current-session *root*)))))))
