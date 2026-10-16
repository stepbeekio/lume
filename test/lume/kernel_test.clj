(ns lume.kernel-test
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [lume.agents :as agents]
            [lume.cli :as cli]
            [lume.spec :as spec]
            [lume.status :as status]
            [lume.store :as store])
  (:import (java.time Instant)))

(def ^:dynamic *root* nil)

(use-fixtures :each
  (fn [t]
    (let [home (str (fs/create-temp-dir {:prefix "lume-home"}))
          root (str (fs/create-temp-dir {:prefix "lume-repo"}))]
      (try
        (binding [store/*home* home store/*session* nil cli/*actor* "human" *root* root
                  cli/*confirm* (constantly true)
                  agents/*claude-home* (str (fs/path home "claude"))]
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
        (is (re-find #"\nm2 i1 build_s=12\.5 hits=3 · \d+\.\ds · vs m1\ni1 not done: gate g1" out) out)))
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
          (is (re-find #"m8 vs m6 build_s=9\.5\(-0\.1\) hits=3 · \d+\.\ds · " line) line))))
    (testing "a closed item can still be measured"
      (cli/item-close s [id "dropped" "x"])
      (is (str/starts-with? (cli/measure s [id]) "m9 i1 ")))))

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
      (is (re-matches #"m2 i1 k=7 t=10\.5\(-0\.5\)\[10\.\.12\] · \d+\.\ds · 4 of 5 runs · vs m1" (first (rows 1)))))
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
                                                     "LUME_ACTOR" "human"}}
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
        (is (str/includes? out "+5 more") "8 open asks, 3 shown")
        (is (str/includes? out "+14 more") "20 pinned, 6 shown")
        (is (str/includes? out "+49 more") "50 notes and 2 answers, 3 shown beside pinned")
        (is (str/includes? out "+16 more") "20 closed, 4 shown")
        (is (str/includes? out "+176 more items") "180 open with gates, 4 whole items shown")
        (is (re-find #"(?m)^        i24  .*\n            gate true\n        \+176" out)
            "an item's gate line is never cut off from it"))))
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
        (is (str/includes? (cli/show-status s ["--color"]) esc))
        (is (thrown-with-msg? Exception #"usage: lume status" (cli/show-status s ["--colour"])))))))

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

;; A fake driver: each turn runs the next shell script, which can call lume.
(def ^:private driver-turns (atom []))
(def ^:private driver-calls (atom []))

(defmethod agents/command :driver [_ prompt opts]
  (swap! driver-calls conj (assoc opts :prompt prompt))
  (let [script (or (first @driver-turns) "true")]
    (swap! driver-turns #(vec (rest %)))
    ["sh" "-c" script]))

(defn- lume-cmd [& args]
  (str/join " " (concat ["bb" "-cp" (str (fs/absolutize "src")) "-m" "lume.cli"]
                        (map #(str "'" % "'") args))))

(defn- drive! [s & turns]
  (reset! driver-turns (vec turns))
  (reset! driver-calls [])
  (with-out-str (is (string? (cli/drive s *root* ["--agent" "driver" "--turns" "3"]))))
  (last (keep :why (store/read-events s))))

(deftest drive-runs-driver-turns
  (testing "a driver's spawn is a request, run after its turn"
    (let [s (new-session "goal")]
      (cli/item-add s ["do it" "--gate" "true"])
      (is (= "no open items"
             (drive! s (str (lume-cmd "spawn" "i1" "--agent" "test") "; echo \"me $LUME_ACTOR\""))))
      (let [events (store/read-events s)
            {:keys [items spawns]} (status/project events)]
        (is (= [:drive/started :drive/turn :spawn/requested :drive/exited :spawn/started]
               (take 5 (drop 2 (map :type events)))))
        (is (= "d1" (:by (first (filter #(= :spawn/requested (:type %)) events)))))
        (is (= "done by s1" (get-in spawns ["s1" :result])))
        (is (= :done (get-in items ["i1" :state])) "the spawn's gate closed it"))
      (let [{:keys [prompt role]} (first @driver-calls)]
        (is (= :driver role))
        (is (str/includes? prompt "turn d1"))
        (is (str/includes? prompt "open    i1  do it") "the prompt carries status"))))
  (testing "it stops when a turn logs nothing, when turns run out, and when an ask is open"
    (let [s (new-session "goal")]
      (cli/item-add s ["never done"])
      (is (= "d1 logged no events" (drive! s "true")))
      (is (= "3 turns run" (drive! s (lume-cmd "note" "a") (lume-cmd "note" "b") (lume-cmd "note" "c"))))
      (is (= ["d2" "d3" "d4"] (map :by (:notes (status/project (store/read-events s)))))
          "driver ids keep counting across drives")
      (is (re-find #"^ask a1 is open: which\? " (drive! s (lume-cmd "ask" "which?") (lume-cmd "note" "never"))))
      (is (= 1 (count @driver-calls)) "no turn runs while an ask is open"))))

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
      (is (empty? @driver-calls) "no turn starts past a limit")
      (is (str/includes? (status/render "sess" (store/read-events s) (Instant/now)) "reached: no new spawns or turns")))
    (testing "dollars and minutes are limits too"
      (cli/budget s ["--spawns" "100" "--usd" "1.75"])
      (is (re-find #"^budget reached: \$1\.75 of \$1\.75 spent" (drive! s (lume-cmd "note" "never"))))
      (cli/budget s ["--usd" "100" "--minutes" "1"])
      (is (re-find #"^budget reached: 1\.\d of 1 minutes" (drive! s (lume-cmd "note" "never")))))))

(defn- spawn-cmds [& ids]
  (str/join "; " (map #(lume-cmd "spawn" % "--agent" "test") ids)))

(deftest cadence-decides-when-drive-hands-back
  (let [render #(status/render "sess" (store/read-events %) (Instant/now))
        started #(map :item/id (filter (fn [e] (= :spawn/started (:type e))) (store/read-events %)))
        handed-back #(first (filter (fn [e] (= :drive/handed-back (:type e))) (store/read-events %)))
        session (fn [cadence & gates]
                  (let [s (new-session "goal")]
                    (when cadence (cli/cadence s [cadence]))
                    (doseq [g gates] (cli/item-add s ["work" "--gate" g]))
                    s))]
    (testing "rules are data: no rules prints them, bad rules are usage errors"
      (let [s (session nil)
            cad #(status/cadence (status/project (store/read-events s)))]
        (is (= "cadence every-spawn · ceiling every-spawn" (cli/cadence s [])))
        (is (not (re-find #"(?m)^allow" (render s))) "an unset cadence isn't shown")
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
                       (render s))))
        (testing "the human raises at a terminal, and their cadence is the new ceiling"
          (binding [cli/*confirm* (constantly false)]
            (is (thrown-with-msg? Exception #"interactive terminal" (cli/cadence s ["batch 5"]))))
          (cli/cadence s ["every-spawn"])
          (binding [cli/*actor* "d1"]
            (is (thrown-with-msg? Exception #"batch 1 → 2" (cli/cadence s ["batch 2"]))))
          (is (re-find #"(?m)^allow   cadence every-spawn$" (render s))))))
    (testing "every-spawn: a turn may request one spawn"
      (let [s (session nil "false" "false")]
        (is (= "d2 logged no events" (drive! s (spawn-cmds "i1" "i2"))))
        (is (= ["i1"] (started s)))))
    (testing "batch N: up to N requests a turn, run in order, failed gates and all"
      (let [s (session "batch 3" "false" "false" "false" "false")]
        (binding [cli/*actor* "d9"]
          (is (re-find #"\(1 of 3 this turn\)" (cli/spawn s *root* ["i1"]))))
        (is (= "d2 logged no events" (drive! s (spawn-cmds "i3" "i2" "i1" "i4"))))
        (is (= ["i3" "i2" "i1"] (started s)))
        (is (nil? (handed-back s)))))
    (testing "on gate-fail: a failed gate ends the batch and hands back"
      (let [s (session "batch 3 on gate-fail" "true" "false" "true")]
        (is (= "d2 logged no events" (drive! s (spawn-cmds "i1" "i2" "i3"))))
        (is (= ["i1" "i2"] (started s)) "a passing gate carries on")
        (is (= {:drive/id "d1" :why "on gate-fail: i2 gate g2 failed" :skipped ["i3"]}
               (select-keys (handed-back s) [:drive/id :why :skipped])))))
    (testing "on plateau K: K rows without a new best ask the human, and drive stops"
      (let [s (session "batch 5 on plateau 2" "false")]
        (spit (str (fs/path *root* "t")) "t=5")
        (cli/item-measure s ["i1" "cat t"])
        (is (re-find #"^ask a1 is open: i1 plateaued: 2 measurement rows without a new best \(best m1, lower is better\)"
                     (drive! s (spawn-cmds "i1" "i1" "i1" "i1") (lume-cmd "note" "never"))))
        (is (= ["i1" "i1" "i1"] (started s)))
        (is (= ["i1"] (:skipped (handed-back s))))
        (is (= "an ask is open" (:why (handed-back s))))
        (is (= 1 (count @driver-calls)))
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
                 (select-keys (get-in (status/project (store/read-events s)) [:items "i1" :plateau]) [:stale :best-id]))))))))

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
                    {:intent "just measured" :measure "bb size"}]}]
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
                           :items [{:intent "x" :runs 3 :drop 3} "y" {:measure "bb bench" :drop 2}]})
            msg (try (cli/check [f]) nil (catch clojure.lang.ExceptionInfo e (ex-message e)))]
        (is (= [(str f ": 12 problems")
                "  [:alow] unknown key; expected one of :allow :budget :cadence :gate :goal :items"
                "  [:goal] is required"
                "  [:gate] must be a command (non-blank string), got 3"
                "  [:budget :tokens] unknown key; expected one of :minutes :spawns :usd"
                "  [:budget :usd] must be a non-negative number, got -1"
                "  [:cadence :batch] must be a positive whole number, got 0"
                "  [:cadence :gate-fail] must be true or false, got \"yes\""
                "  [:items 0] :runs and :drop need a :measure"
                "  [:items 0 :drop] must be less than :runs (3), got 3"
                "  [:items 1] must be a map with :intent, got \"y\""
                "  [:items 2 :intent] is required"
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
    (is (thrown-with-msg? Exception #"knows no agent" (cli/install ["--agent" "nope"])))
    (is (empty? (fs/list-dir *root*)) "nothing is written in the repo")))
