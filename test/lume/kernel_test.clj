(ns lume.kernel-test
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [lume.agents :as agents]
            [lume.cli :as cli]
            [lume.status :as status]
            [lume.store :as store])
  (:import (java.time Instant)))

(def ^:dynamic *root* nil)

(use-fixtures :each
  (fn [t]
    (let [home (str (fs/create-temp-dir {:prefix "lume-home"}))
          root (str (fs/create-temp-dir {:prefix "lume-repo"}))]
      (try
        (binding [store/*home* home store/*session* nil cli/*actor* "human" *root* root] (t))
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
      (is (thrown-with-msg? Exception #"no such item" (cli/item-close s ["i9" "done" "x"]))))))

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

(defmethod agents/command :allowed [_ _prompt {:keys [allow]}]
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
      (let [out (status/render "sess" (store/read-events s) (Instant/now))
            lines (str/split-lines out)]
        (is (<= (count lines) 40) out)
        (is (every? #(<= (count %) 110) lines) out)
        (doseq [section ["goal" "asks" "allow" "pinned" "open" "closed" "notes" "recent" "next"]]
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
        (is (= [:drive/started :drive/turn :spawn/requested :spawn/started]
               (take 4 (drop 2 (map :type events)))))
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

(deftest claude-driver-args
  (is (= ["--model" "opus" "--tools" "Bash,Read"
          "--allowedTools" "Bash(lume:*),Bash(bin/lume:*),Bash(jj:*),Bash(bb test:*),Read"
          "--disallowedTools" "Bash(jj git push:*),Bash(git push:*)"]
         (agents/claude-driver-args "[\"--model\" \"opus\"]" ["bb test"]))))

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
