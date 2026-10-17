(ns lume.policy-test
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [lume.agents :as agents]
            [lume.cli :as cli]
            [lume.drive :as drive]
            [lume.kernel :as kernel]
            [lume.measure :as measure]
            [lume.ntfy :as ntfy]
            [lume.policy :as policy]
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
                  kernel/*features* cli/features
                  kernel/*confirm* (constantly true) kernel/*read-line* (constantly nil)
                  drive/*source* nil ntfy/*ntfy* nil drive/*reexec* (fn [& _] (throw (ex-info "no exec in tests" {})))
                  *driver-turns* (atom []) *driver-calls* (atom [])]
          (t))
        (finally (fs/delete-tree home) (fs/delete-tree root))))))

(defn- new-session [goal]
  (cli/init *root* [goal])
  (store/current-session *root*))

;; A worker that does nothing and fails, and a driver that runs the next of
;; *driver-turns* as a shell script and summarizes like claude, so a turn can
;; report a cost on a result line.
(defmethod agents/command :policy-worker [_ _prompt _opts]
  ["sh" "-c" "printf 'working\\ndone by %s\\n' \"$LUME_ACTOR\"; exit 3"])

(defmethod agents/command :policy-driver [_ prompt opts]
  (swap! *driver-calls* conj (assoc opts :prompt prompt))
  (let [script (or (first @*driver-turns*) "true")]
    (swap! *driver-turns* #(vec (rest %)))
    ["sh" "-c" script]))

(defmethod agents/summarize :policy-driver [_ out] (agents/summarize :claude out))

(defn- lume-cmd [& args]
  (str/join " " (concat ["bb" "-cp" (str (fs/absolutize "src")) "-m" "lume.cli"]
                        (map #(str "'" % "'") args))))

(defn- drive! [s & turns]
  (reset! *driver-turns* (vec turns))
  (reset! *driver-calls* [])
  (with-out-str (is (string? (drive/drive s *root* ["--agent" "policy-driver" "--turns" "3"]))))
  (last (keep :why (store/read-events s))))

(defn- result-line [usd]
  (str "echo '" (json/generate-string {:type "result" :result "ok" :total_cost_usd usd}) "'"))

(deftest budgets-bound-spend
  (let [s (new-session "goal")
        spent #(:spent (status/project (store/read-events s)))]
    (cli/item-add s ["work" "--gate" "false"])
    (testing "with no flags it prints spent against limits"
      (is (= "budget $0.00 · 0 spawns · 0 min" (policy/budget s []))))
    (testing "bad values are usage errors"
      (doseq [args [["--usd"] ["--usd" "x"] ["--spawns" "1.5"] ["--minutes" "-1"] ["10"] ["--usd" "1" "--usd" "2"]]]
        (is (thrown-with-msg? Exception #"usage" (policy/budget s args)) (pr-str args))))
    (testing "agents may set and lower limits, never raise them"
      (binding [kernel/*actor* "d1"]
        (is (= "budget $0.00/$5.00 · 0/3 spawns · 0 min" (policy/budget s ["--usd" "5" "--spawns" "3"])))
        (policy/budget s ["--spawns" "2"])
        (is (thrown-with-msg? Exception #"only the human raises" (policy/budget s ["--spawns" "4"])))
        (is (thrown-with-msg? Exception #"only the human raises" (policy/budget s ["--usd" "1" "--spawns" "9"]))
            "a raise refuses the whole command, lowering included"))
      (is (= {:usd 5.0 :spawns 2} (:budget (status/project (store/read-events s))))))
    (testing "the human raises at a terminal, with a typed confirmation"
      (binding [kernel/*confirm* (constantly false)]
        (is (thrown-with-msg? Exception #"interactive terminal" (policy/budget s ["--spawns" "4"]))))
      (policy/budget s ["--spawns" "4"])
      (is (= 4 (:spawns (:budget (status/project (store/read-events s)))))))
    (testing "spawns and drive turns are spent: count, dollars and minutes"
      (cli/spawn s *root* ["i1" "--agent" "policy-worker"])
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
      (policy/budget s ["--spawns" "1"])
      (cli/item-add s ["more" "--gate" "true"])
      (is (thrown-with-msg? Exception #"^budget reached: 1 of 1 spawns — only the human raises it, with lume budget --spawns N$"
                            (cli/spawn s *root* ["i2" "--agent" "policy-worker"])))
      (binding [kernel/*actor* "d9"]
        (is (thrown-with-msg? Exception #"budget reached" (cli/spawn s *root* ["i2"])) "a driver's request too"))
      (is (re-find #"^budget reached: 1 of 1 spawns" (drive! s (lume-cmd "note" "never"))))
      (is (empty? @*driver-calls*) "no turn starts past a limit")
      (is (str/includes? (status/render "sess" (store/read-events s) (Instant/now)) "reached: no new spawns or turns")))
    (testing "dollars and minutes are limits too"
      (policy/budget s ["--spawns" "100" "--usd" "1.75"])
      (is (re-find #"^budget reached: \$1\.75 of \$1\.75 spent" (drive! s (lume-cmd "note" "never"))))
      (policy/budget s ["--usd" "100" "--minutes" "1"])
      (is (re-find #"^budget reached: 1\.\d of 1 minutes" (drive! s (lume-cmd "note" "never")))))))

(defn- spawn-cmds [& ids]
  (str/join "; " (map #(lume-cmd "spawn" % "--agent" "policy-worker") ids)))

;; Cadence tests: one deftest per rule, so the drive-heavy ones run in parallel.
(defn- started [s] (map :item/id (filter #(= :spawn/started (:type %)) (store/read-events s))))
(defn- handed-back [s] (first (filter #(= :drive/handed-back (:type %)) (store/read-events s))))

(defn- cadence-session [cadence & gates]
  (let [s (new-session "goal")]
    (when cadence (policy/cadence-cmd s [cadence]))
    (doseq [g gates] (cli/item-add s ["work" "--gate" g]))
    s))

(deftest cadence-rules-are-data
  (testing "rules are data: no rules prints them, bad rules are usage errors"
    (let [s (cadence-session nil)
          render #(status/render "sess" (store/read-events s) (Instant/now))
          cad #(policy/cadence (status/project (store/read-events s)))]
      (is (= "cadence every-spawn · ceiling every-spawn" (policy/cadence-cmd s [])))
      (is (not (re-find #"(?m)^allow" (render))) "an unset cadence isn't shown")
      (doseq [args [["batch"] ["batch" "0"] ["on"] ["on" "plateau"] ["on" "fail"] ["sometimes"]]]
        (is (thrown-with-msg? Exception #"usage: lume cadence" (policy/cadence-cmd s args)) (pr-str args)))
      (is (= "cadence batch 3 on gate-fail on plateau 4 · ceiling batch 3 on gate-fail on plateau 4"
             (policy/cadence-cmd s ["batch 3" "on" "gate-fail" "on plateau 4"])))
      (is (= {:batch 3 :gate-fail true :plateau 4} (cad)))
      (testing "a driver may hand back more often, within the human's ceiling, never less"
        (binding [kernel/*actor* "d1"]
          (policy/cadence-cmd s ["batch 2 on gate-fail on plateau 2"])
          (doseq [rules ["batch 4 on gate-fail on plateau 4" "batch 2 on plateau 2" "batch 2 on gate-fail"
                         "batch 2 on gate-fail on plateau 5"]]
            (is (thrown-with-msg? Exception #"^only the human hands back less often \(" (policy/cadence-cmd s [rules])) rules)))
        (is (= {:batch 2 :gate-fail true :plateau 2} (cad)))
        (is (re-find #"(?m)^allow   cadence batch 2 on gate-fail on plateau 2 \(ceiling batch 3 on gate-fail on plateau 4\)$"
                     (render))))
      (testing "the human raises at a terminal, and their cadence is the new ceiling"
        (binding [kernel/*confirm* (constantly false)]
          (is (thrown-with-msg? Exception #"interactive terminal" (policy/cadence-cmd s ["batch 5"]))))
        (policy/cadence-cmd s ["every-spawn"])
        (binding [kernel/*actor* "d1"]
          (is (thrown-with-msg? Exception #"batch 1 → 2" (policy/cadence-cmd s ["batch 2"]))))
        (is (re-find #"(?m)^allow   cadence every-spawn$" (render)))))))

(deftest cadence-every-spawn-and-batch
  (testing "every-spawn: a turn may request one spawn"
    (let [s (cadence-session nil "false" "false")]
      (is (= "d2 logged no events" (drive! s (spawn-cmds "i1" "i2"))))
      (is (= ["i1"] (started s)))
      (binding [kernel/*actor* "d9"]
        (cli/spawn s *root* ["i2"])
        (is (thrown-with-msg? Exception #"^cadence every-spawn allows 1 spawn request\(s\) a turn, and this turn has made them"
                              (cli/spawn s *root* ["i2"]))))))
  (testing "batch N: up to N requests a turn, run in order, failed gates and all"
    (let [s (cadence-session "batch 3" "false" "false" "false" "false")]
      (binding [kernel/*actor* "d9"]
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
      (measure/item-measure s ["i1" "cat t"])
      (is (re-find #"^ask a1 is open: i1 plateaued: 2 measurement rows without a new best \(best m1, lower is better\)"
                   (drive! s (spawn-cmds "i1" "i1" "i1" "i1") (lume-cmd "note" "never"))))
      (is (= ["i1" "i1" "i1"] (started s)))
      (is (= ["i1"] (:skipped (handed-back s))))
      (is (= "an ask is open" (:why (handed-back s))))
      (is (= 1 (count @*driver-calls*)))
      (testing "one ask at a time per item, and a new best resets the count"
        (measure/measure s ["i1"])
        (is (not (str/includes? (measure/measure s ["i1"]) "ask")) "m5 is 4 rows on, but a1 is open")
        (cli/answer s ["a1" "keep going"])
        (measure/measure s ["i1"])
        (is (str/includes? (measure/measure s ["i1"]) "\nask a2 i1 plateaued: 6 measurement rows"))
        (cli/answer s ["a2" "keep going"])
        (spit (str (fs/path *root* "t")) "t=4")
        (is (re-find #"^m8 i1 t=4\(-1\)" (measure/measure s ["i1"])))
        (is (= {:stale 0 :best-id "m8"}
               (select-keys (get-in (status/project (store/read-events s)) [:items "i1" :plateau]) [:stale :best-id])))))))
