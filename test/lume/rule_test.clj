(ns lume.rule-test
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [lume.agent :as agent]
            [lume.agents :as agents]
            [lume.drive :as drive]
            [lume.features :as features]
            [lume.item :as item]
            [lume.kernel :as kernel]
            [lume.note :as note]
            [lume.ntfy :as ntfy]
            [lume.rule :as rule]
            [lume.session :as session]
            [lume.store :as store]))

(def ^:dynamic *root* nil)

(use-fixtures :each
  (fn [t]
    (let [home (str (fs/create-temp-dir {:prefix "lume-home"}))
          root (str (fs/create-temp-dir {:prefix "lume-repo"}))]
      (try
        (binding [store/*home* home store/*session* nil kernel/*actor* "human" kernel/*spawned-as* nil
                  kernel/*depth* 0 *root* root
                  kernel/*features* features/all
                  kernel/*confirm* (constantly true) kernel/*read-line* (constantly nil)
                  drive/*source* nil ntfy/*ntfy* nil]
          (t))
        (finally (fs/delete-tree home) (fs/delete-tree root))))))

(defn- new-session [goal]
  (session/init *root* [goal])
  (store/current-session *root*))

(defn- of-type [s type] (filter #(= type (:type %)) (store/read-events s)))

(defn- lume-cmd [& args]
  (str/join " " (concat ["bb" "-cp" (str (fs/absolutize "src")) "-m" "lume.cli"]
                        (map #(str "'" % "'") args))))

(deftest rules-are-the-humans
  (let [s (new-session "goal")]
    (testing "an agent can't add or remove a rule"
      (binding [kernel/*actor* "s1"]
        (is (thrown-with-msg? Exception #"only the human" (rule/on s ["gate/ran" "true"])))))
    (testing "a pattern must name a declared type or namespace"
      (is (thrown-with-msg? Exception #"no event type or namespace gate/rna" (rule/on s ["gate/rna" "true"]))))
    (is (= "r1" (rule/on s ["gate/ran" "true"])))
    (is (= "r2" (rule/on s ["ask" "echo asked"])))
    (is (= "r1 on gate/ran → true\nr2 on ask → echo asked" (rule/on s [])))
    (binding [kernel/*actor* "s1"]
      (is (thrown-with-msg? Exception #"only the human" (rule/on s ["--remove" "r1"]))))
    (is (= "removed r1" (rule/on s ["--remove" "r1"])))
    (is (= "r2 on ask → echo asked" (rule/on s [])))
    (is (thrown-with-msg? Exception #"usage" (rule/on s ["gate/ran"])))))

(deftest rules-run-on-their-events
  (let [s (new-session "goal")
        seen (str (fs/path *root* "seen"))
        id (item/item-add s ["gated" "--gate" "exit 3"])
        gate! #(item/run-gate s (get-in (kernel/project (store/read-events s)) [:items id]))]
    (rule/on s ["gate/ran" (str "echo \"$LUME_EVENT_ITEM_ID $LUME_EVENT_EXIT $LUME_ACTOR\" >> " seen "; echo \"$LUME_EVENT\" > " seen ".edn")])
    (testing "the event reaches the command in env, and the run is logged"
      (gate!)
      (is (= "i1 3 r1\n" (slurp seen)))
      (is (= :gate/ran (:type (edn/read-string (slurp (str seen ".edn"))))))
      (let [[ran] (of-type s :rule/ran)]
        (is (= ["r1" :gate/ran 0 "r1"] ((juxt :rule/id :trigger :exit :by) ran)))
        (is (number? (:ms ran)))
        (is (re-find #"on gate/ran → .* · last exit 0" (rule/on s [])))))
    (testing "a failing or hung rule is logged and never fails what triggered it"
      (rule/on s ["--remove" "r1"])
      (rule/on s ["gate/ran" "echo broke; exit 7"])
      (rule/on s ["gate/ran" "sleep 5"])
      (binding [rule/*timeout-ms* 200]
        (is (= 3 (:exit (gate!)))))
      (is (= [["r2" 7 "broke" nil] ["r3" 124 nil true]]
             (map (juxt :rule/id :exit :tail :timed-out) (rest (of-type s :rule/ran))))))
    (testing "a removed rule doesn't run"
      (is (= 1 (count (filter #(= "r1" (:rule/id %)) (of-type s :rule/ran))))))))

;; A worker that notes: its event reacts in the spawn, after it exits.
(defmethod agents/command :rule-noter [_ _prompt _opts]
  ["sh" "-c" (lume-cmd "note" "from the worker")])

(deftest rules-run-for-workers-in-their-spawn
  (let [s (new-session "goal")
        id (item/item-add s ["note"])]
    (rule/on s ["note/added" "echo \"$LUME_EVENT_TEXT\""])
    (with-out-str (agent/spawn s *root* [id "--agent" "rule-noter"]))
    (is (= [["from the worker" "r1"]] (map (juxt :tail :by) (of-type s :rule/ran))))))

(deftest rule-appends-count-toward-the-depth-cap
  (let [s (new-session "goal")]
    (rule/on s ["note/added" (lume-cmd "note" "again")])
    (note/note s ["by hand"])
    (testing "a rule's own appends react one level deeper, until the cap"
      (is (= ["by hand" "again" "again"] (map :text (of-type s :note/added))))
      (is (= ["human" "r1" "r1"] (map :by (of-type s :note/added))))
      (is (= [0 0] (map :exit (of-type s :rule/ran))))
      (is (= [["r1" :note/added]] (map (juxt :rule/id :trigger) (of-type s :rule/capped)))))))
