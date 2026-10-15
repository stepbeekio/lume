(ns lume.kernel-test
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
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
        (binding [store/*home* home *root* root] (t))
        (finally (fs/delete-tree home) (fs/delete-tree root))))))

(defn- new-session [goal]
  (cli/init *root* [goal])
  (store/current-session *root*))

(defmethod agents/command :test [_ _prompt]
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

(deftest concurrent-writers-get-unique-ids
  (let [s (new-session "goal")
        procs (doall (for [n (range 8)]
                       (p/process ["bb" "-cp" "src" "-m" "lume.cli" "item" "add" (str "item " n)]
                                  {:extra-env {"LUME_SESSION" s} :out :string :err :string})))
        ids (map #(str/trim (:out @%)) procs)]
    (is (= (set (map #(str "i" %) (range 1 9))) (set ids)))
    (is (= 9 (count (store/read-events s))))))

(deftest status-stays-bounded
  (let [s (new-session "goal")]
    (dotimes [n 200] (cli/item-add s [(str "item " n " " (apply str (repeat 50 "x")))]))
    (dotimes [n 50] (cli/note s [(str "note " n)]))
    (let [out (status/render "sess" (store/read-events s) (Instant/now))
          lines (str/split-lines out)]
      (is (< (count lines) 40) out)
      (is (every? #(<= (count %) 110) lines) out)
      (is (str/includes? out "+190 more")))))
