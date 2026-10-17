(ns lume.runner
  "Runs test namespaces' tests in parallel. Most of the suite waits on
  subprocesses (spawns, gates, fake drivers calling bb), and each test has its
  own LUME_HOME and repo, so they overlap safely."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :as t]))

(defn test-namespaces
  "The namespace of every *_test.clj under dir, e.g. test/lume/kernel_test.clj
  → lume.kernel-test."
  [dir]
  (sort (for [f (fs/glob dir "**_test.clj")]
          (-> (str (fs/relativize dir f)) (str/replace #"\.clj$" "")
              (str/replace fs/file-separator ".") (str/replace "_" "-") symbol))))

(defn run-tests
  "Like clojure.test/run-tests, but each test var runs in its own future.
  Reports are serialized so a failure prints whole."
  [& ns-syms]
  (let [report t/report
        lock (Object.)]
    (binding [t/*report-counters* (ref t/*initial-report-counters*)
              t/report (fn [m] (locking lock (report m)))]
      ;; Every namespace's tests start at once, so splitting a namespace
      ;; doesn't serialize its tests behind another's.
      (run! deref (vec (for [ns (map the-ns ns-syms)
                             v (->> (vals (ns-interns ns)) (filter (comp :test meta)) (sort-by (comp :line meta)))]
                         (future (t/test-vars [v])))))
      (let [summary (assoc @t/*report-counters* :type :summary)]
        (t/do-report summary)
        summary))))

(defn run-all
  "Require and run every test namespace under dir."
  [dir]
  (let [nss (test-namespaces dir)]
    (apply require nss)
    (apply run-tests nss)))
