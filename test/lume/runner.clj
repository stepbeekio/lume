(ns lume.runner
  "Runs a namespace's tests in parallel. Most of the suite waits on
  subprocesses (spawns, gates, fake drivers calling bb), and each test has its
  own LUME_HOME and repo, so they overlap safely."
  (:require [clojure.test :as t]))

(defn run-tests
  "Like clojure.test/run-tests for one namespace, but each test var runs in
  its own future. Reports are serialized so a failure prints whole."
  [ns-sym]
  (let [ns (the-ns ns-sym)
        vars (->> (vals (ns-interns ns)) (filter (comp :test meta)) (sort-by (comp :line meta)))
        report t/report
        lock (Object.)]
    (binding [t/*report-counters* (ref t/*initial-report-counters*)
              t/report (fn [m] (locking lock (report m)))]
      (t/do-report {:type :begin-test-ns :ns ns})
      (run! deref (mapv (fn [v] (future (t/test-vars [v]))) vars))
      (t/do-report {:type :end-test-ns :ns ns})
      (let [summary (assoc @t/*report-counters* :type :summary)]
        (t/do-report summary)
        summary))))
