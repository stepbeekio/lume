(ns lume.cli
  "The lume command line: the built-in features' commands, run by the kernel.
  Output is agent-first: short, structured, bounded."
  (:require [lume.features :as features]
            [lume.kernel :as kernel]))

(defn -main [& args]
  (try
    (println (binding [kernel/*features* features/all] (kernel/run args)))
    (catch clojure.lang.ExceptionInfo e
      (if (:lume/user-error (ex-data e))
        (do (binding [*out* *err*] (println (ex-message e)))
            (System/exit 1))
        (throw e)))))
