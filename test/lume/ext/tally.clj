(ns lume.ext.tally
  "The test extension (lume.ext-test): lume tally add <n> logs a count, lume
  tally prints the total; it also counts gate runs, and lists each count in
  status."
  (:require [clojure.string :as str]
            [lume.kernel :as kernel]))

(def feature
  {:events {:tally/added {:req #{:n}}
            :tally/seen {:req #{:of}}}
   :init {:counts [] :gates 0}
   :fold {:tally/added (fn [s e] (update s :counts conj (:n e)))
          :gate/ran (fn [s _] (update s :gates inc))}
   :guards {:tally/added (fn [s e] (when (some #{(:n e)} (:counts s)) (str "refused: " (:n e) " is already tallied")))}
   :reactions [{:on :tally/added :run (fn [e] (kernel/append! kernel/*session* (constantly {:type :tally/seen :of (:n e)})))}]
   :commands [{:path ["tally"] :needs :session :usage "usage: lume tally"
               :run (fn [{:keys [session]} _]
                      (let [{:keys [counts gates]} (get-in (kernel/project (kernel/read-events session)) [:ext "tally"])]
                        (str "total " (reduce + counts) " · gates " gates)))
               :help "lume tally                                  the total so far"}
              {:path ["tally" "add"] :needs :session :usage "usage: lume tally add <n>"
               :run (fn [{:keys [session]} [n]]
                      (kernel/append! session (constantly {:type :tally/added :n (parse-long (str/trim (str n)))}))
                      (str "tallied " n))
               :help "lume tally add <n>                          add a count"}]
   :status [{:lines (fn [{:keys [counts gates]}]
                      (cons (str "total " (reduce + counts) " · gates " gates) (map #(str "+" %) counts)))}]})
