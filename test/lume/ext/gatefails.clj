(ns lume.ext.gatefails
  "An extension an agent wrote for i80's done-check: gate failures per item,
  in status, and in full with lume gatefails."
  (:require [clojure.string :as str]
            [lume.kernel :as kernel]))

(defn- by-item [fails]
  (str/join " · " (for [[id n] (sort-by (comp - val) fails)] (str id " " n))))

(def feature
  {:init {}
   :fold {:gate/ran (fn [s e] (if (zero? (:exit e)) s (update s (:item/id e) (fnil inc 0))))}
   :commands [{:path ["gatefails"] :needs :session :usage "usage: lume gatefails"
               :run (fn [{:keys [session]} _]
                      (let [fails (get-in (kernel/project (kernel/read-events session)) [:ext "gatefails"])]
                        (if (seq fails) (by-item fails) "no gate has failed")))
               :help "lume gatefails                              gate failures per item"}]
   :status [{:lines (fn [fails] (when (seq fails) [(str "gate failures " (by-item fails))]))}]})
