(ns lume.session
  "Sessions: lume init starts one, with its goal, repo and default gate."
  (:require [clojure.string :as str]
            [lume.kernel :as kernel :refer [actor fail! human-only! take-flags]]
            [lume.store :as store]))

;; A session may have a default gate: items added without --gate get it, so
;; nothing is ungated by omission. --allow is lume allow, so it's human-only too.
(def ^:private init-usage "usage: lume init \"<goal>\" [--gate \"<cmd>\"] [--allow \"<cmd>\"]...")

(defn init [root args]
  (let [usage init-usage
        [[goal & extra] {gates "--gate" allows "--allow"}] (take-flags args #{"--gate" "--allow"} usage)
        gate (first gates)
        allows (distinct allows)]
    (when (or (str/blank? goal) (seq extra) (next gates)) (fail! usage))
    (when (seq allows)
      (human-only! "only the human changes permissions; ask with lume ask \"<question>\""
                   (str "Workers will be allowed to run: " (str/join " · " allows) ".") "yes"))
    (let [session (store/new-session! root)]
      (kernel/append! session (constantly {:type :session/started :goal goal :repo root :by (actor)}))
      (when gate
        (kernel/append! session (constantly {:type :session/gated :gate gate :by (actor)})))
      (doseq [command allows]
        (kernel/append! session (constantly {:type :session/allowed :command command :by (actor)})))
      (str "session " (store/session-id session) " started"
           (when gate (str " · default gate " gate))
           (when (seq allows) (str " · workers may run " (str/join " · " allows)))))))

(def feature
  {:name "session"
   :events {:session/started {:req #{:goal :repo}}
            :session/gated {:req #{:gate}}}
   :fold {:session/started (fn [s e] (assoc s :goal (:goal e) :repo (:repo e)))
          :session/gated (fn [s e] (assoc s :default-gate (:gate e)))}
   :status [{:label "goal" :order 10 :cap 1 :lines (fn [s _ _] [(str (:goal s))])}]
   :commands [{:path ["init"] :needs :root :usage init-usage
               :run (fn [{:keys [root]} args] (init root args))
               :help "lume init \"<goal>\" [--gate \"<cmd>\"] [--allow \"<cmd>\"]...
                                            start a session; items added without --gate get its --gate"}]})
