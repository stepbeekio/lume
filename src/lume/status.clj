(ns lume.status
  "lume status: a bounded, one-screen projection of the log.

  Deliberately naive. Whenever a driver wanted something this didn't show,
  that's a work item."
  (:require [clojure.string :as str])
  (:import (java.time Duration Instant ZoneId)
           (java.time.format DateTimeFormatter)))

(defn project
  "Fold the log into the current state of the session."
  [events]
  (reduce
   (fn [s {:keys [type] :as e}]
     (case type
       :session/started (assoc s :goal (:goal e) :repo (:repo e))
       :item/added      (update s :items assoc (:item/id e)
                                {:id (:item/id e) :intent (:intent e) :state :open :order (count (:items s))})
       :item/closed     (update-in s [:items (:item/id e)] assoc
                                   :state (:disposition e) :why (:why e) :closed-at (:t e))
       :note/added      (update s :notes conj e)
       :spawn/started   (-> s
                            (assoc-in [:spawns (:spawn/id e)] e)
                            (update-in [:items (:item/id e) :spawns] (fnil conj []) (:spawn/id e)))
       :spawn/exited    (update-in s [:spawns (:spawn/id e)] merge e)
       s))
   {:items {} :notes [] :spawns {}}
   events))

;; Rendering

(def ^:private width 100)

(defn- clip [s n]
  (let [s (str/replace (str s) #"\s+" " ")]
    (if (> (count s) n) (str (subs s 0 (dec n)) "…") s)))

(defn- ago [now t]
  (let [secs (.getSeconds (Duration/between (Instant/parse t) now))]
    (cond (< secs 60) (str secs "s ago")
          (< secs 3600) (str (quot secs 60) "m ago")
          (< secs 86400) (str (quot secs 3600) "h ago")
          :else (str (quot secs 86400) "d ago"))))

(defn- clock [t]
  (.format (.atZone (Instant/parse t) (ZoneId/systemDefault)) (DateTimeFormatter/ofPattern "HH:mm")))

(defn- spawn-line [now {:keys [exit interrupted result t] :as sp}]
  (str (:spawn/id sp) " "
       (cond interrupted "interrupted"
             (contains? sp :exit) (str "exit " exit)
             :else "running")
       " · " (ago now t)
       (when result (str " · " (clip result 60)))))

(defn- section [label lines]
  (when (seq lines)
    (cons (format "%-8s%s" label (first lines))
          (map #(str "        " %) (rest lines)))))

(defn- capped [n more-hint xs]
  (if (> (count xs) n)
    (concat (take n xs) [(str "+" (- (count xs) n) " more — " more-hint)])
    xs))

(defn- event-line [e]
  (str (clock (:t e)) " " (namespace (:type e)) "/" (name (:type e)) " "
       (clip (str/join " " (keep e [:item/id :spawn/id :disposition :exit :intent :text :goal])) 70)))

(defn render
  "Status as text. Every section is capped, so it stays one screen however
  long the session runs."
  [session-id events now]
  (let [{:keys [goal repo items notes spawns]} (project events)
        items (sort-by :order (vals items))
        {open true closed false} (group-by #(= :open (:state %)) items)]
    (str/join
     "\n"
     (concat
      [(str "lume status · " session-id (when repo (str " · " repo)))]
      (section "goal" [(clip goal (- width 8))])
      (section "open"
               (capped 10 "lume log"
                       (mapcat (fn [{:keys [id intent] :as item}]
                                 (cons (str id "  " (clip intent 80))
                                       (when-let [sp (get spawns (peek (:spawns item)))]
                                         [(str "    " (spawn-line now sp))])))
                               open)))
      (section "closed"
               (capped 5 "lume log"
                       (map (fn [{:keys [id state intent why]}]
                              (str id "  " (name state) " — " (clip intent 40) (when why (str " — " (clip why 40)))))
                            (reverse (sort-by :closed-at closed)))))
      (section "notes" (capped 5 "lume log"
                               (map #(str "[" (clock (:t %)) " " (:by %) "] " (clip (:text %) 80))
                                    (reverse notes))))
      (section "recent" (map event-line (take-last 6 events)))
      (section "next" ["lume item add \"<intent>\" · lume spawn <id> · lume item close <id> done|dropped \"<why>\""
                       "lume note \"<text>\" · lume log [-n N]"])))))
