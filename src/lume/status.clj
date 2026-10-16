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
       :session/gated   (assoc s :default-gate (:gate e))
       :session/allowed (update s :allow #(vec (distinct (conj % (:command e)))))
       :session/disallowed (update s :allow #(vec (remove #{(:command e)} %)))
       :item/added      (update s :items assoc (:item/id e)
                                (cond-> {:id (:item/id e) :intent (:intent e) :state :open :order (count (:items s))}
                                  (:gate e) (assoc :gate (:gate e))))
       :item/gated      (assoc-in s [:items (:item/id e) :gate] (:gate e))
       :gate/ran        (assoc-in s [:items (:item/id e) :last-gate] e)
       :item/closed     (update-in s [:items (:item/id e)] assoc
                                   :state (:disposition e) :why (:why e) :closed-at (:t e))
       :note/added      (update s :notes conj e)
       :ask/opened      (update s :asks assoc (:ask/id e) {:id (:ask/id e) :question (:question e) :state :open})
       ;; An answer is steering, like a note, so it joins the notes.
       :ask/answered    (-> s
                            (assoc-in [:asks (:ask/id e) :state] :answered)
                            (update :notes conj (assoc (select-keys e [:ask/id :answer :t :by])
                                                       :question (get-in s [:asks (:ask/id e) :question])
                                                       :text (str (:ask/id e) " → " (:answer e)))))
       :note/unpinned  (update s :notes (partial mapv #(cond-> % (= (:note/id e) (:note/id %)) (dissoc :pinned))))
       :spawn/started   (-> s
                            (assoc-in [:spawns (:spawn/id e)] e)
                            (update-in [:items (:item/id e) :spawns] (fnil conj []) (:spawn/id e)))
       :spawn/progress  (assoc-in s [:spawns (:spawn/id e) :progress] e)
       :spawn/exited    (update-in s [:spawns (:spawn/id e)] merge e)
       s))
   {:items {} :notes [] :spawns {} :allow [] :asks {}}
   events))

;; Rendering

(def ^:private width 100)

;; Colour is for humans at a terminal. It wraps text that is already clipped
;; and padded, so it never changes visible width or line count.
(def ^:private ^:dynamic *color* false)

(def ^:private sgr {:dim "2" :yellow "33" :green "32" :red "31" :cyan "36"})

(defn- paint [style s]
  (if *color* (str "\u001b[" (sgr style) "m" s "\u001b[0m") s))

(defn strip-color [s]
  (str/replace s #"\u001b\[[0-9;]*m" ""))

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

(defn- spawn-line [now {:keys [exit interrupted result t progress] :as sp}]
  (let [running (not (or interrupted (contains? sp :exit)))]
    (str (:spawn/id sp) " "
         (cond interrupted "interrupted"
               running (paint :cyan "running")
               :else (str "exit " exit))
         " · " (ago now t)
         (when-let [steps (or (:steps sp) (:steps progress))] (str " · " steps (if (= 1 steps) " step" " steps")))
         (cond result (str " · " (clip result 60))
               (and running (:last progress)) (str " · " (clip (:last progress) 50) " (" (ago now (:t progress)) ")")))))

(defn- gate-line [now {:keys [gate last-gate]}]
  (str "gate " (clip gate 40)
       (when-let [{:keys [exit t tail] gid :gate/id} last-gate]
         (str " · " gid " " (if (zero? exit) (paint :green "pass") (paint :red (str "fail exit " exit))) " · " (ago now t)
              (when (and tail (not (zero? exit))) (str " · " (clip tail 30)))))))

(defn- section [label lines]
  (when (seq lines)
    (cons (str (paint :dim label) (subs "        " (count label)) (first lines))
          (map #(str "        " %) (rest lines)))))

(defn- capped [n more-hint xs]
  (if (> (count xs) n)
    (concat (take n xs) [(str "+" (- (count xs) n) " more — " more-hint)])
    xs))

(defn- capped-items
  "Like capped, for items that each take several lines: whole items only, as
  many as fit in n lines, and the hint counts items."
  [n more-hint groups]
  (if (<= (reduce + (map count groups)) n)
    (apply concat groups)
    (let [shown (->> (reductions + (map count groups))
                     (take-while #(< % n))
                     count)]
      (concat (apply concat (take shown groups))
              [(str "+" (- (count groups) shown) " more items — " more-hint)]))))

(defn- event-line [e]
  (str (clock (:t e)) " " (namespace (:type e)) "/" (name (:type e)) " "
       (clip (str/join " " (keep e [:drive/id :item/id :spawn/id :gate/id :note/id :ask/id :disposition :exit :intent :gate :command :text :question :answer :goal :why])) 70)))

(defn- render* [session-id events now]
  (let [{:keys [goal repo items notes spawns allow asks default-gate]} (project events)
        items (sort-by :order (vals items))
        {open true closed false} (group-by #(= :open (:state %)) items)
        ;; Pinned notes are the lasting ones; when there are any, recent notes give up room.
        pinned (filter :pinned notes)]
    (str/join
     "\n"
     (concat
      [(str "lume status · " session-id (when repo (str " · " repo)))]
      (section "goal" [(clip goal (- width 8))])
      ;; Questions waiting on the human come first: they're what blocks.
      (section "asks" (capped 3 "lume log"
                              (->> (vals asks)
                                   (filter #(= :open (:state %)))
                                   (sort-by #(parse-long (subs (:id %) 1)))
                                   (map #(paint :yellow (clip (str (:id %) " " (:question %)) (- width 8)))))))
      (when (or (seq allow) default-gate)
        (section "allow" [(clip (str/join " · " (cond->> allow default-gate (cons (str "default gate " default-gate))))
                                (- width 8))]))
      (section "pinned" (capped 6 "lume log"
                                (map #(clip (str (:note/id %) " " (:text %)) (- width 8)) pinned)))
      (section "open"
               (capped-items 10 "lume log"
                       (map (fn [{:keys [id intent] :as item}]
                                 (concat [(str id "  " (clip intent 80))]
                                         (when (:gate item)
                                           [(str "    " (gate-line now item))])
                                         (when-let [sp (get spawns (peek (:spawns item)))]
                                           [(str "    " (spawn-line now sp))])))
                               open)))
      (section "closed"
               (capped 4 "lume log"
                       (map (fn [{:keys [id state intent why]}]
                              (str id "  " (name state) " — " (clip intent 40) (when why (str " — " (clip why 40)))))
                            (reverse (sort-by :closed-at closed)))))
      (section "notes" (capped (if (seq pinned) 3 5) "lume log"
                               (map #(str "[" (clock (:t %)) " " (:by %) "] " (clip (:text %) 80))
                                    (reverse (remove :pinned notes)))))
      ;; Progress is shown on its spawn; in recent it would crowd out everything else.
      (section "recent" (map event-line (take-last 4 (remove #(= :spawn/progress (:type %)) events))))
      (section "next" ["lume item add \"<intent>\" [--gate \"<cmd>\"] · lume item gate <id> \"<cmd>\" · lume spawn <id>"
                       "lume item close <id> done|dropped \"<why>\" (done runs the gate) · lume note [--pin] \"<text>\""
                       "lume ask \"<question>\" · lume answer <id> \"<text>\" (human only) · lume allow \"<cmd>\" · lume log"])))))

(defn render
  "Status as text. Every section is capped, so it stays one screen however
  long the session runs: at most 40 lines with every section full. Plain
  unless {:color true}; colour adds escapes only, never visible width."
  ([session-id events now] (render session-id events now {}))
  ([session-id events now {:keys [color]}]
   (binding [*color* (boolean color)]
     (render* session-id events now))))
