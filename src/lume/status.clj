(ns lume.status
  "lume status: a bounded, one-screen projection of the log. The frame; the
  sections in it are the features' :status (lume.kernel).

  Deliberately naive. Whenever a driver wanted something this didn't show,
  that's a work item."
  (:require [clojure.string :as str]
            [lume.kernel :as kernel :refer [clock paint]]))

;; Rendering

(def width "Status's width: no line is longer." 100)

(defn strip-color [s]
  (str/replace s #"\u001b\[[0-9;]*m" ""))

(defn clip [s n]
  (let [s (str/replace (str s) #"\s+" " ")]
    (if (> (count s) n) (str (subs s 0 (dec n)) "…") s)))

(defn wrap
  "Lines of at most n, broken at spaces (a word longer than n is cut). With
  max-lines, the last line kept is clipped and ends in tail when there is more."
  ([s n] (wrap s n nil nil))
  ([s n max-lines tail]
   (let [lines (loop [words (str/split (str/trim (str/replace (str s) #"\s+" " ")) #" ")
                      line nil, out []]
                 (if-let [[w & more] (seq words)]
                   (cond (nil? line) (if (> (count w) n)
                                       (recur (cons (subs w n) more) nil (conj out (subs w 0 n)))
                                       (recur more w out))
                         (<= (+ (count line) 1 (count w)) n) (recur more (str line " " w) out)
                         :else (recur words nil (conj out line)))
                   (cond-> out line (conj line))))]
     (if (and max-lines (> (count lines) max-lines))
       (let [last-n (- n (count tail))]
         (conj (vec (take (dec max-lines) lines))
               (str (clip (str/join " " (drop (dec max-lines) lines)) last-n) tail)))
       lines))))

;; Replay: a decision is a drive turn or a spawn, and ends with its exit event.
(defn decision? [e] (#{:drive/exited :spawn/exited} (:type e)))

(defn decision-text
  "What a decision did, in one line: its id, exit, then its result."
  [e]
  (str (or (:drive/id e) (:spawn/id e)) " " (if (:interrupted e) "interrupted" (str "exit " (:exit e)))
       (when-let [r (:result e)] (str " · " r))))

(defn tree
  "Items in order, each followed by its children (:parent), as [item depth]
  pairs. An item whose parent isn't among them is a root."
  [items]
  (let [ids (set (map :id items))
        kids (group-by #(when (ids (:parent %)) (:parent %)) (sort-by :order items))
        walk (fn walk [depth item]
               (cons [item depth] (mapcat (partial walk (inc depth)) (kids (:id item)))))]
    (mapcat (partial walk 0) (kids nil))))

;; The built-in features are listed in lume.features, which requires every
;; feature, this namespace among them; so it is resolved when first needed.
(defn- built-in [] @(requiring-resolve 'lume.features/all))

(defn project
  "Fold the log into the current state of the session (kernel/project), with
  the built-in features when none are bound: a lume repl evaluates on nREPL's
  threads, outside the run's binding."
  [events]
  (binding [kernel/*features* (or kernel/*features* (built-in))]
    (kernel/project events)))

;; The frame. Each feature's :status sections are
;;   {:label "open" :order 60      where it sits, lowest first
;;    :cap 10                      most lines (0 hides it), or (fn [state shown])
;;                                 with shown {label lines} of the sections above it
;;    :more "items — lume log"     after "+N more " when cut (default "— lume log")
;;    :lines (fn [state events now] lines)}
;; where a line is a string, or a seq of strings kept whole (an item and the
;; lines under it). The frame cuts each section to its cap and clips every
;; line to the width, so a section can't push status past one screen.

(def ^:private default-cap 5)

(defn- cut [n more lines]
  (if (some sequential? lines)
    (let [groups (map #(if (sequential? %) % [%]) lines)]
      (if (<= (reduce + (map count groups)) n)
        (apply concat groups)
        (let [shown (count (take-while #(< % n) (reductions + (map count groups))))]
          (concat (apply concat (take shown groups))
                  [(str "+" (- (count groups) shown) " more " more)]))))
    (if (> (count lines) n)
      (concat (take n lines) [(str "+" (- (count lines) n) " more " more)])
      lines)))

;; A line that's too wide loses its colour with its tail: escapes can't be cut safely.
(defn- fit [n s]
  (let [plain (strip-color (str s))
        [_ indent text] (re-matches #"(?s)( *)(.*)" plain)]
    (if (> (count plain) n) (str indent (clip text (- n (count indent)))) (str s))))

(defn- section [label lines]
  (when (seq lines)
    (let [label (clip label 7)]
      (cons (str (paint :dim label) (subs "        " (count label)) (first lines))
            (map #(str "        " %) (rest lines))))))

(defn- sections [state events now]
  (:out (reduce (fn [{:keys [shown] :as acc} {:keys [label cap more lines]}]
                  (let [cap (cond (fn? cap) (cap state shown) cap cap :else default-cap)
                        ls (when (pos? cap)
                             (map #(fit (- width 8) %) (cut cap (or more "— lume log") (lines state events now))))]
                    (-> acc
                        (update-in [:shown label] (fnil + 0) (count ls))
                        (update :out into (section label ls)))))
                {:shown {} :out []}
                (sort-by :order (mapcat :status (kernel/features))))))

(defn- render* [session-id events now past replay]
  (let [{:keys [repo] :as state} (kernel/project events)]
    (str/join
     "\n"
     (concat
      [(if-let [{:keys [of]} replay]
         (str "lume replay · " session-id " · event " (count events) " of " of
              (when-let [t (:t (last events))] (str " · " (clock t))))
         (str "lume status · " session-id (when past " (not current)") (when repo (str " · " repo))))]
      (sections state events now)
      ;; In a replay the commands to run next don't apply; what the decision just
      ;; ending did takes their room.
      (if replay
        (when (decision? (last events))
          (section "decided" (wrap (decision-text (last events)) (- width 8) 3 " — lume log")))
        (section "next" ["lume item add \"<intent>\" [--gate \"<cmd>\"] · lume item gate <id> \"<cmd>\" · lume spawn <id>"
                         "lume item close <id> done|dropped \"<why>\" (done runs the gate) · lume note [--pin] \"<text>\""
                         "lume ask \"<question>\" · lume ask (read open asks) · lume answer <id> (human only) · lume log"]))))))

(defn render
  "Status as text: a header, the bound features' :status sections, then the
  commands to run next. Every section is capped, so it stays one screen
  however long the session runs: at most 40 lines with every section full,
  and 4 more from extensions (lume.ext).
  Plain unless {:color true}; colour adds escapes only, never visible width.
  {:past true} marks a session that isn't the repo's current one.
  {:replay {:of M}} renders the session as it stood after these events, the
  first N of M: the header says so, and the decision they end with replaces
  the next commands."
  ([session-id events now] (render session-id events now {}))
  ([session-id events now {:keys [color past replay]}]
   (binding [kernel/*color* (boolean color)
             kernel/*features* (or kernel/*features* (built-in))]
     (render* session-id events now past replay))))

(defn prompt-lines
  "What the bound features add to a worker's prompt for item: each :prompt
  (fn [state item] lines), in feature order."
  [state item]
  (mapcat #((:prompt %) state item) (filter :prompt (kernel/features))))
