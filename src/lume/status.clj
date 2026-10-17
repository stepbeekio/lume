(ns lume.status
  "lume status: a bounded, one-screen projection of the log.

  Deliberately naive. Whenever a driver wanted something this didn't show,
  that's a work item."
  (:require [clojure.string :as str])
  (:import (java.time Duration Instant ZoneId)
           (java.time.format DateTimeFormatter)))

;; Budgets bound what agents spend: dollars and minutes of spawns and drive
;; turns, and how many spawns started.
(defn- spend [s {:keys [cost-usd ms]}]
  (cond-> s
    cost-usd (update-in [:spent :usd] + cost-usd)
    ms (update-in [:spent :minutes] + (/ ms 60000.0))))

;; A measurement spec is its command and how it repeats; rows compare only
;; with rows of the same spec. One run, none dropped, is the default.
(defn- spec [{:keys [measure runs drop]}]
  {:measure measure :runs (or runs 1) :drop (or drop 0)})

(defn spec-text
  "The command, then how it repeats, then the numbers where higher is better,
  e.g. bb bench ×10 drop 1 higher score."
  [{:keys [measure higher] :as m}]
  (let [{:keys [runs drop]} (spec m)]
    (str measure (when (> runs 1) (str " ×" runs)) (when (pos? drop) (str " drop " drop))
         (when (seq higher) (str " higher " (str/join "," higher))))))

(defn direction-text
  "Which way is better, as the plateau ask says it."
  [higher]
  (if (seq higher)
    (str "higher is better for " (str/join "," higher) ", lower for the rest")
    "lower is better"))

;; Cadence: when drive hands control back. A driver may request up to :batch
;; spawns a turn (every-spawn is 1), run in order; :gate-fail ends a batch at a
;; failed gate; :plateau K asks the human after K rows without a new best. The
;; human's latest cadence is the ceiling for everyone else's.
;; A cadence event holds only the rules given (a spec may leave out :batch), so
;; the default fills in on read.
(def default-cadence {:batch 1})

(defn cadence [state] (merge default-cadence (:cadence state)))

(defn cadence-ceiling [state] (merge default-cadence (:cadence-ceiling state)))

(defn cadence-text
  "The rules as lume cadence takes them, e.g. batch 3 on gate-fail on plateau 4."
  [{:keys [batch gate-fail plateau]}]
  (str/join " " (cond-> [(if (= 1 batch) "every-spawn" (str "batch " batch))]
                  gate-fail (conj "on gate-fail")
                  plateau (conj (str "on plateau " plateau)))))

(defn cadence-raises
  "How cadence c hands back less often than the ceiling allows, as text, or nil."
  [ceiling c]
  (not-empty
   (str/join ", " (remove nil? [(when (> (:batch c) (:batch ceiling))
                                  (str "batch " (:batch ceiling) " → " (:batch c)))
                                (when (and (:gate-fail ceiling) (not (:gate-fail c)))
                                  "drops on gate-fail")
                                (when (and (:plateau ceiling) (> (or (:plateau c) Long/MAX_VALUE) (:plateau ceiling)))
                                  (str "plateau " (:plateau ceiling) " → " (or (:plateau c) "none")))]))))

;; A plateau is counted per item and measurement spec: rows since the last one
;; that set a new best. Lower is better, except for the row's :higher names. A
;; failed row, or one without numbers, is no evidence and leaves the count be. A
;; change of direction, or of the numbers' names (a misparse, say), starts over.
(defn- plateau [{:keys [best stale best-id] :as p} {:keys [exit values higher] mid :measure/id}]
  (let [higher (set higher)
        best (when (and (= higher (:higher p #{})) (= (set (keys values)) (set (keys best)))) best)
        better? (fn [k v b] (if (higher k) (> v b) (< v b)))]
    (cond
      (or (not (zero? exit)) (empty? values)) p
      (or (nil? best) (some (fn [[k v]] (better? k v (get best k))) values))
      {:best (reduce-kv (fn [b k v] (cond-> b (or (nil? (get b k)) (better? k v (get b k))) (assoc k v)))
                        (or best {}) values)
       :stale 0 :best-id mid :higher higher :counted mid}
      :else {:best best :stale (inc (or stale 0)) :best-id best-id :higher higher :counted mid})))

(defn amount [k v]
  (let [n (str/replace (format "%.1f" (double v)) #"\.0$" "")]
    (case k :usd (format "$%.2f" (double v)) :spawns (str (long v)) n)))

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
       :item/measuring  (update-in s [:items (:item/id e)]
                                   #(merge (dissoc % :runs :drop :higher) (select-keys e [:measure :runs :drop :higher])))
       ;; Each row carries the row it's compared with: the latest earlier one
       ;; of the same spec (command, runs and drop) that passed with numbers
       ;; and shares a number's name with it, so a misparsed row isn't the
       ;; baseline for a good one. One good row is kept per set of names.
       :measure/ran     (update-in s [:items (:item/id e)]
                                   (fn [item]
                                     (let [names #(set (keys (:values %)))
                                           base (last (filter #(some (names %) (keys (:values e)))
                                                              (get-in item [:good-measures (spec e)])))
                                           p (plateau (get-in item [:plateaus (spec e)]) e)]
                                       (cond-> (-> item
                                                   (assoc :last-measure (cond-> e base (assoc :base base)) :plateau p)
                                                   (assoc-in [:plateaus (spec e)] p))
                                         (and (zero? (:exit e)) (seq (:values e)))
                                         (update-in [:good-measures (spec e)]
                                                    (fn [rows] (conj (filterv #(not= (names %) (names e)) rows) e)))))))
       :item/closed     (update-in s [:items (:item/id e)] assoc
                                   :state (:disposition e) :why (:why e) :closed-at (:t e))
       :note/added      (update s :notes conj e)
       :ask/opened      (update s :asks assoc (:ask/id e) (cond-> {:id (:ask/id e) :question (:question e) :state :open}
                                                            (:item/id e) (assoc :item (:item/id e))))
       ;; An answer is steering, like a note, so it joins the notes.
       :ask/answered    (-> s
                            (assoc-in [:asks (:ask/id e) :state] :answered)
                            (update :notes conj (assoc (select-keys e [:ask/id :answer :t :by])
                                                       :question (get-in s [:asks (:ask/id e) :question])
                                                       :text (str (:ask/id e) " → " (:answer e)))))
       :note/unpinned  (update s :notes (partial mapv #(cond-> % (= (:note/id e) (:note/id %)) (dissoc :pinned))))
       :session/budgeted (update s :budget merge (select-keys e [:usd :spawns :minutes]))
       :session/cadence (let [c (select-keys e [:batch :gate-fail :plateau])]
                          (cond-> (assoc s :cadence c)
                            (= "human" (:by e)) (assoc :cadence-ceiling c)))
       :spawn/started   (-> s
                            (assoc-in [:spawns (:spawn/id e)] e)
                            (update-in [:items (:item/id e) :spawns] (fnil conj []) (:spawn/id e))
                            (update-in [:spent :spawns] inc))
       :spawn/progress  (assoc-in s [:spawns (:spawn/id e) :progress] e)
       :spawn/exited    (-> s (update-in [:spawns (:spawn/id e)] merge e) (spend e))
       :drive/exited    (spend s e)
       s))
   {:items {} :notes [] :spawns {} :allow [] :asks {} :spent {:usd 0 :spawns 0 :minutes 0}}
   events))

(defn over-budget
  "Why no more agent work may start, or nil: the first limit that spending has
  reached. Spend is the whole session's, spawns and drive turns alike."
  [{:keys [budget spent]}]
  (some (fn [[k unit]]
          (when-let [limit (get budget k)]
            (when (>= (get spent k) limit)
              (str "budget reached: " (amount k (get spent k)) " of " (amount k limit) unit
                   " — only the human raises it, with lume budget --" (name k) " N"))))
        [[:usd " spent"] [:spawns " spawns"] [:minutes " minutes"]]))

(defn budget-text
  "Spent against each limit, e.g. $1.20/$10.00 · 7/20 spawns · 12.5 min."
  [{:keys [budget spent]}]
  (str/join " · " (for [[k unit] [[:usd ""] [:spawns " spawns"] [:minutes " min"]]]
                    (str (amount k (get spent k)) (when-let [l (get budget k)] (str "/" (amount k l))) unit))))

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

(defn secs [ms]
  (format "%.1fs" (/ ms 1000.0)))

(defn num-text
  "A measured number for reading: whole numbers as they are, others to 3
  significant digits, never rounding away a digit before the point."
  [v]
  (if (or (not (number? v)) (integer? v) (Double/isNaN v) (Double/isInfinite v))
    (str v)
    (let [d (bigdec v)]
      (-> d (.round (java.math.MathContext. (max 3 (- (.precision d) (.scale d))))) .stripTrailingZeros .toPlainString))))

(defn- change [v b]
  (when (and (number? v) (number? b) (not= v b))
    (let [d (num-text (- v b))]
      (str "(" (when-not (str/starts-with? d "-") "+") d ")"))))

(defn measure-text
  "A ledger row's numbers: name=value pairs (name↑ where higher is better),
  each with its change against the row's :base when it has one, then wall
  time, then a failure, then which row it was compared with (unless :vs is
  false)."
  [{:keys [values spread ms exit base runs drop failed-run higher] :or {drop 0}} & {:keys [vs] :or {vs true}}]
  (str (if (seq values)
         (str/join " " (map (fn [[k v]]
                              (str k (when (some #{k} higher) "↑") "=" (num-text v) (change v (get (:values base) k))
                                   (when-let [[lo hi] (get spread k)]
                                     (when (not= lo hi) (str "[" (num-text lo) ".." (num-text hi) "]")))))
                            (sort values)))
         "no numbers")
       " · " (secs ms)
       (when (and runs (not failed-run)) (str " · " (if (pos? drop) (str (- runs drop) " of " runs) runs) " runs"))
       (when-not (zero? exit) (str " · fail exit " exit (when failed-run (str " in run " failed-run " of " runs))))
       (when (and vs base) (str " · vs " (:measure/id base)))))

(defn env-text
  "Where a row was taken, e.g. on asahi load 0.21 at a3d907a/msslzuos."
  [{:keys [host load commit change]}]
  (str "on " host (when load (str " load " load))
       (when (or commit change) (str " at " (str/join "/" (remove nil? [commit change]))))))

;; The latest row only; lume item show has the rest.
(defn- measure-line [now {:keys [measure runs drop last-measure]}]
  (let [repeats (spec-text {:runs runs :drop drop})]
    (str "measure " (clip measure (- 18 (count repeats))) repeats
         (when-let [{:keys [t exit base] mid :measure/id :as m} last-measure]
           (str " · " mid (when base (str " vs " (:measure/id base))) " "
                (when-not (zero? exit) (str (paint :red (str "exit " exit)) " · "))
                (clip (measure-text (-> m (assoc :exit 0) (dissoc :runs)) :vs false) 36) " · " (ago now t))))))

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
  many as fit in n lines, and the hint counts them as nouns."
  [n nouns more-hint groups]
  (if (<= (reduce + (map count groups)) n)
    (apply concat groups)
    (let [shown (->> (reductions + (map count groups))
                     (take-while #(< % n))
                     count)]
      (concat (apply concat (take shown groups))
              [(str "+" (- (count groups) shown) " more " nouns " — " more-hint)]))))

(defn- event-line [e]
  (str (clock (:t e)) " " (namespace (:type e)) "/" (name (:type e)) " "
       (clip (str/join " " (keep e [:drive/id :item/id :spawn/id :gate/id :measure/id :note/id :ask/id :disposition :exit :intent :gate :measure :command :text :question :answer :goal :why])) 70)))

(defn- render* [session-id events now past]
  (let [{:keys [goal repo items notes spawns allow asks default-gate budget spent] :as state} (project events)
        cadence-line (when (:cadence state)
                       (str "cadence " (cadence-text (cadence state))
                            (when (not= (cadence state) (cadence-ceiling state))
                              (str " (ceiling " (cadence-text (cadence-ceiling state)) ")"))))
        items (sort-by :order (vals items))
        {open true closed false} (group-by #(= :open (:state %)) items)
        ;; Pinned notes are the lasting ones; when there are any, recent notes give up room.
        pinned (filter :pinned notes)
        ;; Questions waiting on the human come first: they're what blocks. Each
        ;; wraps to 3 lines before it's clipped; a clipped one says where the rest is.
        ask-lines (capped-items 7 "asks" "lume ask"
                                (->> (vals asks)
                                     (filter #(= :open (:state %)))
                                     (sort-by #(parse-long (subs (:id %) 1)))
                                     (map (fn [{:keys [id question]}]
                                            (let [indent (apply str (repeat (inc (count id)) " "))
                                                  [l & more] (wrap question (- width 8 (count indent)) 3
                                                                   (str " — lume ask show " id))]
                                              (map #(paint :yellow %)
                                                   (cons (str id " " l) (map #(str indent %) more))))))))
        ;; Asks past 4 lines take their room from the pinned notes, so status stays one screen.
        pinned-cap (- 6 (max 0 (- (count ask-lines) 4)))]
    (str/join
     "\n"
     (concat
      [(str "lume status · " session-id (when past " (not current)") (when repo (str " · " repo)))]
      (section "goal" [(clip goal (- width 8))])
      (section "asks" ask-lines)
      (when (or (seq allow) default-gate cadence-line)
        (section "allow" [(clip (str/join " · " (cond->> allow
                                                  default-gate (cons (str "default gate " default-gate))
                                                  cadence-line (cons cadence-line)))
                                (- width 8))]))
      (when (or (seq budget) (some pos? (vals spent)))
        (section "budget" [(let [over (over-budget state)
                                 line (clip (str (budget-text state) (when over " · reached: no new spawns or turns"))
                                            (- width 8))]
                             (if over (paint :red line) line))]))
      (section "pinned" (capped pinned-cap "lume log"
                                (map #(clip (str (:note/id %) " " (:text %)) (- width 8)) pinned)))
      (section "open"
               (capped-items 10 "items" "lume log"
                            (map (fn [{:keys [id intent] :as item}]
                                   (concat [(str id "  " (clip intent 80))]
                                           (when (:gate item)
                                             [(str "    " (gate-line now item))])
                                           (when (:measure item)
                                             [(str "    " (measure-line now item))])
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
      (section "recent" (map event-line (take-last 3 (remove #(= :spawn/progress (:type %)) events))))
      (section "next" ["lume item add \"<intent>\" [--gate \"<cmd>\"] · lume item gate <id> \"<cmd>\" · lume spawn <id>"
                       "lume item close <id> done|dropped \"<why>\" (done runs the gate) · lume note [--pin] \"<text>\""
                       "lume ask \"<question>\" · lume ask (read open asks) · lume answer <id> (human only) · lume log"])))))

(defn render
  "Status as text. Every section is capped, so it stays one screen however
  long the session runs: at most 40 lines with every section full. Plain
  unless {:color true}; colour adds escapes only, never visible width.
  {:past true} marks a session that isn't the repo's current one."
  ([session-id events now] (render session-id events now {}))
  ([session-id events now {:keys [color past]}]
   (binding [*color* (boolean color)]
     (render* session-id events now past))))

;; lume ls: a session as one line. The outcome is what the session is doing or
;; how it ended, most pressing first.
(defn- outcome [events {:keys [items spawns asks]}]
  (let [drive (last (filter #(#{:drive/started :drive/stopped} (:type %)) events))
        running (->> (vals spawns) (remove #(or (:interrupted %) (contains? % :exit))) (sort-by :t) last)
        ask (->> (vals asks) (filter #(= :open (:state %))) (sort-by #(parse-long (subs (:id %) 1))) first)]
    (cond (= :drive/started (:type drive)) "driving"
          running (str "running " (:spawn/id running))
          ask (str "ask " (:id ask) " open")
          (and (seq items) (not-any? #(= :open (:state %)) (vals items))) "all closed"
          drive (str "stopped: " (:why drive))
          (empty? items) "no items"
          :else "idle")))

(defn session-line
  "One session for lume ls: id (* if current), goal, items, last event, outcome."
  [session-id events now current?]
  (let [{:keys [goal items] :as state} (project events)
        open (count (filter #(= :open (:state %)) (vals items)))]
    (str (if current? "* " "  ")
         (clip (str session-id " " (clip (or goal "(no goal)") 40)
                    " · " open " open " (- (count items) open) " closed"
                    (when-let [t (:t (last events))] (str " · " (ago now t)))
                    " · " (outcome events state))
               (- width 2)))))
