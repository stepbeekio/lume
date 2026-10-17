(ns lume.status
  "lume status: a bounded, one-screen projection of the log.

  Deliberately naive. Whenever a driver wanted something this didn't show,
  that's a work item."
  (:require [clojure.string :as str]
            [lume.kernel :as kernel :refer [ago clock paint]]
            [lume.measure :as measure]
            [lume.note :as note]
            [lume.ntfy :as ntfy]
            [lume.policy :as policy :refer [cadence cadence-ceiling cadence-text]]))

;; Rendering

(def ^:private width 100)

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

(defn- event-line [e]
  (str (clock (:t e)) " " (namespace (:type e)) "/" (name (:type e)) " "
       (clip (str/join " " (keep e [:drive/id :item/id :spawn/id :gate/id :measure/id :note/id :ask/id :disposition :exit :port :intent :gate :measure :command :text :question :answer :goal :why])) 70)))

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

;; Status sections, for the features below (see render for the frame).

(defn- allow-lines [{:keys [allow default-gate] :as state} _ _]
  (let [cadence-line (when (:cadence state)
                       (str "cadence " (cadence-text (cadence state))
                            (when (not= (cadence state) (cadence-ceiling state))
                              (str " (ceiling " (cadence-text (cadence-ceiling state)) ")"))))]
    (when (or (seq allow) default-gate cadence-line)
      [(str/join " · " (cond->> allow
                         default-gate (cons (str "default gate " default-gate))
                         cadence-line (cons cadence-line)))])))

;; Questions waiting on the human come first: they're what blocks. Each wraps
;; to 3 lines before it's clipped; a clipped one says where the rest is.
(defn- ask-lines [{:keys [asks]} _ _]
  (->> (vals asks)
       (filter #(= :open (:state %)))
       (sort-by #(parse-long (subs (:id %) 1)))
       (map (fn [{:keys [id question]}]
              (let [indent (apply str (repeat (inc (count id)) " "))
                    [l & more] (wrap question (- width 8 (count indent)) 3
                                     (str " — lume ask show " id))]
                (map #(paint :yellow %)
                     (cons (str id " " l) (map #(str indent %) more))))))))

;; An open item is a group: its line, then each feature's :item-lines under it.
(defn- open-lines [{:keys [items] :as state} _ now]
  (let [under (keep :item-lines (kernel/features))]
    (map (fn [[{:keys [id intent] :as item} depth]]
           (let [in (apply str (repeat (* 2 depth) " "))]
             (map #(str in %)
                  (cons (str id "  " (clip intent (- 80 (count in))))
                        (map #(str "    " %) (mapcat #(% state item now) under))))))
         (tree (filter #(= :open (:state %)) (vals items))))))

(defn- closed-lines [{:keys [items]} _ _]
  (map (fn [{:keys [id state intent why]}]
         (str id "  " (name state) " — " (clip intent 40) (when why (str " — " (clip why 40)))))
       (reverse (sort-by :closed-at (sort-by :order (remove #(= :open (:state %)) (vals items)))))))

;; The built-in features (see lume.kernel): the events each appends and how
;; they fold into the state status renders, the status sections they add,
;; the lines under each open item and what they add to a worker's prompt.
;; Several features may fold one type; they run in the order listed.

(def session-feature
  {:name "session"
   :events {:session/started {:req #{:goal :repo}}
            :session/gated {:req #{:gate}}
            :session/allowed {:req #{:command} :human true}
            :session/disallowed {:req #{:command} :human true}}
   :init {:allow []}
   :fold {:session/started (fn [s e] (assoc s :goal (:goal e) :repo (:repo e)))
          :session/gated (fn [s e] (assoc s :default-gate (:gate e)))
          :session/allowed (fn [s e] (update s :allow #(vec (distinct (conj % (:command e))))))
          :session/disallowed (fn [s e] (update s :allow #(vec (remove #{(:command e)} %))))}
   :status [{:label "goal" :order 10 :cap 1 :lines (fn [s _ _] [(str (:goal s))])}
            {:label "allow" :order 30 :cap 1 :lines allow-lines}]})

(def item-feature
  {:name "item"
   :events {:item/added {:req #{:item/id :intent}}
            :item/gated {:req #{:item/id :gate}}
            :item/closed {:req #{:item/id :disposition}}
            :gate/ran {:req #{:gate/id :item/id :gate :exit :out}}}
   :init {:items {}}
   :fold {:item/added (fn [s e]
                        (update s :items assoc (:item/id e)
                                (cond-> {:id (:item/id e) :intent (:intent e) :state :open :order (count (:items s))}
                                  (:gate e) (assoc :gate (:gate e))
                                  (:parent e) (assoc :parent (:parent e)))))
          :item/gated (fn [s e] (assoc-in s [:items (:item/id e) :gate] (:gate e)))
          :gate/ran (fn [s e] (assoc-in s [:items (:item/id e) :last-gate] e))
          :item/closed (fn [s e] (update-in s [:items (:item/id e)] assoc
                                            :state (:disposition e) :why (:why e) :closed-at (:t e)))}
   :status [{:label "open" :order 60 :cap 10 :more "items — lume log" :lines open-lines}
            {:label "closed" :order 70 :cap 4 :lines closed-lines}]
   :item-lines (fn [_ item now] (when (:gate item) [(gate-line now item)]))})

(def ask-feature
  {:name "ask"
   :events {:ask/opened {:req #{:ask/id :question}}
            :ask/answered {:req #{:ask/id :answer} :human true}}
   :init {:asks {}}
   :fold {:ask/opened (fn [s e]
                        (update s :asks assoc (:ask/id e) (cond-> {:id (:ask/id e) :question (:question e) :state :open}
                                                            (:item/id e) (assoc :item (:item/id e)))))
          ;; An answer is steering, like a note, so it joins the notes (and
          ;; reaches status and the worker prompt with them).
          :ask/answered (fn [s e]
                          (-> s
                              (assoc-in [:asks (:ask/id e) :state] :answered)
                              (update :notes conj (assoc (select-keys e [:ask/id :answer :t :by])
                                                         :question (get-in s [:asks (:ask/id e) :question])
                                                         :text (str (:ask/id e) " → " (:answer e))))))}
   :status [{:label "asks" :order 20 :cap 7 :more "asks — lume ask" :lines ask-lines}]})

(def spawn-feature
  {:name "spawn"
   :events {:spawn/started {:req #{:spawn/id :item/id :agent :out}}
            :spawn/progress {:req #{:spawn/id}}
            :spawn/exited {:req #{:spawn/id}}
            :spawn/permission {:req #{:spawn/id :disposition}}}
   :init {:spawns {}}
   :fold {:spawn/started (fn [s e]
                           (-> s
                               (assoc-in [:spawns (:spawn/id e)] e)
                               (update-in [:items (:item/id e) :spawns] (fnil conj []) (:spawn/id e))))
          :spawn/progress (fn [s e] (assoc-in s [:spawns (:spawn/id e) :progress] e))
          :spawn/exited (fn [s e] (update-in s [:spawns (:spawn/id e)] merge e))}
   :item-lines (fn [s item now] (when-let [sp (get (:spawns s) (peek (:spawns item)))] [(spawn-line now sp)]))})

(def inspect-feature
  {:name "inspect"
   :events {:repl/opened {:req #{:port}}}
   ;; Progress is shown on its spawn; in recent it would crowd out everything else.
   :status [{:label "recent" :order 90 :cap 3
             :lines (fn [_ events _] (map event-line (take-last 3 (remove #(= :spawn/progress (:type %)) events))))}]})

(def features
  "The built-in features, in fold order, but for lume.drive, which renders
  status for its prompt and so requires this namespace: lume.cli adds it, and
  binds kernel/*features* to them all. drive folds nothing, so a projection
  here (the unbound fallback below) is the same without it."
  [session-feature item-feature ask-feature note/feature measure/feature spawn-feature
   policy/budget-feature policy/cadence-feature ntfy/feature inspect-feature])

(defn project
  "Fold the log into the current state of the session (kernel/project), with
  the built-in features when none are bound: a lume repl evaluates on nREPL's
  threads, outside the run's binding."
  [events]
  (binding [kernel/*features* (or kernel/*features* features)]
    (kernel/project events)))

;; The frame. Each feature's :status sections are
;;   {:label "open" :order 60      where it sits, lowest first
;;    :cap 10                      most lines, or (fn [state shown]) with shown
;;                                 {label lines} of the sections above it
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
                        ls (map #(fit (- width 8) %) (cut cap (or more "— lume log") (lines state events now)))]
                    (-> acc
                        (assoc-in [:shown label] (count ls))
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
  however long the session runs: at most 40 lines with every section full.
  Plain unless {:color true}; colour adds escapes only, never visible width.
  {:past true} marks a session that isn't the repo's current one.
  {:replay {:of M}} renders the session as it stood after these events, the
  first N of M: the header says so, and the decision they end with replaces
  the next commands."
  ([session-id events now] (render session-id events now {}))
  ([session-id events now {:keys [color past replay]}]
   (binding [kernel/*color* (boolean color)
             kernel/*features* (or kernel/*features* features)]
     (render* session-id events now past replay))))

(defn prompt-lines
  "What the bound features add to a worker's prompt for item: each :prompt
  (fn [state item] lines), in feature order."
  [state item]
  (mapcat #((:prompt %) state item) (filter :prompt (kernel/features))))

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
