(ns lume.ask
  "Questions for the human: lume ask, answer and ask show. Anyone may ask;
  only the human answers, and the answer reaches status and workers alongside
  the notes, and LUME_NTFY (lume.ntfy reacts to :ask/opened)."
  (:require [clojure.string :as str]
            [lume.kernel :as kernel :refer [*read-line* actor columns fail! human-only! next-id paint width wrap]]))

(defn open-asks
  "The open asks in state, oldest first."
  [{:keys [asks]}]
  (->> (vals asks)
       (filter #(= :open (:state %)))
       (sort-by #(parse-long (subs (:id %) 1)))))

(defn ask-item
  "The item an ask (as folded into state) concerns: a plateau's, or the
  asking worker's."
  [events {:keys [item by]}]
  (or item
      (some #(when (and (= :spawn/started (:type %)) (= by (:spawn/id %))) (:item/id %)) events)))

(defn opened-event
  "The :ask/opened event for question, with the next ask id; extra adds keys
  (a plateau's :item/id and :measure/id)."
  [events question & {:as extra}]
  (merge {:type :ask/opened :ask/id (next-id "a" :ask/opened events) :question question} extra))

(defn answer-why
  "Why ask id in state can't be answered, or nil when it is open."
  [state id]
  (case (get-in state [:asks id :state])
    nil (str "no such ask: " id)
    :answered (str id " is already answered")
    :open nil))

(defn answered-event
  "The :ask/answered event giving ask id the answer text, or nil when the ask
  isn't open (answer-why says why); extra adds keys (:via)."
  [events id text & {:as extra}]
  (when-not (answer-why (kernel/project events) id)
    (merge {:type :ask/answered :ask/id id :answer text} extra)))

;; With no question: what status clips, for the human to read and answer. Open
;; asks in full, oldest first, wrapped to the terminal.
(defn- open-asks-text [session width]
  (let [events (kernel/read-events session)
        open (open-asks (kernel/project events))]
    (if (empty? open)
      "no open asks"
      (str/join "\n\n"
                (for [{:keys [id question t by measure] :as a} open
                      :let [item (ask-item events a)]]
                  (str/join "\n"
                            (concat
                             [(str id " · asked " (kernel/stamp t) " by " by
                                   (when item (str " · item " item (when measure (str " at " measure)))))]
                             (map #(str "  " %) (wrap question (- width 2)))
                             [(str "  → lume answer " id "   (or lume answer " id " \"<text>\")")])))))))

(def ^:private ask-usage "usage: lume ask [\"<question>\"]")

(defn ask [session [question & extra]]
  (cond (and (nil? question) (nil? extra)) (open-asks-text session (columns))
        (or (str/blank? question) (seq extra)) (fail! ask-usage)
        :else (:ask/id (kernel/append! session #(opened-event % question)))))

;; With no text, at a terminal, the human reads the question in full and types
;; the answer at the prompt; the typed-id confirmation still follows.
(def ^:private answer-usage "usage: lume answer <id> [\"<text>\"]")

(defn- answer-args
  "answer's args as [id text-or-nil], or a usage error."
  [args]
  (let [[[id text & extra]] (kernel/take-flags args #{} answer-usage)]
    (when (or (str/blank? id) (seq extra)) (fail! answer-usage))
    [id text]))

(defn- typed-answer!
  "The answer the human types after reading ask id in full, or a refusal."
  [session id]
  (let [state (kernel/state session)
        question (get-in state [:asks id :question])]
    (some-> (answer-why state id) fail!)
    (or (some-> (*read-line* (str/join "\n" (cons (str id ":") (map #(str "  " %) (wrap question (- (columns) 2)))))
                             "Answer: ")
                str/trim)
        (kernel/unconfirmed!))))

(defn answer [session args]
  (let [[id text] (answer-args args)]
    (when-not (= "human" (actor)) (fail! "only the human answers asks; record what you think with lume note"))
    (let [text (or text (typed-answer! session id))]
      (when (str/blank? text) (fail! answer-usage))
      (human-only! "only the human answers asks; record what you think with lume note"
                   (str "Answer " id ": " text) id)
      (kernel/append! session (fn [events]
                                (or (answered-event events id text)
                                    (fail! (answer-why (kernel/project events) id)))))
      (str id " answered"))))

;; Ask show: what status clips. The question in full, who asked and when, the
;; item it concerns (a plateau's, or the asking worker's), and the answer.
(def ^:private ask-show-usage "usage: lume ask show <id>")

(defn ask-show [session args]
  (let [[id] (kernel/positionals args 1 ask-show-usage)
        events (kernel/read-events session)
        opened (or (get-in (kernel/project events) [:asks id])
                   (fail! "no such ask:" id))
        answered (kernel/find-event events :ask/answered :ask/id id)
        item (ask-item events opened)]
    (str/join "\n"
              (concat
               [(str id " " (if answered "answered" "open"))
                (str "question " (:question opened))
                (str "asked    " (kernel/stamp-by opened))]
               (when item [(str "item     " item (when (:measure opened) (str " at " (:measure opened))))])
               (if answered
                 [(str "answer   " (:answer answered))
                  (str "answered " (kernel/stamp-by answered) (when-let [via (:via answered)] (str " via " (name via))))]
                 [(str "answer   none yet; only the human answers: lume answer " id)])))))

;; Questions waiting on the human come first in status: they're what blocks.
;; Each wraps to 3 lines before it's clipped; a clipped one says where the rest is.
(defn- ask-lines [state _ _]
  (->> (open-asks state)
       (map (fn [{:keys [id question]}]
              (let [indent (apply str (repeat (inc (count id)) " "))
                    [l & more] (wrap question (- width 8 (count indent)) 3
                                     (str " — lume ask show " id))]
                (map #(paint :yellow %)
                     (cons (str id " " l) (map #(str indent %) more))))))))

;; The fold keeps who asked and when, and a plateau's item and row, so readers
;; don't go back to the log for the opened event.
(defn- opened-ask [s {id :ask/id :keys [question t by] item :item/id mid :measure/id}]
  (assoc-in s [:asks id] (cond-> {:id id :question question :state :open :t t :by by}
                           item (assoc :item item)
                           mid (assoc :measure mid))))

(def feature
  {:name "ask"
   :events {:ask/opened {:req #{:ask/id :question}}
            :ask/answered {:req #{:ask/id :answer} :human true}}
   :init {:asks {}}
   :fold {:ask/opened opened-ask
          ;; The note feature folds the answer into the notes too.
          :ask/answered (fn [s e] (assoc-in s [:asks (:ask/id e) :state] :answered))}
   :status [{:label "asks" :order 20 :cap 7 :more "asks — lume ask" :lines ask-lines}]
   ;; ask show needs an id: with anything else, show is the text.
   :commands [{:path ["ask"] :needs :session :usage ask-usage
               :run (fn [{:keys [session]} args] (ask session args))
               :help "lume ask \"<question>\"                       put a question to the human; open asks top status
lume ask                                    open asks in full, wrapped to the terminal, each with its answer command"}
              {:path ["ask" "show"] :needs :session :usage ask-show-usage
               :run (fn [{:keys [session]} args] (if (= 1 (count args)) (ask-show session args) (ask session (cons "show" args))))
               :help "lume ask show <id>                          one ask in full: question, who asked and when, its item, its answer"}
              {:path ["answer"] :needs :session :usage answer-usage
               :run (fn [{:keys [session]} args] (answer session args))
               :help "lume answer <id> [\"<text>\"]                 answer an ask (human at a tty: retype the id); no text: prompt for it
                                            after the full question. Answers join the notes"}]})
