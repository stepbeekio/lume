(ns lume.ask
  "Questions for the human: lume ask, answer and ask show. Anyone may ask;
  only the human answers, and the answer reaches status and workers alongside
  the notes, and LUME_NTFY (lume.ntfy reacts to :ask/opened)."
  (:require [clojure.string :as str]
            [lume.kernel :as kernel :refer [*read-line* actor columns fail! human-only! next-id paint]]
            [lume.status :as status :refer [wrap]]))

(defn open-asks
  "The open asks in state, oldest first."
  [{:keys [asks]}]
  (->> (vals asks)
       (filter #(= :open (:state %)))
       (sort-by #(parse-long (subs (:id %) 1)))))

(defn ask-item
  "The item an ask concerns: a plateau's, or the asking worker's."
  [events opened]
  (or (:item/id opened)
      (some #(when (and (= :spawn/started (:type %)) (= (:by opened) (:spawn/id %))) (:item/id %)) events)))

;; With no question: what status clips, for the human to read and answer. Open
;; asks in full, oldest first, wrapped to the terminal.
(defn- open-asks-text [session width]
  (let [events (kernel/read-events session)
        open (open-asks (kernel/project events))
        opened (into {} (keep #(when (= :ask/opened (:type %)) [(:ask/id %) %]) events))]
    (if (empty? open)
      "no open asks"
      (str/join "\n\n"
                (for [{:keys [id question]} open
                      :let [e (opened id)
                            item (ask-item events e)]]
                  (str/join "\n"
                            (concat
                             [(str id " · asked " (subs (:t e) 0 16) " by " (:by e)
                                   (when item (str " · item " item (when (:measure/id e) (str " at " (:measure/id e))))))]
                             (map #(str "  " %) (wrap question (- width 2)))
                             [(str "  → lume answer " id "   (or lume answer " id " \"<text>\")")])))))))

(def ^:private ask-usage "usage: lume ask [\"<question>\"]")

(defn ask [session [question & extra]]
  (cond (and (nil? question) (nil? extra)) (open-asks-text session (columns))
        (or (str/blank? question) (seq extra)) (fail! ask-usage)
        :else (let [opened (kernel/append! session (fn [events]
                                                     {:type :ask/opened :ask/id (next-id "a" :ask/opened events)
                                                      :question question}))]
                (:ask/id opened))))

;; With no text, at a terminal, the human reads the question in full and types
;; the answer at the prompt; the typed-id confirmation still follows.
(def ^:private answer-usage "usage: lume answer <id> [\"<text>\"]")

(defn answer [session [id text]]
  (when (str/blank? id) (fail! answer-usage))
  (when-not (= "human" (actor)) (fail! "only the human answers asks; record what you think with lume note"))
  (let [open-ask (fn [events]
                   (let [{:keys [state] :as a} (get-in (kernel/project events) [:asks id])]
                     (case state
                       nil (fail! "no such ask:" id)
                       :answered (fail! id "is already answered")
                       :open a)))
        text (or text
                 (let [{:keys [question]} (open-ask (kernel/read-events session))]
                   (or (some-> (*read-line* (str/join "\n" (cons (str id ":") (map #(str "  " %) (wrap question (- (columns) 2)))))
                                            "Answer: ")
                               str/trim)
                       (fail! "refused: human-only commands need an interactive terminal and a typed confirmation;"
                              "agents ask with lume ask \"<question>\""))))]
    (when (str/blank? text) (fail! answer-usage))
    (human-only! "only the human answers asks; record what you think with lume note"
                 (str "Answer " id ": " text) id)
    (kernel/append! session (fn [events]
                              (open-ask events)
                              {:type :ask/answered :ask/id id :answer text}))
    (str id " answered")))

;; Ask show: what status clips. The question in full, who asked and when, the
;; item it concerns (a plateau's, or the asking worker's), and the answer.
(def ^:private ask-show-usage "usage: lume ask show <id>")

(defn ask-show [session [id]]
  (when (str/blank? id) (fail! ask-show-usage))
  (let [events (kernel/read-events session)
        opened (or (kernel/find-event events :ask/opened :ask/id id)
                   (fail! "no such ask:" id))
        answered (kernel/find-event events :ask/answered :ask/id id)
        item (ask-item events opened)
        at #(str (subs (:t %) 0 16) " by " (:by %))]
    (str/join "\n"
              (concat
               [(str id " " (if answered "answered" "open"))
                (str "question " (:question opened))
                (str "asked    " (at opened))]
               (when item [(str "item     " item (when (:measure/id opened) (str " at " (:measure/id opened))))])
               (if answered
                 [(str "answer   " (:answer answered))
                  (str "answered " (at answered) (when-let [via (:via answered)] (str " via " (name via))))]
                 [(str "answer   none yet; only the human answers: lume answer " id)])))))

;; Questions waiting on the human come first in status: they're what blocks.
;; Each wraps to 3 lines before it's clipped; a clipped one says where the rest is.
(defn- ask-lines [state _ _]
  (->> (open-asks state)
       (map (fn [{:keys [id question]}]
              (let [indent (apply str (repeat (inc (count id)) " "))
                    [l & more] (wrap question (- status/width 8 (count indent)) 3
                                     (str " — lume ask show " id))]
                (map #(paint :yellow %)
                     (cons (str id " " l) (map #(str indent %) more))))))))

(def feature
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
