(ns lume.note
  "Notes: steering for drivers and workers. lume note, note --unpin and note
  show, the pinned and notes status sections, and the notes in a worker's
  prompt. Pinned notes stay in status and every worker prompt until unpinned;
  others age out."
  (:require [clojure.string :as str]
            [lume.kernel :as kernel :refer [clip clock fail! next-id]]))

(def ^:private note-usage "usage: lume note [--pin] \"<text>\"")
(def ^:private unpin-usage "usage: lume note --unpin <id>")

(defn add!
  "Log a note (pinned with :pin) and return its :note/added event."
  [session text & {:keys [pin]}]
  (kernel/append! session (fn [events]
                            (cond-> {:type :note/added :note/id (next-id "n" :note/added events)
                                     :text text}
                              pin (assoc :pinned true)))))

(defn added-line
  "The line that answers a note: \"noted n1\", or \"pinned n1\"."
  [e]
  (str (if (:pinned e) "pinned " "noted ") (:note/id e)))

(defn- note-args
  "note's args as {:text :pin}, or a usage error."
  [args]
  (let [[[text & extra] {pin "--pin"}] (kernel/take-flags args #{} #{"--pin"} note-usage)]
    (when (or (str/blank? text) (seq extra)) (fail! note-usage))
    {:text text :pin (boolean pin)}))

(defn note [session args]
  (let [{:keys [text pin]} (note-args args)]
    (added-line (add! session text :pin pin))))

(defn- pinned? [events id]
  (some #(and (= id (:note/id %)) (:pinned %)) (:notes (kernel/project events))))

(defn unpin [session args]
  (let [[id] (kernel/positionals args 1 unpin-usage)]
    (kernel/append! session (fn [events]
                              (when-not (pinned? events id) (fail! "no pinned note:" id))
                              {:type :note/unpinned :note/id id}))
    (str "unpinned " id)))

;; Note show: what status clips. The text in full, who noted it and when, and
;; whether it's pinned.
(def ^:private note-show-usage "usage: lume note show <id>")

(defn note-show [session args]
  (let [[id] (kernel/positionals args 1 note-show-usage)
        events (kernel/read-events session)
        added (or (kernel/find-event events :note/added :note/id id)
                  (fail! "no such note:" id))
        unpinned (kernel/find-event events :note/unpinned :note/id id)
        at #(str (subs (:t %) 0 16) " by " (:by %))]
    (str/join "\n"
              (concat
               [(str id (cond unpinned " unpinned" (:pinned added) " pinned" :else ""))
                (str "noted    " (at added))]
               (when unpinned [(str "unpinned " (at unpinned))])
               [(:text added)]))))

;; An answer is steering, like a note, so it joins the notes (and reaches status
;; and the worker prompt with them). The question comes from the ask feature's
;; :asks, which its fold, earlier in feature order, has already set.
(defn- answered-note [s e]
  (update s :notes conj (assoc (select-keys e [:ask/id :answer :t :by])
                               :question (get-in s [:asks (:ask/id e) :question])
                               :text (str (:ask/id e) " → " (:answer e)))))

(defn- prompt [{:keys [notes]} _]
  (let [line #(if (:ask/id %) (str (:ask/id %) " " (:question %) " → " (:answer %)) (:text %))
        bullets (fn [heading ns] (when (seq ns) (cons heading (map #(str "- " (line %)) ns))))]
    (concat (bullets "Pinned notes:" (filter :pinned notes))
            (bullets "Recent notes from the session:" (take-last 10 (remove :pinned notes))))))

(def feature
  {:name "note"
   :events {:note/added {:req #{:note/id :text}}
            :note/unpinned {:req #{:note/id}}}
   :init {:notes []}
   :fold {:note/added (fn [s e] (update s :notes conj e))
          :note/unpinned (fn [s e] (update s :notes (partial mapv #(cond-> % (= (:note/id e) (:note/id %)) (dissoc :pinned)))))
          :ask/answered answered-note}
   ;; Pinned notes are the lasting ones. Asks past 4 lines take their room, so
   ;; status stays one screen; when there are any, recent notes give up room.
   :status [{:label "pinned" :order 50 :cap (fn [_ shown] (- 6 (max 0 (- (shown "asks" 0) 4))))
             :lines (fn [s _ _] (map #(str (:note/id %) " " (:text %)) (filter :pinned (:notes s))))}
            {:label "notes" :order 80 :cap (fn [s _] (if (some :pinned (:notes s)) 3 5))
             :lines (fn [s _ _] (map #(str "[" (clock (:t %)) " " (:by %) "] " (clip 80 (:text %)))
                                     (reverse (remove :pinned (:notes s)))))}]
   :prompt prompt
   :commands [{:path ["note"] :needs :session :usage note-usage
               :run (fn [{:keys [session]} args] (note session args))
               :help "lume note [--pin] \"<text>\"                  steering for drivers and workers; pinned notes never age out"}
              {:path ["note" "--unpin"] :needs :session :usage unpin-usage
               :run (fn [{:keys [session]} args] (unpin session args))
               :help "lume note --unpin <id>                      let a pinned note age out like the rest"}
              {:path ["note" "show"] :needs :session :usage note-show-usage
               :run (fn [{:keys [session]} args] (note-show session args))
               :help "lume note show <id>                         one note in full: text, who noted it and when, pinned or not"}]})
