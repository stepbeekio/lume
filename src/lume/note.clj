(ns lume.note
  "Notes: steering for drivers and workers. lume note, note --unpin and note
  show, the pinned and notes status sections, and the notes in a worker's
  prompt. Pinned notes stay in status and every worker prompt until unpinned;
  others age out."
  (:require [clojure.string :as str]
            [lume.kernel :as kernel :refer [clip clock fail! next-id]]))

(def ^:private note-usage "usage: lume note [--pin] \"<text>\" | lume note --unpin <id>")

(defn note [session [a b]]
  (let [usage note-usage]
    (case a
      "--unpin"
      (do (when (str/blank? b) (fail! usage))
          (kernel/append! session (fn [events]
                                    (when-not (some #(and (= b (:note/id %)) (:pinned %))
                                                    (:notes (kernel/project events)))
                                      (fail! "no pinned note:" b))
                                    {:type :note/unpinned :note/id b}))
          (str "unpinned " b))
      (let [pin (= "--pin" a)
            text (if pin b a)]
        (when (str/blank? text) (fail! usage))
        (let [e (kernel/append! session (fn [events]
                                          (cond-> {:type :note/added :note/id (next-id "n" :note/added events)
                                                   :text text}
                                            pin (assoc :pinned true))))]
          (str (if pin "pinned " "noted ") (:note/id e)))))))

;; Note show: what status clips. The text in full, who noted it and when, and
;; whether it's pinned.
(def ^:private note-show-usage "usage: lume note show <id>")

(defn note-show [session [id]]
  (when (str/blank? id) (fail! note-show-usage))
  (let [events (kernel/read-events session)
        added (or (some #(when (and (= :note/added (:type %)) (= id (:note/id %))) %) events)
                  (fail! "no such note:" id))
        unpinned (some #(when (and (= :note/unpinned (:type %)) (= id (:note/id %))) %) events)
        at #(str (subs (:t %) 0 16) " by " (:by %))]
    (str/join "\n"
              (concat
               [(str id (cond unpinned " unpinned" (:pinned added) " pinned" :else ""))
                (str "noted    " (at added))]
               (when unpinned [(str "unpinned " (at unpinned))])
               [(:text added)]))))

;; Answered asks join :notes too (the ask feature folds them in), so they
;; reach the prompt with the notes.
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
          :note/unpinned (fn [s e] (update s :notes (partial mapv #(cond-> % (= (:note/id e) (:note/id %)) (dissoc :pinned)))))}
   ;; Pinned notes are the lasting ones. Asks past 4 lines take their room, so
   ;; status stays one screen; when there are any, recent notes give up room.
   :status [{:label "pinned" :order 50 :cap (fn [_ shown] (- 6 (max 0 (- (shown "asks" 0) 4))))
             :lines (fn [s _ _] (map #(str (:note/id %) " " (:text %)) (filter :pinned (:notes s))))}
            {:label "notes" :order 80 :cap (fn [s _] (if (some :pinned (:notes s)) 3 5))
             :lines (fn [s _ _] (map #(str "[" (clock (:t %)) " " (:by %) "] " (clip 80 (:text %)))
                                     (reverse (remove :pinned (:notes s)))))}]
   :prompt prompt
   ;; note show needs an id: with anything else, show is the text.
   :commands [{:path ["note"] :needs :session :usage note-usage
               :run (fn [{:keys [session]} args] (note session args))
               :help "lume note [--pin] \"<text>\"                  steering for drivers and workers; pinned notes never age out
lume note --unpin <id>                      let a pinned note age out like the rest"}
              {:path ["note" "show"] :needs :session :usage note-show-usage
               :run (fn [{:keys [session]} args] (if (= 1 (count args)) (note-show session args) (note session (cons "show" args))))
               :help "lume note show <id>                         one note in full: text, who noted it and when, pinned or not"}]})
