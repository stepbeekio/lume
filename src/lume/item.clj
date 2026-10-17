(ns lume.item
  "Work items: lume item add, gate, close and show, and gates. An item's gate
  is a shell command whose exit code decides done.

  Each fold writes only its own feature's state, with one accepted exception:
  other features add their own keys to an item map (measure its spec and rows:
  :measure, :runs, :drop, :higher, :before, :last-measure, :plateau(s),
  :good-measures, :before-rows; agent :spawns). Only item sets :state, :intent
  and :gate."
  (:require [clojure.string :as str]
            [lume.kernel :as kernel :refer [ago clip fail! more next-id open-item paint run-in-repo!]]
            [lume.measure :as measure]
            [lume.status :as status]))

;; --parent files work that emerged while on an open item under it. The tree
;; only makes that work visible: nothing is inherited and nothing orders spawns.
(def ^:private item-add-usage "usage: lume item add \"<intent>\" [--gate \"<cmd>\"] [--parent <id>]")

(defn item-add [session args]
  (let [usage item-add-usage
        [[intent & extra] {[gate & gates] "--gate" [parent & parents] "--parent"}]
        (kernel/take-flags args #{"--gate" "--parent"} usage)]
    (when (or (str/blank? intent) (seq extra) (seq gates) (seq parents)) (fail! usage))
    ;; Without --gate the item gets the session's default gate, written on the event.
    (let [e (kernel/append! session (fn [events]
                                      (when parent (open-item events parent))
                                      (let [gate (or gate (:default-gate (kernel/project events)))]
                                        (cond-> {:type :item/added :item/id (next-id "i" :item/added events)
                                                 :intent intent}
                                          gate (assoc :gate gate)
                                          parent (assoc :parent parent)))))]
      (:item/id e))))

(def ^:private item-gate-usage "usage: lume item gate <id> \"<cmd>\"")

(defn item-gate [session args]
  (let [[id gate] (kernel/positionals args 2 item-gate-usage)]
    (kernel/append! session (fn [events]
                              (open-item events id)
                              {:type :item/gated :item/id id :gate gate}))
    (str id " gate " gate)))

;; Gates and measurements: shell commands run in the repo, their output kept
;; under the session. A gate's exit code decides done; a measurement's output
;; is numbers, one ledger row per run.

(defn run-gate
  "Run the item's gate and log a :gate/ran event, which is returned."
  [session {:keys [id gate]}]
  (run-in-repo! session "gates" gate
               (fn [events _]
                 (let [gid (next-id "g" :gate/ran events)]
                   [gid {:type :gate/ran :gate/id gid :item/id id :gate gate}]))))

(defn gate-summary [{:keys [exit tail out] gid :gate/id}]
  (str gid " " (if (zero? exit) "pass" (str "fail exit " exit))
       (when (and tail (not (zero? exit))) (str " · " (str/replace tail #"\s+" " ") "\n" out))))

;; dropped --revert restores the working copy to the item's before row (lume.measure).
(def ^:private item-close-usage "usage: lume item close <id> done|dropped [--revert] \"<why>\"")

(defn- item-close-args
  "item close's args as {:id :disposition :why :revert}, or a usage error."
  [args]
  (let [usage item-close-usage
        [[id disposition why] {revert "--revert"}] (kernel/take-flags args #{} #{"--revert"} usage)]
    (when-not (#{"done" "dropped"} disposition) (fail! usage))
    (when (and revert (not= "dropped" disposition)) (fail! "--revert goes with dropped;" usage))
    {:id id :disposition (keyword disposition) :why why :revert (boolean revert)}))

(defn close!
  "Close item id as disposition (:done or :dropped) for why. Done runs the
  item's gate first and fails if it fails; :revert restores the working copy
  to the item's before row. Returns {:id :disposition :ran :restored}: the
  :gate/ran event and what was restored, either nil."
  [session id disposition why & {:keys [revert]}]
  (let [events (kernel/read-events session)
        item (open-item events id)
        ran (when (and (= :done disposition) (:gate item)) (run-gate session item))]
    (when (and ran (not (zero? (:exit ran))))
      (fail! id "not done: gate" (gate-summary ran)))
    (let [restored (when revert (measure/revert! (:repo (kernel/project events)) id item))]
      (kernel/append! session (fn [events]
                                (open-item events id)
                                (cond-> {:type :item/closed :item/id id :disposition disposition :why why}
                                  ran (assoc :gate/id (:gate/id ran))
                                  restored (merge restored))))
      {:id id :disposition disposition :ran ran :restored restored})))

(defn close-line
  "close!'s result as one line: the disposition, the gate run, what was restored."
  [{:keys [id disposition ran restored]}]
  (str id " " (name disposition) (when ran (str " · " (gate-summary ran)))
       (when restored
         (str " · restored " (if (:jj-commit restored) (str "jj " (:jj-commit restored)) (str "git " (:commit restored)))
              " from " (:measure/id restored)))))

(defn item-close [session args]
  (let [{:keys [id disposition why revert]} (item-close-args args)]
    (close-line (close! session id disposition why :revert revert))))

;; Item show: what status clips. The intent and gate in full, then the item's
;; events: its own, its spawns' (bar progress), and notes and asks naming it.

(defn item-event-pred
  "A predicate for the events of item id: its own, those of its spawns (bar
  progress), and notes and asks that name it."
  [id spawn-ids]
  (let [named (re-pattern (str "(?<![A-Za-z0-9])" id "(?![0-9])"))]
    (fn [{:keys [type] :as e}]
      (or (= id (:item/id e))
          (and (spawn-ids (:spawn/id e)) (not= :spawn/progress type))
          (and (#{:note/added :ask/opened :ask/answered} type)
               (some #(and % (re-find named %)) [(:text e) (:question e) (:answer e)]))))))

;; Every line here is the item's, so its id is left out.
(defn- item-event-line [e]
  (str (kernel/event-line #(subs % 0 16) 240 (dissoc e :item/id))
       (when (:by e) (str " [" (:by e) "]"))))

(def ^:private item-show-usage "usage: lume item show <id>")

(defn item-show [session args]
  (let [[id] (kernel/positionals args 1 item-show-usage)
        events (kernel/read-events session)
        item (or (get-in (kernel/project events) [:items id]) (fail! "no such item:" id))
        related (filter (item-event-pred id (set (:spawns item))) events)
        shown (take-last 20 related)]
    (str/join "\n"
              (concat
               [(str id " " (name (:state item)) (when (:why item) (str " — " (clip 200 (:why item)))))
                (str "intent  " (clip 2000 (:intent item)))
                (str "gate    " (or (:gate item) "none"))]
               (when (:measure item)
                 [(str "measure " (kernel/event-text (last (filter #(= :item/measuring (:type %)) related))))])
               (more related shown (str "earlier events — lume log --item " id " -n N"))
               (map item-event-line shown)))))

;; Status: open items as a tree, each with each feature's :item-lines under
;; it, then the latest closed.

(defn- gate-line [now {:keys [gate last-gate]}]
  (str "gate " (clip 40 gate)
       (when-let [{:keys [exit t tail] gid :gate/id} last-gate]
         (str " · " gid " " (if (zero? exit) (paint :green "pass") (paint :red (str "fail exit " exit))) " · " (ago now t)
              (when (and tail (not (zero? exit))) (str " · " (clip 30 tail)))))))

;; An open item is a group: its line, then each feature's :item-lines under it.
(defn- open-lines [{:keys [items] :as state} _ now]
  (let [under (keep :item-lines (kernel/features))]
    (map (fn [[{:keys [id intent] :as item} depth]]
           (let [in (apply str (repeat (* 2 depth) " "))]
             (map #(str in %)
                  (cons (str id "  " (clip (- 80 (count in)) intent))
                        (map #(str "    " %) (mapcat #(% state item now) under))))))
         (status/tree (filter #(= :open (:state %)) (vals items))))))

(defn- closed-lines [{:keys [items]} _ _]
  (map (fn [{:keys [id state intent why]}]
         (str id "  " (name state) " — " (clip 40 intent) (when why (str " — " (clip 40 why)))))
       (reverse (sort-by :closed-at (sort-by :order (remove #(= :open (:state %)) (vals items)))))))

(def feature
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
   :item-lines (fn [_ item now] (when (:gate item) [(gate-line now item)]))
   :commands [{:path ["item" "add"] :needs :session :usage item-add-usage
               :run (fn [{:keys [session]} args] (item-add session args))
               :help "lume item add \"<intent>\" [--gate \"<cmd>\"] [--parent <id>]
                                            add a work item; the gate's exit code decides done;
                                            --parent files work that emerged on an open item under it
                                            (status indents it there; nothing else follows from it)"}
              {:path ["item" "gate"] :needs :session :usage item-gate-usage
               :run (fn [{:keys [session]} args] (item-gate session args))
               :help "lume item gate <id> \"<cmd>\"                 set or replace an open item's gate"}
              {:path ["item" "close"] :needs :session :usage item-close-usage
               :run (fn [{:keys [session]} args] (item-close session args))
               :help "lume item close <id> done|dropped \"<why>\"   record a disposition; done runs the gate, refused if it fails
lume item close <id> dropped --revert \"<why>\"
                                            also restore the working copy to the jj (else git) commit
                                            of the item's before row; refused if it has none or that fails"}
              {:path ["item" "show"] :needs :session :usage item-show-usage
               :run (fn [{:keys [session]} args] (item-show session args))
               :help "lume item show <id>                         one item in full: intent, gate, disposition and its events"}]})
