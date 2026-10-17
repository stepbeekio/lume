(ns lume.rule
  "Rules: lume on <event-pattern> \"<cmd>\" runs a shell command after each
  matching event, as a reaction. The human declares them in the log, like
  lume allow; lume.rule turns each into a reaction, so a rule runs where
  reactions run: the human-side process, never a worker or driver."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [lume.kernel :as kernel :refer [actor fail! human-only! next-id]]))

(def ^:private on-usage "usage: lume on [<event-pattern> \"<cmd>\" | --remove <id>]")

;; A pattern is what a reaction's :on is: a type (gate/ran) or a namespace
;; (ask, every ask/… type), and must name something a feature declares.
(defn- pattern [s]
  (let [k (keyword (str/replace-first s #"^:" ""))
        types (keys (kernel/events))]
    (when-not (some #(or (= k %) (and (nil? (namespace k)) (= (name k) (namespace %)))) types)
      (fail! (str "no event type or namespace " s "; lume log shows the types")))
    k))

(defn- rule-line [{:keys [id on cmd last]}]
  (str id " on " (subs (str on) 1) " → " cmd
       (when last (str " · last " (if (:timed-out last) "timed out" (str "exit " (:exit last))) " " (kernel/secs (:ms last))))))

(defn on [session args]
  (let [[a b] args]
    (cond
      (empty? args)
      (let [rules (vals (:rules (kernel/project (kernel/read-events session))))]
        (if (seq rules) (str/join "\n" (map rule-line (sort-by :order rules))) "no rules"))

      (= "--remove" a)
      (let [rule (or (get-in (kernel/project (kernel/read-events session)) [:rules b]) (fail! "no such rule:" (str b)))]
        (human-only! "only the human changes rules; ask with lume ask \"<question>\""
                     (str "Remove " (rule-line rule) ".") "yes")
        (kernel/append! session (constantly {:type :rule/removed :rule/id b :by (actor)}))
        (str "removed " b))

      (or (str/starts-with? a "--") (str/blank? b) (> (count args) 2)) (fail! on-usage)

      :else
      (let [k (pattern a)]
        (human-only! "only the human adds rules; ask with lume ask \"<question>\""
                     (str "After every " (subs (str k) 1) " event, run: " b ".") "yes")
        (:rule/id (kernel/append! session (fn [events]
                                            (let [id (next-id "r" :rule/added events)]
                                              {:type :rule/added :rule/id id :on k :cmd b :by (actor)}))))))))

;; Running a rule. Its command sees the event in env: LUME_EVENT as EDN, and
;; LUME_EVENT_<KEY> for each scalar key (gate/ran's :exit is LUME_EVENT_EXIT,
;; :item/id is LUME_EVENT_ITEM_ID). It acts as the rule (LUME_ACTOR r<n>), so
;; what it logs says which rule did it, and it runs with this process's env
;; (LUME_NTFY included), unlike a worker. Anything it logs reacts one level
;; deeper (LUME_DEPTH); a rule that would run at the depth cap, where its own
;; appends could no longer react, logs :rule/capped instead. Its failures
;; (exit, missing binary, timeout, even lume refusing the log) stay its own:
;; they never fail the command whose event triggered it.

(def ^:dynamic *timeout-ms* 60000)

(defn- env-key [k]
  (str "LUME_EVENT_" (-> (str (some-> (namespace k) (str "_")) (name k)) (str/replace #"[^A-Za-z0-9]" "_") str/upper-case)))

(defn event-env [e]
  (into {"LUME_EVENT" (pr-str e)}
        (for [[k v] e :when (and (keyword? k) (or (string? v) (number? v) (keyword? v) (boolean? v)))]
          [(env-key k) (if (keyword? v) (subs (str v) 1) (str v))])))

(defn- run-rule [session {:keys [id cmd]} e]
  (let [root (:repo (kernel/project (kernel/read-events session)))
        dir (fs/create-dirs (fs/path session "rules"))
        out (fs/file (fs/create-temp-file {:dir dir :prefix (str id "-") :suffix ".out"}))
        t0 (System/currentTimeMillis)
        proc (p/process ["sh" "-c" cmd] {:dir root :in "" :out :write :out-file out :err :out
                                         :extra-env (merge (event-env e)
                                                           {"LUME_SESSION" (str session) "LUME_ACTOR" id
                                                            "LUME_SPAWNED_AS" "" "LUME_DEPTH" (str kernel/*depth*)})})
        done (deref proc *timeout-ms* nil)]
    (when-not done (p/destroy-tree proc) @proc)
    (let [tail (last (remove str/blank? (str/split-lines (slurp out))))]
      (cond-> {:exit (if done (:exit done) 124) :ms (- (System/currentTimeMillis) t0) :out (str out)}
        (not done) (assoc :timed-out true)
        tail (assoc :tail (kernel/clip 200 tail))))))

(defn- fire! [session {:keys [id] :as rule} e]
  (try
    (kernel/append! session (constantly
                             (merge {:rule/id id :trigger (:type e) :by id}
                                    (if (< kernel/*depth* kernel/max-depth)
                                      (merge {:type :rule/ran}
                                             (try (run-rule session rule e)
                                                  (catch Exception x {:exit 127 :ms 0 :tail (ex-message x)})))
                                      {:type :rule/capped}))))
    (catch Exception x
      (binding [*out* *err*] (println "lume: rule" id "on" (:type e) "failed:" (ex-message x))))))

(defn- reactions [{:keys [rules]}]
  (for [rule (vals rules)]
    {:on (:on rule) :run #(fire! kernel/*session* rule %)}))

(def feature
  {:name "rule"
   :events {:rule/added {:req #{:rule/id :on :cmd} :human true}
            :rule/removed {:req #{:rule/id} :human true}
            :rule/ran {:req #{:rule/id :trigger :exit :ms}}
            :rule/capped {:req #{:rule/id :trigger}}}
   :init {:rules {}}
   :fold {:rule/added (fn [s e] (assoc-in s [:rules (:rule/id e)]
                                          {:id (:rule/id e) :on (:on e) :cmd (:cmd e) :order (count (:rules s))}))
          :rule/removed (fn [s e] (update s :rules dissoc (:rule/id e)))
          :rule/ran (fn [s e] (cond-> s (get-in s [:rules (:rule/id e)]) (assoc-in [:rules (:rule/id e) :last] e)))}
   :reactions reactions
   :commands [{:path ["on"] :needs :session :usage on-usage
               :run (fn [{:keys [session]} args] (on session args))
               :help "lume on <event-pattern> \"<cmd>\"             human at a tty, types yes: run <cmd> after each matching event
                                            (a type like gate/ran, or a namespace like ask), event in env
lume on [--remove <id>]                     list rules, or drop one (human)"}]})
