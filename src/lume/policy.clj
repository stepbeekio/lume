(ns lume.policy
  "Budget and cadence: the limits the human sets on agents. lume budget and
  lume cadence, their folds, the guards that refuse spawns and turns past them,
  and the budget status line. Anyone may tighten a limit; loosening one is the
  human's (human-only!), like permissions. Drive reads them through cadence and
  over-budget."
  (:require [clojure.string :as str]
            [lume.kernel :as kernel :refer [actor clip-to fail! human-only! paint take-flags]]))

;; Budgets bound what agents spend: dollars and minutes of spawns and drive
;; turns, and how many spawns started.
(defn- spend [s {:keys [cost-usd ms]}]
  (cond-> s
    cost-usd (update-in [:spent :usd] + cost-usd)
    ms (update-in [:spent :minutes] + (/ ms 60000.0))))

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

(defn- amount [k v]
  (let [n (str/replace (format "%.1f" (double v)) #"\.0$" "")]
    (case k :usd (format "$%.2f" (double v)) :spawns (str (long v)) n)))

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

;; Budgets: the substrate, not the driver, bounds spend. Anyone may lower or add
;; a limit; raising one is the human's, like permissions.
(def ^:private budget-usage "usage: lume budget [--usd N] [--spawns N] [--minutes N]")

(defn budget [session args]
  (let [usage budget-usage
        [pos vals] (take-flags args #{"--usd" "--spawns" "--minutes"} usage)
        limits (into {} (for [[flag [v & more]] vals
                              :let [k (keyword (subs flag 2))
                                    n (if (= :spawns k) (parse-long v) (parse-double v))]]
                          (do (when (or (seq more) (nil? n) (neg? n)) (fail! usage))
                              [k n])))
        state #(kernel/project (kernel/read-events session))]
    (when (seq pos) (fail! usage))
    (when (seq limits)
      (let [{:keys [budget]} (state)
            raised (filter (fn [[k n]] (when-let [l (get budget k)] (> n l))) limits)]
        (when (seq raised)
          (human-only! "only the human raises a budget; lower it, or ask with lume ask \"<question>\""
                       (str "Raise the budget: " (str/join " · " (map (fn [[k n]] (str (name k) " " (budget k) " → " n)) raised)) ".")
                       "yes"))
        (kernel/append! session (constantly (assoc limits :type :session/budgeted :by (actor))))))
    (let [s (state)]
      (str "budget " (budget-text s) (when-let [over (over-budget s)] (str "\n" over))))))

;; Cadence: when drive hands control back, as data. Anyone may hand back more
;; often than the human's latest cadence; handing back less often is the human's.
(def ^:private cadence-usage "usage: lume cadence [every-spawn | batch N] [on gate-fail] [on plateau K]")

(defn- parse-cadence [words]
  (let [n #(let [n (some-> % parse-long)] (if (and n (pos? n)) n (fail! cadence-usage)))]
    (loop [[a b c :as words] words, rules default-cadence]
      (cond (empty? words) rules
            (= "every-spawn" a) (recur (rest words) (assoc rules :batch 1))
            (= "batch" a) (recur (drop 2 words) (assoc rules :batch (n b)))
            (= ["on" "gate-fail"] [a b]) (recur (drop 2 words) (assoc rules :gate-fail true))
            (= ["on" "plateau"] [a b]) (recur (drop 3 words) (assoc rules :plateau (n c)))
            :else (fail! cadence-usage)))))

(defn cadence-cmd [session args]
  (let [words (remove str/blank? (mapcat #(str/split % #"\s+") args))
        state #(kernel/project (kernel/read-events session))]
    (when (seq words)
      (let [c (parse-cadence words)]
        (when-let [raises (cadence-raises (cadence-ceiling (state)) c)]
          (human-only! (str "only the human hands back less often (" raises "); hand back more often,"
                            " or ask with lume ask \"<question>\"")
                       (str "Raise the cadence ceiling: " raises ".") "yes"))
        (kernel/append! session (constantly (assoc c :type :session/cadence :by (actor))))))
    (let [s (state)]
      (str "cadence " (cadence-text (cadence s))
           " · ceiling " (cadence-text (cadence-ceiling s))))))

(defn- budget-lines [{:keys [budget spent] :as state} _ _]
  (when (or (seq budget) (some pos? (vals spent)))
    [(let [over (over-budget state)
           line (clip-to 92 (str (budget-text state) (when over " · reached: no new spawns or turns")))]
       (if over (paint :red line) line))]))

(defn- within-budget [s _] (over-budget s))

(def budget-feature
  {:name "budget"
   :events {:session/budgeted {:req #{}}}
   :init {:spent {:usd 0 :spawns 0 :minutes 0}}
   :fold {:session/budgeted (fn [s e] (update s :budget merge (select-keys e [:usd :spawns :minutes])))
          :spawn/started (fn [s _] (update-in s [:spent :spawns] inc))
          :spawn/exited spend
          :drive/exited spend}
   :guards {:spawn/started within-budget
            :drive/turn within-budget
            :spawn/requested within-budget}
   :status [{:label "budget" :order 40 :cap 1 :lines budget-lines}]
   :commands [{:path ["budget"] :needs :session :usage budget-usage
               :run (fn [{:keys [session]} args] (budget session args))
               :help "lume budget [--usd N] [--spawns N] [--minutes N]
                                            limit the session's agent spend (spawns and drive turns); with no
                                            flags, print spent/limit. Lowering is anyone's; raising, the human's"}]})

;; A driver's turn may request up to its cadence's batch of spawns; :requested
;; counts each actor's requests (a driver id is one turn).
(def cadence-feature
  {:name "cadence"
   :events {:session/cadence {:req #{}}}
   :fold {:session/cadence (fn [s e]
                             (let [c (select-keys e [:batch :gate-fail :plateau])]
                               (cond-> (assoc s :cadence c)
                                 (= "human" (:by e)) (assoc :cadence-ceiling c))))
          :spawn/requested (fn [s e] (update-in s [:requested (:by e)] (fnil inc 0)))}
   :guards {:spawn/requested (fn [s e]
                               (let [c (cadence s)]
                                 (when (>= (get-in s [:requested (:by e)] 0) (:batch c))
                                   (str "cadence " (cadence-text c) " allows " (:batch c) " spawn request(s) a turn,"
                                        " and this turn has made them; finish the turn now"))))}
   :commands [{:path ["cadence"] :needs :session :usage cadence-usage
               :run (fn [{:keys [session]} args] (cadence-cmd session args))
               :help "lume cadence [every-spawn | batch N] [on gate-fail] [on plateau K]
                                            when drive hands back: N spawn requests a turn, run in order;
                                            a failed gate ends the batch; K rows without a new best (lower
                                            is better, unless --higher) ask the human. No rules: print it. Above the human's
                                            latest cadence is the human's"}]})
