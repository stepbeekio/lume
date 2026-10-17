(ns lume.measure
  "Measurements: an item's numbers as ledger rows. lume item measure, lume
  measure, lume ledger, dropped --revert's restore, the plateau ask, the
  :item/measuring and :measure/ran folds, and the measure line under an item.
  A measurement runs after every spawn on its item too; lume.agent's spawn
  calls run-measure and measure-row for that."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [lume.kernel :as kernel :refer [actor ago clip-to fail! next-id open-item paint
                                            run-in-repo! run-repeated secs sh-out take-flags]]))

;; A measurement spec is its command and how it repeats; rows compare only
;; with rows of the same spec. One run, none dropped, is the default.
(defn- spec [{:keys [measure runs drop]}]
  {:measure measure :runs (or runs 1) :drop (or drop 0)})

(defn before-row
  "The row a projected item's latest spawn pinned as its before row for the
  item's current spec, or nil: no spawn has started, or no good row preceded
  it."
  [item]
  (last (get-in item [:before-rows (spec item)])))

(defn spec-text
  "The command, then how it repeats, then the numbers where higher is better,
  then whether a spawn takes a before row, e.g. bb bench ×10 drop 1 higher
  score before."
  [{:keys [measure higher before] :as m}]
  (let [{:keys [runs drop]} (spec m)]
    (str measure (when (> runs 1) (str " ×" runs)) (when (pos? drop) (str " drop " drop))
         (when (seq higher) (str " higher " (str/join "," higher)))
         (when before " before"))))

(defn direction-text
  "Which way is better, as the plateau ask says it."
  [higher]
  (if (seq higher)
    (str "higher is better for " (str/join "," higher) ", lower for the rest")
    "lower is better"))

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

(defn num-text
  "A measured number for reading: whole numbers as they are, others to 3
  significant digits, never rounding away a digit before the point."
  [v]
  (if (or (not (number? v)) (integer? v) (Double/isNaN v) (Double/isInfinite v))
    (str v)
    (let [d (bigdec v)]
      (-> d (.round (java.math.MathContext. (max 3 (- (.precision d) (.scale d))))) .stripTrailingZeros .toPlainString))))

;; With both spreads, " ~" when the ranges overlap (within noise), " !" when
;; they don't; a single-run row or base has no spread, so no mark.
(defn- change [v b [lo hi] [blo bhi]]
  (when (and (number? v) (number? b) (not= v b))
    (let [d (num-text (- v b))]
      (str "(" (when-not (str/starts-with? d "-") "+") d
           (when (and lo blo) (if (and (<= lo bhi) (<= blo hi)) " ~" " !"))
           ")"))))

(defn measure-text
  "A ledger row's numbers: name=value pairs (name↑ where higher is better),
  each with its change against the row's :base when it has one (marked ~
  within noise or ! outside it when both rows have spreads), then wall
  time, then a failure, then which row it was compared with (unless :vs is
  false), then the spawn it was taken before, if it's a before row."
  [{:keys [values spread ms exit base runs drop failed-run higher before-spawn] :or {drop 0}} & {:keys [vs] :or {vs true}}]
  (str (if (seq values)
         (str/join " " (map (fn [[k v]]
                              (str k (when (some #{k} higher) "↑") "=" (num-text v) (change v (get (:values base) k) (get spread k) (get (:spread base) k))
                                   (when-let [[lo hi] (get spread k)]
                                     (when (not= lo hi) (str "[" (num-text lo) ".." (num-text hi) "]")))))
                            (sort values)))
         "no numbers")
       " · " (secs ms)
       (when (and runs (not failed-run)) (str " · " (if (pos? drop) (str (- runs drop) " of " runs) runs) " runs"))
       (when-not (zero? exit) (str " · fail exit " exit (when failed-run (str " in run " failed-run " of " runs))))
       (when (and vs base) (str " · vs " (:measure/id base)))
       (when before-spawn (str " · before " before-spawn))))

(defn env-text
  "Where a row was taken, e.g. on asahi load 0.21 at a3d907a/msslzuos."
  [{:keys [host load commit change]}]
  (str "on " host (when load (str " load " load))
       (when (or commit change) (str " at " (str/join "/" (remove nil? [commit change]))))))

;; The latest row only; lume item show has the rest.
(defn- measure-line [now {:keys [measure runs drop last-measure]}]
  (let [repeats (spec-text {:runs runs :drop drop})]
    (str "measure " (clip-to (- 18 (count repeats)) measure) repeats
         (when-let [{:keys [t exit base] mid :measure/id :as m} last-measure]
           (str " · " mid (when base (str " vs " (:measure/id base))) " "
                (when-not (zero? exit) (str (paint :red (str "exit " exit)) " · "))
                (clip-to 36 (measure-text (-> m (assoc :exit 0) (dissoc :runs)) :vs false)) " · " (ago now t))))))

;; A measurement runs after every spawn on the item, and on demand with lume measure.
;; --runs N --drop K runs it N times and keeps the last N-K (warm-up runs dropped).
;; Lower is better unless a number is named with --higher (repeatable).
;; --before has each spawn measure first too, so it yields a before/after pair.
(def ^:private item-measure-usage
  "usage: lume item measure <id> \"<cmd>\" [--runs N] [--drop K] [--higher <name>]... [--before]")

(defn item-measure [session args]
  (let [usage item-measure-usage
        before (boolean (some #{"--before"} args))
        [[id cmd & extra] {[runs & rs] "--runs" [drop & ds] "--drop" higher "--higher"}]
        (take-flags (remove #{"--before"} args) #{"--runs" "--drop" "--higher"} usage)
        runs (if runs (parse-long runs) 1)
        drop (if drop (parse-long drop) 0)
        higher (vec (sort (distinct higher)))]
    (when (or (str/blank? cmd) (seq extra) (seq rs) (seq ds)) (fail! usage))
    (when-not (and runs drop (pos? runs) (<= 0 drop) (< drop runs))
      (fail! "--runs must be a positive whole number and --drop less than it;" usage))
    (kernel/append! session (fn [events]
                              (open-item events id)
                              (cond-> {:type :item/measuring :item/id id :measure cmd :by (actor)}
                                (> runs 1) (assoc :runs runs)
                                (pos? drop) (assoc :drop drop)
                                (seq higher) (assoc :higher higher)
                                before (assoc :before true))))
    (str id " measure " (spec-text {:measure cmd :runs runs :drop drop :higher higher :before before}))))

;; Measurements: shell commands run in the repo, their output kept under the
;; session. A measurement's output is numbers, one ledger row per run.

(defn measure-values
  "The numbers in a measurement's output: each name=number or name: number
  (the last one wins), or with none of those, the last number, as value.
  A number may lack digits on one side of its point (.5, -.5, 5.), as bc
  prints them."
  [output]
  (let [num #(or (parse-long %) (parse-double %))
        named (re-seq #"([A-Za-z_][\w.-]*)\s*[=:]\s*(-?(?:\d+(?:\.\d*)?|\.\d+))(?![\w.])" output)]
    (if (seq named)
      (reduce (fn [m [_ k v]] (assoc m k (num v))) {} named)
      (when-let [n (last (re-seq #"-?(?:\d+(?:\.\d*)?|\.\d+)" output))]
        {"value" (num n)}))))

(defn- median [xs]
  (let [xs (vec (sort xs)) n (count xs) mid (quot n 2)]
    (if (odd? n)
      (xs mid)
      ;; Decimal, so the mean of 0.1 and 0.2 is 0.15; whole when it can be.
      (let [m (/ (+ (bigdec (xs (dec mid))) (bigdec (xs mid))) 2M)]
        (if (== m (bigint m)) (long m) (double m))))))

(defn summarize-runs
  "One row from the kept runs: each number's median, its [min max] across
  runs when there are several, and the median wall time."
  [kept]
  (let [per-run (keep (comp measure-values :output) kept)
        values (into {} (for [k (distinct (mapcat keys per-run))
                              :let [vs (keep #(get % k) per-run)]]
                          [k (median vs)]))
        spread (into {} (for [k (keys values)
                              :let [vs (keep #(get % k) per-run)]
                              :when (> (count vs) 1)]
                          [k [(apply min vs) (apply max vs)]]))]
    (cond-> {:ms (long (median (map :ms kept)))}
      (seq values) (assoc :values values)
      (seq spread) (assoc :spread spread))))

(defn- environment
  "Where a row was taken, so a noisy one can be explained: host, 1-minute
  load average, and the repo's git HEAD and jj change. Read-only, and it asks
  only the tools that can answer: each process costs a row a few ms. The jj
  change's commit (:jj-commit) is what a revert restores; read-only means it
  is the working copy as of jj's last snapshot."
  [root]
  (let [load (if (fs/exists? "/proc/loadavg")
               (some-> (re-find #"^[\d.]+" (first (fs/read-all-lines "/proc/loadavg"))) parse-double)
               (some->> (sh-out root "uptime") (re-find #"load averages?:\s*([\d.]+)") second parse-double))]
    (cond-> {:host (try (.getHostName (java.net.InetAddress/getLocalHost)) (catch Exception _ "?"))}
      load (assoc :load load)
      :always (merge (when (fs/exists? (fs/path root ".git"))
                       (when-let [c (sh-out root "git" "rev-parse" "--short" "HEAD")] {:commit c}))
                     (when (fs/exists? (fs/path root ".jj"))
                       (when-let [[change commit] (some-> (sh-out root "jj" "log" "-r" "@" "--ignore-working-copy" "--no-graph"
                                                                  "-T" "change_id.short(8) ++ \" \" ++ commit_id.short(12)")
                                                          (str/split #" "))]
                         {:change change :jj-commit commit}))))))

(defn- plateau-ask!
  "Under cadence on plateau K, the row that makes K (2K, …) rows without a new
  best on its item hands back to the human: an ask is opened and returned,
  unless the item is closed or an ask about it is already open. A row that
  didn't count (failed, or no numbers) never asks."
  [session {id :item/id mid :measure/id}]
  (kernel/append! session
                  (fn [events]
                    (let [s (kernel/project events)
                          k (:plateau (:cadence s))
                          {:keys [stale best-id higher counted]} (get-in s [:items id :plateau])]
                      (when (and k (= mid counted) (pos? stale) (zero? (mod stale k))
                                 (= :open (get-in s [:items id :state]))
                                 (not-any? #(and (= :open (:state %)) (= id (:item %))) (vals (:asks s))))
                        {:type :ask/opened :ask/id (next-id "a" :ask/opened events) :item/id id :measure/id mid
                         :question (str id " plateaued: " stale " measurement rows without a new best"
                                        (when best-id (str " (best " best-id ", " (direction-text (sort higher)) ")"))
                                        "; keep going, change approach, or stop?")
                         :by (actor)})))))

(defn- measure-fields
  "A row's fields from its runs: the kept runs summarized, the spec, and which
  run failed if one did."
  [ran {:keys [measure runs drop higher] :or {runs 1 drop 0}}]
  (let [kept (nthrest ran drop)
        failed (when-not (zero? (:exit (peek ran))) (count ran))]
    (cond-> (merge {:measure measure} (when (seq kept) (summarize-runs kept)))
      (> runs 1) (assoc :runs runs)
      (pos? drop) (assoc :drop drop)
      (seq higher) (assoc :higher higher)
      (and failed (> runs 1)) (assoc :failed-run failed))))

(defn run-measure
  "Run the item's measurement --runs times, drop the first --drop, and log one
  :measure/ran row, which is returned (with :ask if it opened a plateau ask).
  A failed run ends the measurement; the row has the runs kept so far and the
  failing run's exit. :before-spawn sid marks a spawn's before row."
  [session {:keys [id measure runs] :or {runs 1} :as item} & {:keys [before-spawn]}]
  (let [env (environment (:repo (kernel/project (kernel/read-events session))))
        ran (run-in-repo! session "measures" measure
                         (fn [events ran]
                           (let [mid (next-id "m" :measure/ran events)]
                             [mid (cond-> (merge {:type :measure/ran :measure/id mid :item/id id :env env}
                                                 (measure-fields ran item))
                                    before-spawn (assoc :before-spawn before-spawn))]))
                         :n runs)]
    (if-let [ask (plateau-ask! session ran)]
      (assoc ran :ask ask)
      ran)))

(defn try-measure
  "Run cmd exactly as an item's measurement would (sh -c in the repo, runs,
  drop, name=number values, wall time) and return the row with the last run's
  :exit and :output. Logs nothing and keeps no output file: for trying a
  candidate in lume repl before it goes into a spec."
  [session cmd & {:keys [runs drop higher] :or {runs 1 drop 0}}]
  (let [ran (run-repeated session (:repo (kernel/project (kernel/read-events session))) cmd runs (fn [_ _]))
        {:keys [exit output]} (peek ran)]
    (merge {:exit exit} (measure-fields ran {:measure cmd :runs runs :drop drop :higher higher})
           {:output output})))

(defn measure-row
  "The ledger row, not the output: the numbers and wall time against the
  previous row, and where the output is when the command failed."
  [session {:keys [exit out ask] mid :measure/id id :item/id :as ran}]
  (let [row (get-in (kernel/project (kernel/read-events session)) [:items id :last-measure])]
    (str mid " " id " " (measure-text (if (= mid (:measure/id row)) row ran))
         (when-not (zero? exit) (str "\n" out))
         (when ask (str "\nask " (:ask/id ask) " " (:question ask))))))

;; Rollback is data: dropped --revert restores the working copy to the jj
;; commit (or, without jj, the git commit) the item's before row was taken at,
;; and the close event says which row and commit. History is left alone: the
;; restore is a diff in the working copy, and jj's snapshot before it keeps
;; the dropped edits in the op log.
(defn revert!
  "Restore root's working copy to the before row's commit and return what was
  restored, or fail without touching the log."
  [root id item]
  (let [row (or (before-row item)
                (fail! id "has no before row to revert to: a spawn must start on it after a good measurement"))
        mid (:measure/id row)
        {:keys [jj-commit commit]} (:env row)
        [restored cmd] (cond (fs/exists? (fs/path root ".jj"))
                             (if jj-commit
                               [{:jj-commit jj-commit} ["jj" "restore" "--from" jj-commit]]
                               (fail! id "not reverted:" mid "records no jj commit"))
                             (fs/exists? (fs/path root ".git"))
                             (if commit
                               [{:commit commit} ["git" "restore" (str "--source=" commit) "--staged" "--worktree" "--" ":/"]]
                               (fail! id "not reverted:" mid "records no git commit"))
                             :else (fail! id "not reverted:" root "is neither a jj nor a git repo"))
        {:keys [exit err]} (try (apply p/shell {:dir root :in "" :out :string :err :string :continue true} cmd)
                                (catch Exception e {:exit -1 :err (ex-message e)}))]
    (when-not (zero? exit)
      (fail! id "not reverted:" (str/join " " cmd) "failed:" (str/trim (str err))))
    (assoc restored :measure/id mid)))

(def ^:private measure-usage "usage: lume measure <id>")

(defn measure [session [id]]
  (when (str/blank? id) (fail! measure-usage))
  (let [item (or (get-in (kernel/project (kernel/read-events session)) [:items id]) (fail! "no such item:" id))]
    (when-not (:measure item) (fail! id "has no measurement; set one with lume item measure" id "\"<cmd>\""))
    (measure-row session (run-measure session item))))

;; Ledger: every measurement row, open and closed items alike, newest last,
;; each with its change against the row it was compared with when taken. A
;; row names its spec when it's the first shown for its item or the spec changed.

(def ^:private ledger-usage "usage: lume ledger [<item>] [-n N] [--session <id>]")

(defn ledger [root args]
  (let [[[id & extra] {[n & ns] "-n" [sid & sids] "--session"}] (take-flags args #{"-n" "--session"} ledger-usage)
        n (if n (parse-long n) 20)
        _ (when (or (seq extra) (seq ns) (seq sids) (not (and n (<= 0 n)))) (fail! ledger-usage))
        {:keys [items ledger]} (kernel/project (kernel/read-events (kernel/session-by-id root sid)))
        _ (when (and id (not (items id))) (fail! "no such item:" id))
        rows (cond->> ledger id (filterv #(= id (:item/id %))))
        shown (take-last n rows)
        ;; Each shown row's spec, or nil when it's that of the item's previous shown row.
        specs (second (reduce (fn [[last-spec out] {iid :item/id :as row}]
                                (let [spec (spec-text row)]
                                  [(assoc last-spec iid spec) (conj out (when (not= spec (last-spec iid)) spec))]))
                              [{} []] shown))
        lines (map (fn [{mid :measure/id iid :item/id :keys [t] :as row} spec]
                     (str mid " " iid " " (subs t 0 16) " " (measure-text row)
                          (when spec (str " · " (clip-to 60 spec)))))
                   shown specs)]
    (if (empty? rows)
      (if id (str id " has no measurement rows") "no measurement rows")
      (str/join "\n" (concat (when (> (count rows) (count shown))
                               [(str "+" (- (count rows) (count shown)) " earlier rows — lume ledger"
                                     (str/join (map #(str " " %) (remove nil? [id (when sid (str "--session " sid))])))
                                     " -n " (count rows))])
                             lines)))))

(def feature
  {:name "measure"
   :events {:item/measuring {:req #{:item/id :measure}}
            :measure/ran {:req #{:measure/id :item/id :measure :exit :out}}}
   :init {:ledger []}
   :fold {:item/measuring (fn [s e]
                            (update-in s [:items (:item/id e)]
                                       #(merge (dissoc % :runs :drop :higher :before) (select-keys e [:measure :runs :drop :higher :before]))))
          ;; Each row carries the row it's compared with: the latest earlier one
          ;; of the same spec (command, runs and drop) that passed with numbers
          ;; and shares a number's name with it, so a misparsed row isn't the
          ;; baseline for a good one. One good row is kept per set of names.
          ;; Once a spawn has started on the item, that is the latest such row
          ;; before the spawn (its before row, :before-rows), so every row after
          ;; it compares with the code as it was, not with the row before. A row
          ;; a spawn took before its edits (:before-spawn) is the new before row,
          ;; and isn't counted towards a plateau: no change was tried.
          :measure/ran (fn [s e]
                         (let [s (update-in s [:items (:item/id e)]
                                            (fn [item]
                                              (let [names #(set (keys (:values %)))
                                                    base (last (filter #(some (names %) (keys (:values e)))
                                                                       (or (get-in item [:before-rows (spec e)])
                                                                           (get-in item [:good-measures (spec e)]))))
                                                    p (plateau (get-in item [:plateaus (spec e)]) e)]
                                                (cond-> (assoc item :last-measure (cond-> e base (assoc :base base)))
                                                  (not (:before-spawn e)) (-> (assoc :plateau p) (assoc-in [:plateaus (spec e)] p))
                                                  (and (zero? (:exit e)) (seq (:values e)))
                                                  (update-in [:good-measures (spec e)]
                                                             (fn [rows] (conj (filterv #(not= (names %) (names e)) rows) e)))
                                                  (:before-spawn e) (#(assoc % :before-rows (:good-measures %)))))))]
                           ;; Every row, newest last, as compared when it was taken: lume ledger.
                           (update s :ledger conj (get-in s [:items (:item/id e) :last-measure]))))
          :spawn/started (fn [s e] (update-in s [:items (:item/id e)] #(assoc % :before-rows (:good-measures %))))}
   :item-lines (fn [_ item now] (when (:measure item) [(measure-line now item)]))
   :commands [{:path ["item" "measure"] :needs :session :usage item-measure-usage
               :run (fn [{:keys [session]} args] (item-measure session args))
               :help "lume item measure <id> \"<cmd>\" [--runs N] [--drop K] [--higher <name>]... [--before]
                                            measure an item after each spawn: output's name=number pairs
                                            (or else its last number) and wall time become a ledger row;
                                            N runs, first K dropped, give medians and min..max; lower is
                                            better unless --higher names it (shown name↑); rows after a
                                            spawn compare with its before row; --before takes one first"}
              {:path ["measure"] :needs :session :usage measure-usage
               :run (fn [{:keys [session]} args] (measure session args))
               :help "lume measure <id>                           run the item's measurement now and print its row (a baseline)"}
              {:path ["ledger"] :needs :root :usage ledger-usage
               :run (fn [{:keys [root]} args] (ledger root args))
               :help "lume ledger [<item>] [-n N] [--session <id>] measurement rows, newest last (default 20), open and closed items:
                                            numbers with change vs the row compared, spread, wall time, exit"}]})
