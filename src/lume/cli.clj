(ns lume.cli
  "The lume command line. Output is agent-first: short, structured, bounded."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [lume.status :as status]
            [lume.store :as store])
  (:import (java.time Instant)))

;; lume.agents and lume.spec load on first use, via requiring-resolve: most
;; lume calls (note, item, ask, a driver's spawn request) are short processes
;; that need neither, and loading code is most of what they cost.

(defn- fail! [& msg]
  (throw (ex-info (str/join " " msg) {:lume/user-error true})))

(defn- next-id [prefix type events]
  (str prefix (inc (count (filter #(= type (:type %)) events)))))

;; Spawns and drives get their id in LUME_SPAWNED_AS as well as LUME_ACTOR,
;; because an agent's own settings can override LUME_ACTOR in what it runs:
;; Claude Code's settings.json env sets LUME_ACTOR=claude for interactive
;; sessions, and wins over the env lume passes (i46).
(def ^:dynamic *actor*
  "Who is acting: a spawn or drive id inside an agent lume started, else
  LUME_ACTOR, else \"human\". An empty var counts as unset, so a test's
  subprocess can clear the LUME_SPAWNED_AS it inherits inside a spawn. Tests bind it."
  (or (some not-empty [(System/getenv "LUME_SPAWNED_AS") (System/getenv "LUME_ACTOR")]) "human"))

(defn- actor [] *actor*)

(defn- driver? [] (boolean (re-matches #"d\d+" (actor))))

;; Human-only commands need more than LUME_ACTOR, which an agent running as your
;; uid can forge (a1): an interactive stdin and a typed confirmation. Spawns and
;; drives run with no tty, so they can't pass.
(defn- tty? []
  (zero? (:exit (p/shell {:in :inherit :out :string :err :string :continue true} "test" "-t" "0"))))

(defn- tty-confirm
  "True if stdin is a terminal and the user types `expected` at the prompt."
  [prompt expected]
  (and (tty?)
       (do (binding [*out* *err*] (print (str prompt " Type " expected " to confirm: ")) (flush))
           (= expected (some-> (read-line) str/trim)))))

(def ^:dynamic *confirm*
  "(fn [prompt expected]) → true when a human at a terminal confirmed. Tests bind it."
  tty-confirm)

(defn- tty-read
  "What the user types after printing `shown` and `prompt`, or nil with no terminal."
  [shown prompt]
  (when (tty?)
    (binding [*out* *err*] (println shown) (print prompt) (flush))
    (read-line)))

(def ^:dynamic *read-line*
  "(fn [shown prompt]) → a line typed at a terminal, or nil without one. Tests bind it."
  tty-read)

(defn- columns
  "The terminal's width, for text a human reads in full: stty, then COLUMNS, then 100."
  []
  (or (some->> (p/shell {:in :inherit :out :string :err :string :continue true} "stty" "size")
               :out (re-find #"^\d+ (\d+)") second parse-long (max 40))
      (some-> (System/getenv "COLUMNS") parse-long)
      100))

(defn- human-only!
  "Refuse unless the actor is the human and a human at a terminal confirms."
  [refusal prompt expected]
  (when-not (= "human" (actor)) (fail! refusal))
  (when-not (*confirm* prompt expected)
    (fail! "refused: human-only commands need an interactive terminal and a typed confirmation;"
           "agents ask with lume ask \"<question>\"")))

(defn- open-item [events id]
  (let [item (get-in (status/project events) [:items id])]
    (cond (nil? item) (fail! "no such item:" id)
          (not= :open (:state item)) (fail! id "is already" (name (:state item)))
          :else item)))

;; Commands. Each takes the session dir (or repo root, for init) and its args,
;; and returns the text to print.

(defn- take-flags
  "Split args into positionals and the values of the given flags, in order.
  A flag with no value, or any other flag-like arg, is a usage error."
  [args flags usage]
  (loop [[a & more :as args] args, pos [], vals {}]
    (cond (empty? args) [pos vals]
          (flags a) (let [[v & more] more]
                      (when (or (str/blank? v) (str/starts-with? v "--")) (fail! usage))
                      (recur more pos (update vals a (fnil conj []) v)))
          (str/starts-with? a "--") (fail! usage)
          :else (recur more (conj pos a) vals))))

;; A session may have a default gate: items added without --gate get it, so
;; nothing is ungated by omission. --allow is lume allow, so it's human-only too.
(defn init [root args]
  (let [usage "usage: lume init \"<goal>\" [--gate \"<cmd>\"] [--allow \"<cmd>\"]..."
        [[goal & extra] {gates "--gate" allows "--allow"}] (take-flags args #{"--gate" "--allow"} usage)
        gate (first gates)
        allows (distinct allows)]
    (when (or (str/blank? goal) (seq extra) (next gates)) (fail! usage))
    (when (seq allows)
      (human-only! "only the human changes permissions; ask with lume ask \"<question>\""
                   (str "Workers will be allowed to run: " (str/join " · " allows) ".") "yes"))
    (let [session (store/new-session! root)]
      (store/append! session (constantly {:type :session/started :goal goal :repo root :by (actor)}))
      (when gate
        (store/append! session (constantly {:type :session/gated :gate gate :by (actor)})))
      (doseq [command allows]
        (store/append! session (constantly {:type :session/allowed :command command :by (actor)})))
      (str "session " (store/session-id session) " started"
           (when gate (str " · default gate " gate))
           (when (seq allows) (str " · workers may run " (str/join " · " allows)))))))

(defn- flag [opts name]
  (second (drop-while #(not= name %) opts)))

(defn item-add [session args]
  (let [usage "usage: lume item add \"<intent>\" [--gate \"<cmd>\"]"
        [[intent & extra] {[gate & gates] "--gate"}] (take-flags args #{"--gate"} usage)]
    (when (or (str/blank? intent) (seq extra) (seq gates)) (fail! usage))
    ;; Without --gate the item gets the session's default gate, written on the event.
    (let [e (store/append! session (fn [events]
                                     (let [gate (or gate (:default-gate (status/project events)))]
                                       (cond-> {:type :item/added :item/id (next-id "i" :item/added events)
                                                :intent intent :by (actor)}
                                         gate (assoc :gate gate)))))]
      (:item/id e))))

(defn item-gate [session [id gate]]
  (when (str/blank? gate) (fail! "usage: lume item gate <id> \"<cmd>\""))
  (store/append! session (fn [events]
                           (open-item events id)
                           {:type :item/gated :item/id id :gate gate :by (actor)}))
  (str id " gate " gate))

;; A measurement runs after every spawn on the item, and on demand with lume measure.
;; --runs N --drop K runs it N times and keeps the last N-K (warm-up runs dropped).
;; Lower is better unless a number is named with --higher (repeatable).
(defn item-measure [session args]
  (let [usage "usage: lume item measure <id> \"<cmd>\" [--runs N] [--drop K] [--higher <name>]..."
        [[id cmd & extra] {[runs & rs] "--runs" [drop & ds] "--drop" higher "--higher"}]
        (take-flags args #{"--runs" "--drop" "--higher"} usage)
        runs (if runs (parse-long runs) 1)
        drop (if drop (parse-long drop) 0)
        higher (vec (sort (distinct higher)))]
    (when (or (str/blank? cmd) (seq extra) (seq rs) (seq ds)) (fail! usage))
    (when-not (and runs drop (pos? runs) (<= 0 drop) (< drop runs))
      (fail! "--runs must be a positive whole number and --drop less than it;" usage))
    (store/append! session (fn [events]
                             (open-item events id)
                             (cond-> {:type :item/measuring :item/id id :measure cmd :by (actor)}
                               (> runs 1) (assoc :runs runs)
                               (pos? drop) (assoc :drop drop)
                               (seq higher) (assoc :higher higher))))
    (str id " measure " (status/spec-text {:measure cmd :runs runs :drop drop :higher higher}))))

;; Gates and measurements: shell commands run in the repo, their output kept
;; under the session. A gate's exit code decides done; a measurement's output
;; is numbers, one ledger row per run.

(defn- run-repeated
  "Run cmd with sh -c in root up to n times, stopping at the first failure;
  returns the runs, {:exit :ms :output} in order. (on-run i run) sees each."
  [session root cmd n on-run]
  (reduce (fn [runs i]
            (let [t0 (System/currentTimeMillis)
                  {:keys [exit out]} @(p/process ["sh" "-c" cmd] {:dir root :in "" :out :string :err :out
                                                                   :extra-env {"LUME_SESSION" session}})
                  run {:exit exit :ms (- (System/currentTimeMillis) t0) :output out}
                  r (conj runs run)]
              (on-run i run)
              (if (zero? exit) r (reduced r))))
          [] (range n)))

(defn- run-in-repo
  "Run cmd in the repo up to n times (1 by default), stopping at the first
  failure, with all output in one file in the session's `kind` dir. Log the
  event that (event-for events runs) returns as [id event], where runs are
  {:exit :ms :output} in order; the last run's :exit and :ms, the output's
  :tail and :out are added unless the event has its own. The id is allocated
  under the log lock."
  [session kind cmd event-for & {:keys [n] :or {n 1}}]
  (let [root (:repo (status/project (store/read-events session)))
        dir (fs/create-dirs (fs/path session kind))
        tmp (fs/file (fs/create-temp-file {:dir dir :prefix "running-"}))
        runs (run-repeated session root cmd n
                           (fn [i {:keys [exit ms output]}]
                             (spit tmp (str (when (> n 1) (str "--- run " (inc i) "/" n " · exit " exit " · " (status/secs ms) "\n"))
                                            output)
                                   :append true)))
        {:keys [exit ms output]} (peek runs)
        tail (last (remove str/blank? (str/split-lines output)))
        ran (store/append! session (fn [events]
                                     (let [[id e] (event-for events runs)]
                                       (cond-> (merge {:exit exit :ms ms} e
                                                      {:out (str (fs/path dir (str id ".out"))) :by (actor)})
                                         tail (assoc :tail tail)))))]
    (fs/move tmp (:out ran))
    ran))

(defn- run-gate
  "Run the item's gate and log a :gate/ran event, which is returned."
  [session {:keys [id gate]}]
  (run-in-repo session "gates" gate
               (fn [events _]
                 (let [gid (next-id "g" :gate/ran events)]
                   [gid {:type :gate/ran :gate/id gid :item/id id :gate gate}]))))

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

(defn- sh-out [root & cmd]
  (try (let [{:keys [exit out]} (apply p/shell {:dir root :in "" :out :string :err :string :continue true} cmd)]
         (when (zero? exit) (not-empty (str/trim out))))
       (catch Exception _ nil)))

(defn- environment
  "Where a row was taken, so a noisy one can be explained: host, 1-minute
  load average, and the repo's git HEAD and jj change. Read-only, and it asks
  only the tools that can answer: each process costs a row a few ms."
  [root]
  (let [load (if (fs/exists? "/proc/loadavg")
               (some-> (re-find #"^[\d.]+" (first (fs/read-all-lines "/proc/loadavg"))) parse-double)
               (some->> (sh-out root "uptime") (re-find #"load averages?:\s*([\d.]+)") second parse-double))]
    (cond-> {:host (try (.getHostName (java.net.InetAddress/getLocalHost)) (catch Exception _ "?"))}
      load (assoc :load load)
      :always (merge (when (fs/exists? (fs/path root ".git"))
                       (when-let [c (sh-out root "git" "rev-parse" "--short" "HEAD")] {:commit c}))
                     (when (fs/exists? (fs/path root ".jj"))
                       (when-let [c (sh-out root "jj" "log" "-r" "@" "--ignore-working-copy" "--no-graph"
                                            "-T" "change_id.short(8)")]
                         {:change c}))))))

(defn- plateau-ask!
  "Under cadence on plateau K, the row that makes K (2K, …) rows without a new
  best on its item hands back to the human: an ask is opened and returned,
  unless the item is closed or an ask about it is already open. A row that
  didn't count (failed, or no numbers) never asks."
  [session {id :item/id mid :measure/id}]
  (store/append! session
                 (fn [events]
                   (let [s (status/project events)
                         k (:plateau (status/cadence s))
                         {:keys [stale best-id higher counted]} (get-in s [:items id :plateau])]
                     (when (and k (= mid counted) (pos? stale) (zero? (mod stale k))
                                (= :open (get-in s [:items id :state]))
                                (not-any? #(and (= :open (:state %)) (= id (:item %))) (vals (:asks s))))
                       {:type :ask/opened :ask/id (next-id "a" :ask/opened events) :item/id id :measure/id mid
                        :question (str id " plateaued: " stale " measurement rows without a new best"
                                       (when best-id (str " (best " best-id ", " (status/direction-text (sort higher)) ")"))
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

(defn- run-measure
  "Run the item's measurement --runs times, drop the first --drop, and log one
  :measure/ran row, which is returned (with :ask if it opened a plateau ask).
  A failed run ends the measurement; the row has the runs kept so far and the
  failing run's exit."
  [session {:keys [id measure runs] :or {runs 1} :as item}]
  (let [env (environment (:repo (status/project (store/read-events session))))
        ran (run-in-repo session "measures" measure
                         (fn [events ran]
                           (let [mid (next-id "m" :measure/ran events)]
                             [mid (merge {:type :measure/ran :measure/id mid :item/id id :env env}
                                         (measure-fields ran item))]))
                         :n runs)]
    (if-let [ask (plateau-ask! session ran)] (assoc ran :ask ask) ran)))

(defn try-measure
  "Run cmd exactly as an item's measurement would (sh -c in the repo, runs,
  drop, name=number values, wall time) and return the row with the last run's
  :exit and :output. Logs nothing and keeps no output file: for trying a
  candidate in lume repl before it goes into a spec."
  [session cmd & {:keys [runs drop higher] :or {runs 1 drop 0}}]
  (let [ran (run-repeated session (:repo (status/project (store/read-events session))) cmd runs (fn [_ _]))
        {:keys [exit output]} (peek ran)]
    (merge {:exit exit} (measure-fields ran {:measure cmd :runs runs :drop drop :higher higher})
           {:output output})))

(defn- measure-row
  "The ledger row, not the output: the numbers and wall time against the
  previous row, and where the output is when the command failed."
  [session {:keys [exit out ask] mid :measure/id id :item/id :as ran}]
  (let [row (get-in (status/project (store/read-events session)) [:items id :last-measure])]
    (str mid " " id " " (status/measure-text (if (= mid (:measure/id row)) row ran))
         (when-not (zero? exit) (str "\n" out))
         (when ask (str "\nask " (:ask/id ask) " " (:question ask))))))

(defn- gate-summary [{:keys [exit tail out] gid :gate/id}]
  (str gid " " (if (zero? exit) "pass" (str "fail exit " exit))
       (when (and tail (not (zero? exit))) (str " · " (str/replace tail #"\s+" " ") "\n" out))))

(defn item-close [session [id disposition why]]
  (when-not (#{"done" "dropped"} disposition)
    (fail! "usage: lume item close <id> done|dropped \"<why>\""))
  (let [item (open-item (store/read-events session) id)
        ran (when (and (= "done" disposition) (:gate item)) (run-gate session item))]
    (when (and ran (not (zero? (:exit ran))))
      (fail! id "not done: gate" (gate-summary ran)))
    (store/append! session (fn [events]
                             (open-item events id)
                             (cond-> {:type :item/closed :item/id id :disposition (keyword disposition)
                                      :why why :by (actor)}
                               ran (assoc :gate/id (:gate/id ran)))))
    (str id " " disposition (when ran (str " · " (gate-summary ran))))))

(def ^:private measure-usage "usage: lume measure <id>")

(defn measure [session [id]]
  (when (str/blank? id) (fail! measure-usage))
  (let [item (or (get-in (status/project (store/read-events session)) [:items id]) (fail! "no such item:" id))]
    (when-not (:measure item) (fail! id "has no measurement; set one with lume item measure" id "\"<cmd>\""))
    (measure-row session (run-measure session item))))

;; Pinned notes stay in status and every worker prompt until unpinned; others age out.
(defn note [session [a b]]
  (let [usage "usage: lume note [--pin] \"<text>\" | lume note --unpin <id>"]
    (case a
      "--unpin"
      (do (when (str/blank? b) (fail! usage))
          (store/append! session (fn [events]
                                   (when-not (some #(and (= b (:note/id %)) (:pinned %))
                                                   (:notes (status/project events)))
                                     (fail! "no pinned note:" b))
                                   {:type :note/unpinned :note/id b :by (actor)}))
          (str "unpinned " b))
      (let [pin (= "--pin" a)
            text (if pin b a)]
        (when (str/blank? text) (fail! usage))
        (let [e (store/append! session (fn [events]
                                         (cond-> {:type :note/added :note/id (next-id "n" :note/added events)
                                                  :text text :by (actor)}
                                           pin (assoc :pinned true))))]
          (str (if pin "pinned " "noted ") (:note/id e)))))))

;; Note show: what status clips. The text in full, who noted it and when, and
;; whether it's pinned.
(defn note-show [session [id]]
  (when (str/blank? id) (fail! "usage: lume note show <id>"))
  (let [events (store/read-events session)
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

;; Only the human grants permissions: workers and agent drivers would grant themselves
;; whatever they need, so they ask with lume ask instead.
(defn allow [session [a b]]
  (let [revoke (= "--revoke" a)
        command (if revoke b a)]
    (when (str/blank? command) (fail! "usage: lume allow [--revoke] \"<cmd>\""))
    (human-only! "only the human changes permissions; ask with lume ask \"<question>\""
                 (str (if revoke "Workers may no longer run: " "Workers may run: ") command ".") "yes")
    (store/append! session (constantly {:type (if revoke :session/disallowed :session/allowed)
                                        :command command :by (actor)}))
    (str (if revoke "workers may no longer run " "workers may run ") command)))

;; Budgets: the substrate, not the driver, bounds spend. Anyone may lower or add
;; a limit; raising one is the human's, like permissions.
(defn budget [session args]
  (let [usage "usage: lume budget [--usd N] [--spawns N] [--minutes N]"
        [pos vals] (take-flags args #{"--usd" "--spawns" "--minutes"} usage)
        limits (into {} (for [[flag [v & more]] vals
                              :let [k (keyword (subs flag 2))
                                    n (if (= :spawns k) (parse-long v) (parse-double v))]]
                          (do (when (or (seq more) (nil? n) (neg? n)) (fail! usage))
                              [k n])))
        state #(status/project (store/read-events session))]
    (when (seq pos) (fail! usage))
    (when (seq limits)
      (let [{:keys [budget]} (state)
            raised (filter (fn [[k n]] (when-let [l (get budget k)] (> n l))) limits)]
        (when (seq raised)
          (human-only! "only the human raises a budget; lower it, or ask with lume ask \"<question>\""
                       (str "Raise the budget: " (str/join " · " (map (fn [[k n]] (str (name k) " " (budget k) " → " n)) raised)) ".")
                       "yes"))
        (store/append! session (constantly (assoc limits :type :session/budgeted :by (actor))))))
    (let [s (state)]
      (str "budget " (status/budget-text s) (when-let [over (status/over-budget s)] (str "\n" over))))))

;; Cadence: when drive hands control back, as data. Anyone may hand back more
;; often than the human's latest cadence; handing back less often is the human's.
(def ^:private cadence-usage "usage: lume cadence [every-spawn | batch N] [on gate-fail] [on plateau K]")

(defn- parse-cadence [words]
  (let [n #(let [n (some-> % parse-long)] (if (and n (pos? n)) n (fail! cadence-usage)))]
    (loop [[a b c :as words] words, rules status/default-cadence]
      (cond (empty? words) rules
            (= "every-spawn" a) (recur (rest words) (assoc rules :batch 1))
            (= "batch" a) (recur (drop 2 words) (assoc rules :batch (n b)))
            (= ["on" "gate-fail"] [a b]) (recur (drop 2 words) (assoc rules :gate-fail true))
            (= ["on" "plateau"] [a b]) (recur (drop 3 words) (assoc rules :plateau (n c)))
            :else (fail! cadence-usage)))))

(defn cadence [session args]
  (let [words (remove str/blank? (mapcat #(str/split % #"\s+") args))
        state #(status/project (store/read-events session))]
    (when (seq words)
      (let [c (parse-cadence words)]
        (when-let [raises (status/cadence-raises (status/cadence-ceiling (state)) c)]
          (human-only! (str "only the human hands back less often (" raises "); hand back more often,"
                            " or ask with lume ask \"<question>\"")
                       (str "Raise the cadence ceiling: " raises ".") "yes"))
        (store/append! session (constantly (assoc c :type :session/cadence :by (actor))))))
    (let [s (state)]
      (str "cadence " (status/cadence-text (status/cadence s))
           " · ceiling " (status/cadence-text (status/cadence-ceiling s))))))

(defn- within-budget! [events]
  (when-let [over (status/over-budget (status/project events))]
    (fail! over)))

;; Questions for the human. Anyone may ask; only the human answers, and the
;; answer reaches status and workers alongside the notes.
(defn- ask-item
  "The item an ask concerns: a plateau's, or the asking worker's."
  [events opened]
  (or (:item/id opened)
      (some #(when (and (= :spawn/started (:type %)) (= (:by opened) (:spawn/id %))) (:item/id %)) events)))

;; With no question: what status clips, for the human to read and answer. Open
;; asks in full, oldest first, wrapped to the terminal.
(defn- open-asks [session width]
  (let [events (store/read-events session)
        open (->> (vals (:asks (status/project events)))
                  (filter #(= :open (:state %)))
                  (sort-by #(parse-long (subs (:id %) 1))))
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
                             (map #(str "  " %) (status/wrap question (- width 2)))
                             [(str "  → lume answer " id "   (or lume answer " id " \"<text>\")")])))))))

(defn ask [session [question & extra]]
  (cond (and (nil? question) (nil? extra)) (open-asks session (columns))
        (or (str/blank? question) (seq extra)) (fail! "usage: lume ask \"<question>\"")
        :else (:ask/id (store/append! session (fn [events]
                                                {:type :ask/opened :ask/id (next-id "a" :ask/opened events)
                                                 :question question :by (actor)})))))

;; With no text, at a terminal, the human reads the question in full and types
;; the answer at the prompt; the typed-id confirmation still follows.
(defn answer [session [id text]]
  (when (str/blank? id) (fail! "usage: lume answer <id> [\"<text>\"]"))
  (when-not (= "human" (actor)) (fail! "only the human answers asks; record what you think with lume note"))
  (let [open-ask (fn [events]
                   (let [{:keys [state] :as a} (get-in (status/project events) [:asks id])]
                     (case state
                       nil (fail! "no such ask:" id)
                       :answered (fail! id "is already answered")
                       :open a)))
        text (or text
                 (let [{:keys [question]} (open-ask (store/read-events session))]
                   (or (some-> (*read-line* (str/join "\n" (cons (str id ":") (map #(str "  " %) (status/wrap question (- (columns) 2)))))
                                            "Answer: ")
                               str/trim)
                       (fail! "refused: human-only commands need an interactive terminal and a typed confirmation;"
                              "agents ask with lume ask \"<question>\""))))]
    (when (str/blank? text) (fail! "usage: lume answer <id> [\"<text>\"]"))
    (human-only! "only the human answers asks; record what you think with lume note"
                 (str "Answer " id ": " text) id)
    (store/append! session (fn [events]
                             (open-ask events)
                             {:type :ask/answered :ask/id id :answer text :by (actor)}))
    (str id " answered")))

;; Ask show: what status clips. The question in full, who asked and when, the
;; item it concerns (a plateau's, or the asking worker's), and the answer.
(defn ask-show [session [id]]
  (when (str/blank? id) (fail! "usage: lume ask show <id>"))
  (let [events (store/read-events session)
        opened (or (some #(when (and (= :ask/opened (:type %)) (= id (:ask/id %))) %) events)
                   (fail! "no such ask:" id))
        answered (some #(when (and (= :ask/answered (:type %)) (= id (:ask/id %))) %) events)
        item (ask-item events opened)
        at #(str (subs (:t %) 0 16) " by " (:by %))]
    (str/join "\n"
              (concat
               [(str id " " (if answered "answered" "open"))
                (str "question " (:question opened))
                (str "asked    " (at opened))]
               (when item [(str "item     " item (when (:measure/id opened) (str " at " (:measure/id opened))))])
               (if answered
                 [(str "answer   " (:answer answered)) (str "answered " (at answered))]
                 [(str "answer   none yet; only the human answers: lume answer " id)])))))

(defn- worker-prompt [events item]
  (let [{:keys [goal notes]} (status/project events)
        line #(if (:ask/id %) (str (:ask/id %) " " (:question %) " → " (:answer %)) (:text %))
        bullets (fn [heading ns] (when (seq ns) (cons heading (map #(str "- " (line %)) ns))))]
    (str/join "\n"
              (concat
               ["You are a worker in a lume session."
                (str "Session goal: " goal)
                (str "Your work item (" (:id item) "): " (:intent item))]
               (bullets "Pinned notes:" (filter :pinned notes))
               (bullets "Recent notes from the session:" (take-last 10 (remove :pinned notes)))
               ["Work in the current directory. Record findings with `lume note \"<text>\"`"
                "and follow-up work with `lume item add \"<intent>\"`."
                "When you finish, end with a short summary"
                "of what you did and anything left undone."]))))

;; Spawns. A worker's stdout is copied to its .out file line by line, and the
;; adapter folds each line into progress, logged at most every *progress-ms*.

(def ^:dynamic *progress-ms* 15000)

(defn- clip-last [s] (subs s 0 (min 200 (count s))))

(defn- follow!
  "Copy the worker's stdout to `out` until it closes, logging :spawn/progress
  events as the adapter's view of it changes. Returns the final progress."
  [session sid agent proc out]
  (with-open [r (io/reader (:out proc))
              w (io/writer (fs/file out))]
    (loop [prog {} logged {} at 0]
      (if-let [line (.readLine ^java.io.BufferedReader r)]
        (let [prog ((requiring-resolve 'lume.agents/progress) agent prog line)
              now (System/currentTimeMillis)
              due (and (not= prog logged) (>= (- now at) *progress-ms*))]
          (.write w (str line "\n"))
          (.flush w)
          (if due
            (do (store/append! session (constantly (merge {:type :spawn/progress :spawn/id sid}
                                                          (cond-> prog (:last prog) (update :last clip-last)))))
                (recur prog prog now))
            (recur prog logged at)))
        prog))))

(defn- worktree-fingerprint
  "A hash of the repo's git HEAD, its changed paths and their contents, so a
  spawn that changed nothing can be told apart from one that did. Read-only
  (no index refresh), and nil when the repo isn't a git repo (jj colocates
  one) or git can't answer."
  [root]
  (when (fs/exists? (fs/path root ".git"))
    (try
      (let [{:keys [exit out]} (p/shell {:dir root :in "" :out :string :err :string :continue true}
                                        "git" "--no-optional-locks" "status" "--porcelain=v1" "-z" "-uall")]
        (when (zero? exit)
          (let [md (java.security.MessageDigest/getInstance "SHA-256")
                add #(.update md ^bytes %)]
            (add (.getBytes (str (sh-out root "git" "rev-parse" "HEAD") "\n" out)))
            (doseq [entry (str/split out #"\x00")
                    :let [f (fs/path root (subs entry (min 3 (count entry))))]
                    :when (fs/regular-file? f)]
              (add (fs/read-all-bytes f)))
            (apply str (map #(format "%02x" %) (.digest md))))))
      (catch Exception _ nil))))

(defn- idle-why
  "Why a spawn's item shouldn't close on its gate, or nil: it opened an ask,
  or the repo is exactly as it was before it ran."
  [session sid before after]
  (let [asks (keep #(when (and (= :ask/opened (:type %)) (= sid (:by %))) (:ask/id %))
                   (store/read-events session))]
    (cond (seq asks) (str sid " asked " (str/join ", " asks))
          (and before (= before after)) (str sid " changed nothing in the repo"))))

(defn- run-spawn [session root id opts]
  (let [agent (keyword (or (flag opts "--agent") "claude"))
        events (store/read-events session)
        item (open-item events id)
        started (store/append! session (fn [events]
                                         (within-budget! events)
                                         (let [sid (next-id "s" :spawn/started events)]
                                           {:type :spawn/started :spawn/id sid :item/id id :agent agent
                                            :out (str (fs/path session "spawns" (str sid ".out")))
                                            :by (actor)})))
        {sid :spawn/id out :out} started
        done (promise)
        hook (Thread. #(when-not (realized? done)
                         (store/append! session (constantly {:type :spawn/exited :spawn/id sid
                                                             :interrupted true}))))]
    (fs/create-dirs (fs/parent out))
    (.addShutdownHook (Runtime/getRuntime) hook)
    (let [t0 (System/currentTimeMillis)
          before (worktree-fingerprint root)
          ;; A worker may run the session's allowed commands, and its own gate.
          allowed (distinct (cond-> (:allow (status/project events)) (:gate item) (conj (:gate item))))
          proc (p/process ((requiring-resolve 'lume.agents/command) agent (worker-prompt events item) {:allow allowed})
                          {:dir root :in ""
                           :err :write :err-file (fs/file (str out ".err"))
                           :extra-env {"LUME_SESSION" session "LUME_ACTOR" sid "LUME_SPAWNED_AS" sid}})
          {:keys [steps]} (follow! session sid agent proc out)
          {:keys [exit]} @proc
          summary ((requiring-resolve 'lume.agents/summarize) agent (slurp out))
          exited (store/append! session (constantly (cond-> (merge {:type :spawn/exited :spawn/id sid :exit exit
                                                                    :ms (- (System/currentTimeMillis) t0)}
                                                                   summary)
                                                      steps (assoc :steps steps))))]
      (deliver done true)
      (.removeShutdownHook (Runtime/getRuntime) hook)
      (str sid " " id " exit " exit " in " (quot (:ms exited) 1000) "s"
           (when-let [r (:result exited)] (str " · " (str/replace (str r) #"\s+" " ")))
           "\n" out
           ;; Measure what the worker left, then: the worker may have closed the
           ;; item itself; if it asked or changed nothing, a passing gate says
           ;; nothing about its work, so the item stays open; otherwise its gate decides.
           (let [item (get-in (status/project (store/read-events session)) [:items id])]
             (when (:measure item)
               (str "\n" (measure-row session (run-measure session item)))))
           (let [item (get-in (status/project (store/read-events session)) [:items id])]
             (when (and (:gate item) (= :open (:state item)))
               (str "\n" (if-let [why (idle-why session sid before (worktree-fingerprint root))]
                           (str id " left open: " why " · " (note session [(str id " left open, not gated: " why)]))
                           (try (item-close session [id "done" (str "gate passed after " sid)])
                                (catch clojure.lang.ExceptionInfo e (ex-message e)))))))))))

;; A driver's spawn is only a request: its turn ends first, then drive runs it.
;; The cadence's batch is how many requests a turn may make.
(defn spawn [session root [id & opts]]
  (when (str/blank? id) (fail! "usage: lume spawn <id> [--agent claude]"))
  (if (driver?)
    (let [requested (fn [events] (count (filter #(and (= :spawn/requested (:type %)) (= (actor) (:by %))) events)))]
      (store/append! session (fn [events]
                               (open-item events id)
                               (within-budget! events)
                               (let [c (status/cadence (status/project events))]
                                 (when (>= (requested events) (:batch c))
                                   (fail! "cadence" (status/cadence-text c) "allows" (:batch c) "spawn request(s) a turn,"
                                          "and this turn has made them; finish the turn now")))
                               (cond-> {:type :spawn/requested :item/id id :by (actor)}
                                 (flag opts "--agent") (assoc :agent (keyword (flag opts "--agent"))))))
      (let [events (store/read-events session)
            n (requested events)
            batch (:batch (status/cadence (status/project events)))]
        (if (< n batch)
          (str id " spawn requested (" n " of " batch " this turn); requests run in order when your turn ends")
          (str id " spawn requested; it runs when your turn ends, so finish the turn now"))))
    (run-spawn session root id opts)))

;; Drive: a loop of fresh driver agents, each making one decision from status.

(defn- driver-prompt [session did]
  (str/join "\n"
            [(str "You are the driver of a lume session, turn " did ". Make ONE decision, carry it out with")
             "lume commands, and exit. A fresh driver takes the next turn from `lume status`, so"
             "anything the next one must know goes in the log (`lume note`, `lume note --pin`)."
             ""
             "First run `jj st`. Changes in the working copy are a finished worker's: review them"
             "(`jj diff`) before anything else. If they're good, `jj commit -m \"<what and why>\"` and"
             "record it with `lume note \"committed <item>: <change id> <what>\"`; if not, say what's"
             "wrong with `lume note` or `lume item add`. A turn that logs nothing stops the drive."
             "Otherwise pick one: add, gate or close items; `lume spawn <id>` to put a worker on an"
             "open item (it runs after you exit); `lume note` a lesson; or `lume ask \"<question>\"`"
             "when only the human can decide (the loop stops until they answer). Keep an ask to one or two"
             "lines: question, lettered options, your pick; put the context in a note and name it (n12)."
             "The cadence says how many spawns a turn may request (every-spawn: one; batch N: N, run"
             "in order). Set your own within the human's ceiling, e.g. `lume cadence batch 3 on gate-fail`."
             ""
             "Rules: never push. Don't edit files; workers do that. Add items with --gate \"bb test\"."
             "End with one line: what you decided and why."
             ""
             (status/render (store/session-id session) (store/read-events session) (Instant/now))]))

(defn- drive-turn
  "Run one driver agent until it exits, and return its summary."
  [session root agent did allow]
  (let [out (fs/path session "drives" (str did ".out"))]
    (fs/create-dirs (fs/parent out))
    (let [t0 (System/currentTimeMillis)
          {:keys [exit]} @(p/process ((requiring-resolve 'lume.agents/command) agent (driver-prompt session did) {:allow allow :role :driver})
                                     {:dir root :in ""
                                      :out :write :out-file (fs/file out)
                                      :err :write :err-file (fs/file (str out ".err"))
                                      :extra-env {"LUME_SESSION" session "LUME_ACTOR" did "LUME_SPAWNED_AS" did}})]
      (assoc ((requiring-resolve 'lume.agents/summarize) agent (slurp (fs/file out)))
             :exit exit :out (str out) :ms (- (System/currentTimeMillis) t0)))))

(defn- hand-back-early
  "Why a batch stops before its next spawn, from the events since the last
  one started: an ask is open (a plateau's, say), the budget is reached, or
  under on gate-fail, the spawn's gate failed. nil to carry on."
  [events since id]
  (let [state (status/project events)
        gate (->> (drop since events) (filter #(and (= :gate/ran (:type %)) (= id (:item/id %)))) last)]
    (cond (some #(= :open (:state %)) (vals (:asks state))) "an ask is open"
          (status/over-budget state) "the budget is reached"
          (and (:gate-fail (status/cadence state)) gate (not (zero? (:exit gate))))
          (str "on gate-fail: " id " gate " (:gate/id gate) " failed"))))

(defn- run-requests
  "Run a turn's spawn requests in order until the cadence hands back early; a
  cut batch logs :drive/handed-back with the items it skipped."
  [session root did say requests]
  (loop [[{id :item/id a :agent} & more] requests]
    (when id
      (let [since (count (store/read-events session))]
        (say (try (run-spawn session root id ["--agent" (name (or a :claude))])
                  (catch clojure.lang.ExceptionInfo e (ex-message e))))
        (let [why (when (seq more) (hand-back-early (store/read-events session) since id))]
          (if why
            (let [skipped (mapv :item/id more)]
              (store/append! session (constantly {:type :drive/handed-back :drive/id did :why why
                                                  :skipped skipped :by (actor)}))
              (say (str did " handed back: " why "; skipped " (str/join " " skipped))))
            (recur more)))))))

;; A drive outlives the commits its turns make, so before each turn it checks
;; the lume source it loaded. When that changed and the new code loads, the
;; drive logs :drive/reloaded and re-execs itself on it, carrying on from the
;; same turn (--from); when the new code doesn't load, it notes so and keeps
;; running the old.

(def ^:dynamic *source*
  "The src directory this lume runs from."
  (some-> (io/resource "lume/cli.clj") io/file fs/parent fs/parent str))

(defn- fingerprint [src]
  (when src
    (into (sorted-map) (map (juxt str #(fs/file-time->millis (fs/last-modified-time %))))
          (fs/glob src "**.clj"))))

(defn- load-error
  "nil if src's lume.cli loads in a fresh bb, else its first error line."
  [src]
  (let [{:keys [exit err]} @(p/process ["bb" "-cp" src "-e" "(require 'lume.cli)"] {:out :string :err :string})]
    (when-not (zero? exit)
      (or (first (remove str/blank? (str/split-lines err))) (str "exit " exit)))))

(def ^:dynamic *reexec*
  "Replace this process with lume from src, run with args."
  (fn [src args] (p/exec (into ["bb" "-cp" src "-m" "lume.cli"] args))))

(defn drive
  "Run up to --turns driver turns, each followed by the spawns it requested,
  printing a line per turn and spawn. Returns why it stopped."
  [session root opts]
  (let [turns (parse-long (or (flag opts "--turns") "10"))
        agent (or (flag opts "--agent") "claude")
        from (parse-long (or (flag opts "--from") "1"))
        _ (when-not (and turns (pos? turns) from (pos? from)) (fail! "usage: lume drive [--turns N] [--agent claude]"))
        src *source*
        loaded (atom (fingerprint src))
        say #(do (println %) (flush))
        done (promise)
        stop! (fn [why]
                (deliver done true)
                (store/append! session (constantly {:type :drive/stopped :why why :by (actor)}))
                (str "drive stopped: " why))
        hook (Thread. #(when-not (realized? done)
                         (store/append! session (constantly {:type :drive/stopped :why "interrupted" :by (actor)}))))]
    (when (= 1 from)
      (store/append! session (constantly {:type :drive/started :turns turns :agent (keyword agent) :by (actor)})))
    (.addShutdownHook (Runtime/getRuntime) hook)
    (try
      (loop [n from]
        (let [{:keys [items asks allow] :as state} (status/project (store/read-events session))
              ask (->> (vals asks) (filter #(= :open (:state %))) (sort-by #(parse-long (subs (:id %) 1))) first)
              over (status/over-budget state)
              now (when (> n from) (fingerprint src))
              changed (and now (not= now @loaded))
              broken (when changed (load-error src))]
          (when broken
            (reset! loaded now)
            (note session [(str "drive: lume source changed but doesn't load, so turn " n
                                " runs the code the drive started with: " broken)]))
          (cond
            ask (stop! (str "ask " (:id ask) " is open: " (:question ask) " — answer with lume answer " (:id ask) " \"<text>\""))
            over (stop! over)
            (not-any? #(= :open (:state %)) (vals items)) (stop! "no open items")
            (> n turns) (stop! (str turns " turns run"))
            (and changed (not broken))
            (do (store/append! session (constantly {:type :drive/reloaded :n n :by (actor)}))
                (*reexec* src ["drive" "--turns" (str turns) "--agent" agent "--from" (str n)]))
            :else
            (let [{did :drive/id} (store/append! session (fn [events]
                                                           {:type :drive/turn :drive/id (next-id "d" :drive/turn events)
                                                            :n n :by (actor)}))
                  {:keys [exit result out] :as turn} (drive-turn session root (keyword agent) did allow)
                  logged (rest (drop-while #(not= did (:drive/id %)) (store/read-events session)))]
              ;; Logged after reading what the turn did, so it never counts as the turn's work.
              (store/append! session (constantly (merge {:type :drive/exited :drive/id did}
                                                        turn)))
              (say (str did " exit " exit (when result (str " · " (str/replace (str result) #"\s+" " "))) "\n" out))
              (if (empty? logged)
                (stop! (str did " logged no events"))
                (do (run-requests session root did say (filter #(= :spawn/requested (:type %)) logged))
                    (recur (inc n))))))))
      (finally (.removeShutdownHook (Runtime/getRuntime) hook)))))

;; Item show: what status clips. The intent and gate in full, then the item's
;; events: its own, its spawns' (bar progress), and notes and asks naming it.

(defn- clip-to [n s]
  (let [s (str/replace (str s) #"\s+" " ")]
    (if (> (count s) n) (str (subs s 0 (dec n)) "…") s)))

(defn- item-event? [id spawn-ids]
  (let [named (re-pattern (str "(?<![A-Za-z0-9])" id "(?![0-9])"))]
    (fn [{:keys [type] :as e}]
      (or (= id (:item/id e))
          (and (spawn-ids (:spawn/id e)) (not= :spawn/progress type))
          (and (#{:note/added :ask/opened :ask/answered} type)
               (some #(and % (re-find named %)) [(:text e) (:question e) (:answer e)]))))))

(defn- item-event-line [e]
  (str (subs (:t e) 0 16) " " (namespace (:type e)) "/" (name (:type e)) " "
       (clip-to 240 (case (:type e)
                      :measure/ran (str (:measure/id e) " " (status/measure-text e)
                                        (when (:env e) (str " · " (status/env-text (:env e)))))
                      :item/measuring (status/spec-text e)
                      (str/join " " (keep #(when-some [v (e %)] (case % :exit (str "exit " v) (if (keyword? v) (name v) v)))
                                          [:spawn/id :gate/id :note/id :ask/id :disposition :exit
                                           :command :gate :measure :text :question :answer :why :result :tail]))))
       (when (:by e) (str " [" (:by e) "]"))))

(defn item-show [session [id]]
  (when (str/blank? id) (fail! "usage: lume item show <id>"))
  (let [events (store/read-events session)
        item (or (get-in (status/project events) [:items id]) (fail! "no such item:" id))
        related (filter (item-event? id (set (:spawns item))) events)
        shown (take-last 20 related)]
    (str/join "\n"
              (concat
               [(str id " " (name (:state item)) (when (:why item) (str " — " (clip-to 200 (:why item)))))
                (str "intent  " (clip-to 2000 (:intent item)))
                (str "gate    " (or (:gate item) "none"))]
               (when (:measure item) [(str "measure " (status/spec-text item))])
               (when (> (count related) (count shown))
                 [(str "+" (- (count related) (count shown)) " earlier events — lume log --item " id " -n N")])
               (map item-event-line shown)))))

;; Install: set up the agent side of this machine. Safe to re-run; writes only
;; what differs and nothing inside any repo.

(defn install [[a b]]
  (let [agent (keyword (if (= "--agent" a) b "claude"))
        {:keys [files manual]} ((requiring-resolve 'lume.agents/setup) agent)]
    (str/join "\n"
              (concat
               (for [[path content] files]
                 (let [old (when (fs/exists? path) (slurp path))]
                   (if (= old content)
                     (str "unchanged " path)
                     (do (fs/create-dirs (fs/parent path))
                         (spit path content)
                         (str (if old "updated   " "wrote     ") path)))))
               manual))))

;; Past sessions: lume run makes several per repo. ls lists them; status and
;; log read any of them by id, and write nothing to it.

(defn- session-by-id
  "The session to read: the one named by id, else the current one."
  [root id]
  (or (if id (store/find-session root id) (store/current-session root))
      (fail! (if id
               (str "no session " id " for this repo; lume ls lists them")
               "no session for this repo; run lume init \"<goal>\""))))

(def ^:private ls-usage "usage: lume ls [--all]")

(defn ls [root args]
  (when (seq (remove #{"--all"} args)) (fail! ls-usage))
  (let [all (some #{"--all"} args)
        sessions (store/sessions root)
        current (some-> (store/current-session root) store/session-id)
        now (Instant/now)
        shown (if all sessions (take 10 sessions))]
    (if (empty? sessions)
      "no sessions for this repo; run lume init \"<goal>\" or lume run <spec.edn>"
      (str/join "\n" (concat (for [s shown]
                               (status/session-line (store/session-id s) (store/read-events s) now (= (store/session-id s) current)))
                             (when (> (count sessions) (count shown))
                               [(str "+" (- (count sessions) (count shown)) " more — lume ls --all")]))))))

(def ^:private log-usage
  "usage: lume log [-n N] [--session <id>] [--type <t>] [--item <id>] [--by <actor>] [--grep <re>]")

;; Filters narrow the events before -n takes the last N, so a query is as
;; bounded as the plain log. --type matches a prefix of ns/name (note, spawn/);
;; --item is item show's events plus its spawns' progress; --grep searches the
;; event's EDN. Output stays EDN: the hint for what -n left out is a comment.
(defn- log-filter [events {[t & ts] "--type" [iid & iids] "--item" [by & bys] "--by" [re & res] "--grep"}]
  (when (some seq [ts iids bys res]) (fail! log-usage))
  (let [re (when re (try (re-pattern re) (catch Exception e (fail! "bad --grep regex:" (ex-message e)))))
        spawn-ids (when iid
                    (set (:spawns (or (get-in (status/project events) [:items iid]) (fail! "no such item:" iid)))))
        of-item (when iid (item-event? iid spawn-ids))]
    (filterv (fn [e]
               (and (or (nil? t) (str/starts-with? (subs (str (:type e)) 1) t))
                    (or (nil? iid) (of-item e) (contains? spawn-ids (:spawn/id e)))
                    (or (nil? by) (= by (:by e)))
                    (or (nil? re) (re-find re (pr-str e)))))
             events)))

(defn show-log [root args]
  (let [[pos {[n & ns] "-n" [id & ids] "--session" :as flags}]
        (take-flags args #{"-n" "--session" "--type" "--item" "--by" "--grep"} log-usage)
        n (if n (parse-long n) 20)]
    (when (or (seq pos) (seq ns) (seq ids) (not (and n (<= 0 n)))) (fail! log-usage))
    (let [events (log-filter (store/read-events (session-by-id root id)) flags)
          shown (take-last n events)]
      (str/join "\n" (concat (when (> (count events) (count shown))
                               [(str ";; +" (- (count events) (count shown)) " earlier events — -n " (count events))])
                             (map pr-str shown))))))

;; Ledger: every measurement row, open and closed items alike, newest last,
;; each with its change against the row it was compared with when taken. A
;; row names its spec when it's the first shown for its item or the spec changed.

(def ^:private ledger-usage "usage: lume ledger [<item>] [-n N] [--session <id>]")

(defn ledger [root args]
  (let [[[id & extra] {[n & ns] "-n" [sid & sids] "--session"}] (take-flags args #{"-n" "--session"} ledger-usage)
        n (if n (parse-long n) 20)
        _ (when (or (seq extra) (seq ns) (seq sids) (not (and n (<= 0 n)))) (fail! ledger-usage))
        {:keys [items ledger]} (status/project (store/read-events (session-by-id root sid)))
        _ (when (and id (not (items id))) (fail! "no such item:" id))
        rows (cond->> ledger id (filterv #(= id (:item/id %))))
        shown (take-last n rows)
        ;; Each shown row's spec, or nil when it's that of the item's previous shown row.
        specs (second (reduce (fn [[last-spec out] {iid :item/id :as row}]
                                (let [spec (status/spec-text row)]
                                  [(assoc last-spec iid spec) (conj out (when (not= spec (last-spec iid)) spec))]))
                              [{} []] shown))
        lines (map (fn [{mid :measure/id iid :item/id :keys [t] :as row} spec]
                     (str mid " " iid " " (subs t 0 16) " " (status/measure-text row)
                          (when spec (str " · " (clip-to 60 spec)))))
                   shown specs)]
    (if (empty? rows)
      (if id (str id " has no measurement rows") "no measurement rows")
      (str/join "\n" (concat (when (> (count rows) (count shown))
                               [(str "+" (- (count rows) (count shown)) " earlier rows — lume ledger"
                                     (str/join (map #(str " " %) (remove nil? [id (when sid (str "--session " sid))])))
                                     " -n " (count rows))])
                             lines)))))

;; Clean: every trace of lume for this repo, or one session's. Lume writes
;; nothing inside the repo, so that is its state under LUME_HOME, plus any
;; refs/lume/* and git worktrees kept under that state. The log is the whole
;; truth, so workers and drivers may not delete it.

(def ^:private clean-usage "usage: lume clean [--session <id>] [--yes]")

(defn- clean-targets
  "What clean would remove: [kind path-or-ref] pairs, worktrees first."
  [root id]
  (let [dir (store/repo-dir root)]
    (if id
      (let [session (or (store/find-session root id)
                        (fail! (str "no session " id " for this repo; lume ls lists them")))
            last? (= [session] (store/sessions root))]
        (if last?
          [[:dir dir]]
          (cond-> [[:dir session]]
            (= id (some-> (store/current-session root) store/session-id))
            (conj [:file (str (fs/path dir "current"))]))))
      (concat (for [line (some-> (sh-out root "git" "worktree" "list" "--porcelain") str/split-lines)
                    :let [[_ path] (re-matches #"worktree (.*)" line)]
                    :when (and path (fs/starts-with? (fs/absolutize path) (fs/absolutize dir)))]
                [:worktree path])
              (for [ref (some-> (sh-out root "git" "for-each-ref" "--format=%(refname)" "refs/lume/") str/split-lines)]
                [:ref ref])
              ;; Deleting a ref leaves its directories behind, empty.
              (when-let [git (sh-out root "git" "rev-parse" "--git-common-dir")]
                (let [refs (str (fs/path root git "refs" "lume"))]
                  (when (fs/exists? refs) [[:dir refs]])))
              (when (fs/exists? dir) [[:dir dir]])))))

(defn clean [root args]
  (let [[pos {[id & ids] "--session"}] (take-flags (remove #{"--yes"} args) #{"--session"} clean-usage)
        yes (some #{"--yes"} args)]
    (when (or (seq pos) (seq ids)) (fail! clean-usage))
    (when (re-matches #"[sd]\d+" (actor))
      (fail! "refused: workers and drivers don't delete the log; ask with lume ask \"<question>\""))
    (let [targets (clean-targets root id)
          text (fn [[kind x]] (str (name kind) " " x))]
      (cond (empty? targets) "nothing to clean: lume has no state for this repo"
            (not yes) (fail! (str/join "\n" (concat ["refused without --yes; lume clean would remove:"]
                                                    (map #(str "  " (text %)) targets))))
            :else (str/join "\n" (for [[kind x :as t] targets]
                                   (do (case kind
                                         :worktree (p/shell {:dir root :out :string :err :string}
                                                            "git" "worktree" "remove" "--force" x)
                                         :ref (p/shell {:dir root :out :string :err :string}
                                                       "git" "update-ref" "-d" x)
                                         :dir (fs/delete-tree x)
                                         :file (fs/delete-if-exists x))
                                       (str "removed " (text t)))))))))

;; Specs: a harness as one page of EDN. check validates a file; spec prints the
;; current session's harness, folded from the log, so it can be saved and reused.

(def ^:private check-usage "usage: lume check <spec.edn>")

(defn- checked-spec
  "The spec in the file at path, or a failure listing every problem with it."
  [path]
  (let [[s unreadable] ((requiring-resolve 'lume.spec/read-spec) path)
        ps (if unreadable [unreadable] ((requiring-resolve 'lume.spec/problems) s))
        shown (take 20 ps)]
    (when (seq ps)
      (fail! (str/join "\n" (concat [(str path ": " (count ps) (if (= 1 (count ps)) " problem" " problems"))]
                                   (map #(str "  " (clip-to 200 ((requiring-resolve 'lume.spec/problem-text) %))) shown)
                                   (when (> (count ps) (count shown))
                                     [(str "  +" (- (count ps) (count shown)) " more")])))))
    s))

(defn- items-text [items]
  (str (count items) (if (= 1 (count items)) " item" " items")
       (when-let [m (seq (filter :measure items))] (str " · " (count m) " measured"))))

(defn check [[path & extra]]
  (when (or (str/blank? path) (seq extra)) (fail! check-usage))
  (str path ": ok · " (items-text (:items (checked-spec path)))))

(def ^:private spec-usage "usage: lume spec [--all]")

(defn show-spec [session args]
  (when (seq (remove #{"--all"} args)) (fail! spec-usage))
  (let [all (boolean (some #{"--all"} args))
        state (status/project (store/read-events session))
        closed (count (remove #(= :open (:state %)) (vals (:items state))))]
    (str ((requiring-resolve 'lume.spec/pretty) ((requiring-resolve 'lume.spec/from-state) state :all all))
         (when (and (not all) (pos? closed))
           (str "\n;; " closed " closed " (if (= 1 closed) "item" "items") " left out — lume spec --all")))))

;; Run: a spec starts a session, as the events its commands would log, then
;; baselines each measurement and drives. The harness is the same whoever runs
;; it; --turns 0 stops after the baselines, for a human to drive by hand. A
;; spec's policy is the human's as on the command line: its allow, and a
;; cadence above the default ceiling. A fresh session has no limits, so a
;; budget only adds them, which anyone may.

(def ^:private run-usage "usage: lume run <spec.edn> [\"<goal>\"] [--turns N] [--agent claude]")

(defn- spec-policy
  "The human-only parts of a spec, as text for the confirmation, or nil."
  [{:keys [allow cadence]}]
  (let [raises (when cadence
                 (status/cadence-raises status/default-cadence (merge status/default-cadence cadence)))]
    (not-empty (str/join "; " (cond-> []
                                (seq allow) (conj (str "workers may run " (str/join " · " (distinct allow))))
                                raises (conj (str "the cadence ceiling is raised: " raises)))))))

(defn run-spec [root args]
  (let [[[path goal & extra] {[turns & ts] "--turns" [agent & as] "--agent"}]
        (take-flags args #{"--turns" "--agent"} run-usage)
        n (if turns (parse-long turns) 10)]
    (when (or (str/blank? path) (and goal (str/blank? goal)) (seq extra) (seq ts) (seq as)
              (not (and n (<= 0 n))))
      (fail! run-usage))
    (let [sp (cond-> (checked-spec path) goal (assoc :goal goal))
          say #(do (println %) (flush))]
      (when-let [policy (spec-policy sp)]
        (human-only! (str "only the human runs a spec that sets policy (" policy "); remove it from the spec,"
                          " or ask with lume ask \"<question>\"")
                     (str "Start a session where " policy ".") "yes"))
      ;; Run from inside a session (a worker or driver), the new one starts
      ;; alongside it: the repo's current session stays the human's.
      (let [within store/*session*
            session (store/new-session! root :current (not within))]
        (doseq [e ((requiring-resolve 'lume.spec/events) sp root)]
          (store/append! session (constantly (assoc e :by (actor)))))
        (say (str "session " (store/session-id session) " started from " path " · " (items-text (:items sp))))
        (when within
          (say (str "not made current: run inside " (store/session-id within)
                    "; lume status " (store/session-id session) " reads it")))
        (binding [store/*session* (when within session)]
          (doseq [item (->> (vals (:items (status/project (store/read-events session)))) (filter :measure) (sort-by :order))]
            (say (str "baseline " (measure-row session (run-measure session item)))))
          (if (zero? n)
            "--turns 0: not driving; lume drive, or drive by hand from lume status"
            (drive session root (cond-> ["--turns" (str n)] agent (conj "--agent" agent)))))))))

(defn- terminal? []
  ;; Java 22+ Console.isTerminal; before that, a console meant a terminal.
  (when-let [c (System/console)]
    (try (.isTerminal c) (catch Exception _ true))))

(defn color?
  "Colour is for a human at a terminal (NO_COLOR opts out), or forced with
  --color for pipes that render it, like watch --color."
  [flag? terminal? no-color]
  (boolean (or flag? (and terminal? (str/blank? no-color)))))

(def ^:private status-usage "usage: lume status [<session>] [--color]")

(defn show-status [root args]
  (let [[[id & stray] _] (take-flags (remove #{"--color"} args) #{} status-usage)
        _ (when (seq stray) (fail! status-usage))
        session (session-by-id root id)]
    (status/render (store/session-id session) (store/read-events session) (Instant/now)
                   {:color (color? (some #{"--color"} args) (terminal?) (System/getenv "NO_COLOR"))
                    :past (not= (store/session-id session) (some-> (store/current-session root) store/session-id))})))

;; Replay: the session as it stood after its first N events, from the log
;; alone, with the clock stopped at event N so ages read as they did then. A
;; decision is a drive turn or a spawn; --step shows the screen after each in
;; turn, for a human at a terminal. Agents get the list of decisions instead,
;; each with the N that replays it.

(def ^:private replay-usage "usage: lume replay [<session>] [--at N] [--step] [-n N] [--color]")

(defn- decision-index [events from n]
  (let [ds (filter (fn [[_ e]] (status/decision? e)) (drop from (map-indexed (fn [i e] [(inc i) e]) events)))
        shown (take-last n ds)]
    (if (empty? ds)
      (str "no decisions" (when (pos? from) (str " after event " from)) " (drive turns or spawns); "
           (count events) " events — lume replay --at N")
      (str/join "\n" (concat (when (> (count ds) (count shown))
                               [(str "+" (- (count ds) (count shown)) " earlier decisions — -n " (count ds))])
                             (for [[i e] shown]
                               (clip-to 100 (str/replace (str i " " (subs (:t e) 0 16) " " (status/decision-text e)) #"\s+" " ")))
                             [(str "lume replay --at N: the screen after event N (of " (count events) ") · --step: each decision in turn")])))))

(defn replay [root args]
  (let [[[id & stray] {[at & ats] "--at" [n & ns] "-n"}]
        (take-flags (remove #{"--color" "--step"} args) #{"--at" "-n"} replay-usage)
        step (some #{"--step"} args)
        color (color? (some #{"--color"} args) (terminal?) (System/getenv "NO_COLOR"))
        n (if n (parse-long n) 20)
        at (when at (or (parse-long at) (fail! replay-usage)))
        _ (when (or (seq stray) (seq ats) (seq ns) (not (and n (<= 0 n)))) (fail! replay-usage))
        session (session-by-id root id)
        sid (store/session-id session)
        events (store/read-events session)
        m (count events)
        _ (when (and at (not (<= 1 at m))) (fail! (str "--at " at " is outside the log: events 1.." m)))
        screen (fn [i] (status/render sid (subvec events 0 i) (Instant/parse (:t (nth events (dec i))))
                                      {:color color :replay {:of m}}))]
    (cond
      (and at (not step)) (screen at)
      (not step) (decision-index events 0 n)
      :else
      (let [ds (keep-indexed (fn [i e] (when (and (status/decision? e) (>= (inc i) (or at 1))) (inc i))) events)]
        (loop [[i & more] ds, k 1]
          (cond
            (nil? i) (if (= 1 k) (decision-index events (dec (or at 1)) n) (str "end of replay: " (dec k) (if (= 2 k) " decision" " decisions")))
            :else
            (let [typed (*read-line* (str (when color "\u001b[2J\u001b[H") (screen i))
                                     (str "decision " k " of " (count ds) " · event " i " of " m
                                          " · enter: next, q: quit "))]
              (cond (nil? typed) (if (= 1 k)
                                   (decision-index events (dec (or at 1)) n) ; no terminal to step at
                                   (str "stopped at event " i))
                    (= "q" (str/trim typed)) (str "stopped at event " i " — lume replay --at " i)
                    :else (recur more (inc k))))))))))

;; Repl: an nREPL on a session, for prototyping a fold or a screen against its
;; real log before writing the code. The namespace gets the events, their
;; projection and the lume namespaces; it logs repl/opened and nothing else.

(def ^:private repl-usage "usage: lume repl [<session>] [--port N]")

(defn repl-ns!
  "Set up ns-sym for working on session: events, state, render, refresh!,
  measure and the lume namespaces aliased. refresh! re-reads the log, for a
  live session; measure is try-measure on it."
  [ns-sym session]
  (let [sid (store/session-id session)]
    (binding [*ns* (create-ns ns-sym)]
      (refer-clojure)
      (require '[clojure.string :as str] '[lume.store :as store] '[lume.status :as status]
               '[lume.spec :as spec] '[lume.agents :as agents] '[lume.cli :as cli]))
    (letfn [(refresh! []
              (let [events (store/read-events session)]
                (intern ns-sym 'events events)
                (intern ns-sym 'state (status/project events))
                (count events)))
            (render
              ([] (status/render sid @(ns-resolve ns-sym 'events) (Instant/now) {}))
              ([n] (let [events @(ns-resolve ns-sym 'events)]
                     (status/render sid (subvec events 0 n) (Instant/parse (:t (nth events (dec n))))
                                    {:replay {:of (count events)}}))))]
      (intern ns-sym 'session session)
      (intern ns-sym 'sid sid)
      (intern ns-sym 'refresh! refresh!)
      (intern ns-sym 'render render)
      (intern ns-sym 'measure (fn measure
                                ([cmd] (measure cmd {}))
                                ([cmd opts] (try-measure session cmd opts))))
      (refresh!))))

(defn start-repl
  "Start an nREPL whose user namespace (or opts :ns) is set up on session.
  Returns {:server :port :text}; stop it with babashka.nrepl.server/stop-server!."
  [session {:keys [port ns] :or {port 0 ns 'user}}]
  (let [server ((requiring-resolve 'babashka.nrepl.server/start-server!) {:port port :quiet true})
        port (.getLocalPort ^java.net.ServerSocket (:socket server))
        ;; The one write: the log shows prototyping happened, e.g. before a spec.
        _ (store/append! session (constantly {:type :repl/opened :port port :by (actor)}))
        n (repl-ns! ns session)]
    {:server server
     :port port
     :text (str/join "\n"
                     [(str "lume repl · " (store/session-id session) " · " n " events · nREPL on 127.0.0.1:" port)
                      (str "connect: clj-nrepl-eval -p " port " \"(count events)\"")
                      (str "in " ns ": events, state (status/project events), (render) status now, (render N) after event N,")
                      "  (refresh!) re-reads the log, (measure \"<cmd>\" {:runs N :drop K :higher [..]}) runs a"
                      "  candidate measurement as a spec would and returns the row, unlogged;"
                      "  aliases store status spec agents cli str"
                      "runs until killed; logs repl/opened and writes nothing else"])}))

(defn repl [root args]
  (let [[[id & stray] {[port & ports] "--port"}] (take-flags args #{"--port"} repl-usage)
        port (if port (or (parse-long port) (fail! repl-usage)) 0)
        _ (when (or (seq stray) (seq ports)) (fail! repl-usage))
        {:keys [text]} (start-repl (session-by-id root id) {:port port})]
    (println text)
    (flush)
    @(promise)))

(def usage
  "lume install [--agent claude]               set up agents on this machine (a lume skill); safe to re-run
lume init \"<goal>\" [--gate \"<cmd>\"] [--allow \"<cmd>\"]...
                                            start a session; items added without --gate get its --gate
lume status [<session>] [--color]           one screen: goal, items, notes, recent events (colour on a tty);
                                            a past session's by id (lume ls)
lume item add \"<intent>\" [--gate \"<cmd>\"]   add a work item; the gate's exit code decides done
lume item gate <id> \"<cmd>\"                 set or replace an open item's gate
lume item measure <id> \"<cmd>\" [--runs N] [--drop K] [--higher <name>]...
                                            measure an item after each spawn: output's name=number pairs
                                            (or else its last number) and wall time become a ledger row;
                                            N runs, first K dropped, give medians and min..max; lower is
                                            better unless --higher names it (shown name↑)
lume measure <id>                           run the item's measurement now and print its row (a baseline)
lume ledger [<item>] [-n N] [--session <id>] measurement rows, newest last (default 20), open and closed items:
                                            numbers with change vs the row compared, spread, wall time, exit
lume item close <id> done|dropped \"<why>\"   record a disposition; done runs the gate, refused if it fails
lume item show <id>                         one item in full: intent, gate, disposition and its events
lume note [--pin] \"<text>\"                  steering for drivers and workers; pinned notes never age out
lume note --unpin <id>                      let a pinned note age out like the rest
lume note show <id>                         one note in full: text, who noted it and when, pinned or not
lume ask \"<question>\"                       put a question to the human; open asks top status
lume ask                                    open asks in full, wrapped to the terminal, each with its answer command
lume ask show <id>                          one ask in full: question, who asked and when, its item, its answer
lume answer <id> [\"<text>\"]                 answer an ask (human at a tty: retype the id); no text: prompt for it
                                            after the full question. Answers join the notes
lume allow [--revoke] \"<cmd>\"               human at a tty, types yes: let workers run commands starting with <cmd> (their gate is always allowed)
lume budget [--usd N] [--spawns N] [--minutes N]
                                            limit the session's agent spend (spawns and drive turns); with no
                                            flags, print spent/limit. Lowering is anyone's; raising, the human's
lume cadence [every-spawn | batch N] [on gate-fail] [on plateau K]
                                            when drive hands back: N spawn requests a turn, run in order;
                                            a failed gate ends the batch; K rows without a new best (lower
                                            is better, unless --higher) ask the human. No rules: print it. Above the human's
                                            latest cadence is the human's
lume spawn <id> [--agent claude]            run a worker on an item, in the foreground (a driver's is a request)
lume drive [--turns N] [--agent claude]     driver turns until an ask is open, a budget is reached, no items are open, or N (10) run
lume check <spec.edn>                       validate a harness spec: every problem, one per line, exit 1 on any
lume spec [--all]                           this session's harness as spec EDN (open items; --all, closed too)
lume run <spec.edn> [\"<goal>\"] [--turns N] [--agent claude]
                                            check a spec, start a session from it, baseline its measurements,
                                            then drive (--turns 0: don't). A spec's allow, or a cadence above
                                            every-spawn, is the human's. Run by a worker or driver, the new
                                            session starts alongside; the current one stays
lume ls [--all]                             this repo's sessions, newest first (* current): goal, open/closed
                                            items, last event, outcome; 10 unless --all
lume log [-n N] [--session <id>] [--type <t>] [--item <id>] [--by <actor>] [--grep <re>]
                                            raw events as EDN (default last 20), of a past session by id;
                                            filters combine: type prefix (note, spawn/), an item's events,
                                            actor, regex over the event; a ;; line counts what -n left out
lume replay [<session>] [--at N] [--step]   status as it stood after event N, from the log alone; no flags:
                                            the decisions (drive turns, spawns), each with its N (-n, 20);
                                            --step: the screen after each decision in turn, at a terminal
lume repl [<session>] [--port N]            an nREPL (free port) with the session's events, state, render
                                            and lume.* in user; prints the port; writes nothing; runs until killed
lume clean [--session <id>] [--yes]         remove every trace of lume for this repo (or one session):
                                            its state, refs/lume/*, worktrees; lists them, refused without --yes")

(defn run [args]
  (let [;; Finding the repo runs git, so only commands that need it do: a
        ;; worker's lume note has its session from LUME_SESSION.
        root (delay (store/repo-root))
        [cmd sub & more] args
        session #(or store/*session* (store/current-session @root)
                     (fail! "no session for this repo; run lume init \"<goal>\""))]
    (case cmd
      "init" (init @root (rest args))
      "install" (install (rest args))
      "status" (if (#{"-h" "--help"} sub) status-usage (show-status @root (rest args)))
      "item" (case sub
               "add" (item-add (session) more)
               "close" (item-close (session) more)
               "gate" (item-gate (session) more)
               "show" (item-show (session) more)
               "measure" (item-measure (session) more)
               (fail! "usage: lume item add|gate|measure|close|show ..."))
      "measure" (if (#{"-h" "--help"} sub) measure-usage (measure (session) (rest args)))
      "ledger" (if (#{"-h" "--help"} sub) ledger-usage (ledger @root (rest args)))
      "note" (if (and (= "show" sub) (= 2 (count (rest args))))
               (note-show (session) (drop 2 args))
               (note (session) (rest args)))
      "ask" (if (and (= "show" sub) (= 2 (count (rest args))))
              (ask-show (session) (drop 2 args))
              (ask (session) (rest args)))
      "answer" (answer (session) (rest args))
      "allow" (allow (session) (rest args))
      "budget" (if (#{"-h" "--help"} sub)
                 "usage: lume budget [--usd N] [--spawns N] [--minutes N]"
                 (budget (session) (rest args)))
      "cadence" (if (#{"-h" "--help"} sub)
                  cadence-usage
                  (cadence (session) (rest args)))
      "check" (if (#{"-h" "--help"} sub) check-usage (check (rest args)))
      "spec" (if (#{"-h" "--help"} sub) spec-usage (show-spec (session) (rest args)))
      "run" (if (#{"-h" "--help"} sub) run-usage (run-spec @root (rest args)))
      "spawn" (spawn (session) @root (rest args))
      "drive" (drive (session) @root (rest args))
      "log" (if (#{"-h" "--help"} sub) log-usage (show-log @root (rest args)))
      "ls" (if (#{"-h" "--help"} sub) ls-usage (ls @root (rest args)))
      "replay" (if (#{"-h" "--help"} sub) replay-usage (replay @root (rest args)))
      "repl" (if (#{"-h" "--help"} sub) repl-usage (repl @root (rest args)))
      "clean" (if (#{"-h" "--help"} sub) clean-usage (clean @root (rest args)))
      (nil "help" "-h" "--help") usage
      (fail! "unknown command:" cmd "\n\n" usage))))

(defn -main [& args]
  (try
    (println (run args))
    (catch clojure.lang.ExceptionInfo e
      (if (:lume/user-error (ex-data e))
        (do (binding [*out* *err*] (println (ex-message e)))
            (System/exit 1))
        (throw e)))))
