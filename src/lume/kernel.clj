(ns lume.kernel
  "What every lume command shares: who is acting, failing with a user error,
  parsing flags, human-only confirmation, and running shell commands in the
  repo with their output kept under the session."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [lume.store :as store])
  (:import (java.time Duration Instant ZoneId)
           (java.time.format DateTimeFormatter)))

(defn fail! [& msg]
  (throw (ex-info (str/join " " msg) {:lume/user-error true})))

(defn next-id [prefix type events]
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

(defn actor [] *actor*)

(def ^:dynamic *default-agent*
  "The agent spawn, drive, run and install use without --agent: LUME_AGENT, else
  claude. Set it once on a machine whose agent CLI isn't claude. Tests bind it."
  (or (not-empty (System/getenv "LUME_AGENT")) "claude"))

(defn driver? [] (boolean (re-matches #"d\d+" (actor))))

;; Human-only commands need more than LUME_ACTOR, which an agent running as your
;; uid can forge (a1): an interactive stdin and a typed confirmation. Spawns and
;; drives run with no tty, so they can't pass.
(defn tty? []
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

(defn columns
  "The terminal's width, for text a human reads in full: stty, then COLUMNS, then 100."
  []
  (or (some->> (p/shell {:in :inherit :out :string :err :string :continue true} "stty" "size")
               :out (re-find #"^\d+ (\d+)") second parse-long (max 40))
      (some-> (System/getenv "COLUMNS") parse-long)
      100))

(defn human-only!
  "Refuse unless the actor is the human and a human at a terminal confirms."
  [refusal prompt expected]
  (when-not (= "human" (actor)) (fail! refusal))
  (when-not (*confirm* prompt expected)
    (fail! "refused: human-only commands need an interactive terminal and a typed confirmation;"
           "agents ask with lume ask \"<question>\"")))

;; Features: what lume does, as data. A feature is a map
;;   {:name   "item"
;;    :events {type {:req #{keys} :human true?}}  the types it appends
;;    :init   {..}                                 its part of the state
;;    :fold   {type (fn [state e] state)}          how its events fold
;;    :guards {type (fn [state e] why-or-nil)}}    what refuses an append
;; bound per run in *features*, never a global, so parallel tests bind their
;; own. Every type lume appends is declared by a feature with the keys it must
;; carry; :human types are policy, logged only when the human acts. Reading
;; stays lenient (project skips what no feature folds), so old logs still fold.

(def ^:dynamic *features*
  "The features in play, in order. Unbound until a run (cli/-main) or a test binds it."
  nil)

(defn features []
  (or *features* (throw (IllegalStateException. "no features bound: bind lume.kernel/*features*"))))

(defn events
  "Every declared event type: {type {:req #{keys} :human true?}}."
  []
  (apply merge (map :events (features))))

(defn read-events
  "The session's log, oldest first."
  [session]
  (store/read-events session))

(defn session-id [session] (store/session-id session))

(defn project
  "Fold the log into the current state of the session: each event through the
  folds for its type, in feature order, starting from the features' :init."
  [events]
  (let [fs (features)
        folds (apply merge-with into (for [f fs [type fold] (:fold f)] {type [fold]}))]
    (reduce (fn [s e] (reduce (fn [s fold] (fold s e)) s (get folds (:type e))))
            (apply merge (map :init fs))
            events)))

(defn- checked
  "e with :by filled from the actor, or a refusal: no feature declares it, it
  lacks a required key, it is the human's, or a guard refuses it in (state)."
  [{:keys [type] :as e} state]
  (let [{:keys [req human] :as decl} (get (events) type)
        missing (sort (filter #(nil? (get e %)) req))
        guards (keep #(get-in % [:guards type]) (features))]
    (cond (nil? decl) (fail! "refused: undeclared event type" (pr-str type))
          (seq missing) (fail! "refused:" (pr-str type) "needs" (str/join " " (map pr-str missing)))
          (and human (not= "human" (actor))) (fail! "refused:" (pr-str type) "is the human's, not" (str (actor) "'s"))
          :else (let [e (merge {:by (actor)} e)]
                  (when (seq guards)
                    (let [s (state)]
                      (some-> (some #(% s e) guards) fail!)))
                  e))))

;; Reactions: a feature's :reactions [{:on type-or-prefix :run (fn [e])}] run
;; after append! writes an event, in the appending process, with *session*
;; bound to the event's session. :on is a type (:ask/opened) or a namespace
;; (:ask matches every :ask/… type). :reactions may instead be a fn of the
;; session's state returning them, for reactions the log declares (rules).
;; A reaction's own appends react in turn, nested at most max-depth deep; a
;; reaction that runs lume in a subprocess passes *depth* on as LUME_DEPTH.
;; Workers and drivers never run reactions, since secrets like LUME_NTFY are
;; stripped from them (child-env): the spawn or drive turn that started one
;; runs them for its events when it exits.

(def max-depth 3)

(def ^:dynamic *depth*
  "How deep in reactions this append is: LUME_DEPTH from a parent's reaction, else 0."
  (or (some-> (System/getenv "LUME_DEPTH") not-empty parse-long) 0))

(def ^:dynamic *session*
  "The session whose event the running reactions see; nil outside them."
  nil)

(def ^:dynamic *spawned-as*
  "A spawn or drive id when this process is a worker or driver lume started
  (LUME_SPAWNED_AS), else nil: then reactions wait for the parent. Tests bind it."
  (not-empty (System/getenv "LUME_SPAWNED_AS")))

(defn- reacts? [on type]
  (or (= on type)
      (and (nil? (namespace on)) (= (name on) (namespace type)))))

(defn react!
  "Run every reaction matching e, unless this process is a worker or driver
  or the reactions are nested max-depth deep."
  [session e]
  (when (and (not *spawned-as*) (< *depth* max-depth))
    (binding [*depth* (inc *depth*) *session* session]
      (let [state (delay (project (store/read-events session)))]
        (doseq [{:keys [on run]} (mapcat #(let [r (:reactions %)] (if (fn? r) (r @state) r)) (features))
                :when (reacts? on (:type e))]
          (run e))))))

(defn react-for!
  "Run the reactions for the events who (a worker or driver this process
  started, now exited) appended: its own process couldn't."
  [session who]
  (doseq [e (store/read-events session) :when (= who (:by e))]
    (react! session e)))

(defn append!
  "store/append!, checked under the lock: the event (f events) builds must be
  a declared type with its required keys, a :human type only the human
  appends, and no feature's guard may refuse it. :by is the actor unless the
  event names one. Once written, the event's reactions run (outside the lock)."
  [session f]
  (let [e (store/append! session (fn [events] (some-> (f events) (checked #(project events)))))]
    (when e (react! session e))
    e))

;; Commands are data too: a feature's :commands are
;;   {:path ["item" "measure"]   the words that name it
;;    :needs :session            what its ctx holds: :session, :root, :none, or a set of them
;;    :usage "usage: lume .."    printed for -h/--help, and its usage errors
;;    :help "lume item .."       its entry in lume help
;;    :run (fn [ctx args] text)} ctx {:session dir :root repo}, args after the path
;; Finding the repo runs git, so only commands that need it do: a worker's
;; lume note has its session from LUME_SESSION.

(defn commands [] (mapcat :commands (features)))

(defn help
  "lume help: every command's :help, in feature order."
  []
  (str/join "\n" (keep :help (commands))))

(defn- ctx [needs]
  (let [needs (if (keyword? needs) #{needs} (set needs))
        root (delay (store/repo-root))]
    (cond-> {}
      (needs :root) (assoc :root @root)
      (needs :session) (assoc :session (or store/*session* (store/current-session @root)
                                           (fail! "no session for this repo; run lume init \"<goal>\""))))))

(defn run
  "Run the command whose :path is the longest prefix of args, and return what
  it prints: its :usage for -h/--help, lume help with no command."
  [args]
  (let [cmds (commands)
        cmd (first (sort-by #(- (count (:path %)))
                            (filter #(= (:path %) (take (count (:path %)) args)) cmds)))
        [word] args]
    (cond cmd (let [args (drop (count (:path cmd)) args)]
                (if (#{"-h" "--help"} (first args))
                  (:usage cmd)
                  ((:run cmd) (ctx (:needs cmd)) args)))
          (contains? #{nil "help" "-h" "--help"} word) (help)
          :else (let [subs (keep #(when (= word (first (:path %))) (second (:path %))) cmds)]
                  (if (seq subs)
                    (fail! (str "usage: lume " word " " (str/join "|" subs) " ..."))
                    (fail! "unknown command:" word "\n\n" (help)))))))

(defn open-item [events id]
  (let [item (get-in (project events) [:items id])]
    (cond (nil? item) (fail! "no such item:" id)
          (not= :open (:state item)) (fail! id "is already" (name (:state item)))
          :else item)))

(defn take-flags
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

(defn flag [opts name]
  (second (drop-while #(not= name %) opts)))

(defn secs [ms]
  (format "%.1fs" (/ ms 1000.0)))

(defn clock
  "An event's :t as local HH:mm."
  [t]
  (.format (.atZone (Instant/parse t) (ZoneId/systemDefault)) (DateTimeFormatter/ofPattern "HH:mm")))

(defn clip-to [n s]
  (let [s (str/replace (str s) #"\s+" " ")]
    (if (> (count s) n) (str (subs s 0 (dec n)) "…") s)))

(defn ago [now t]
  (let [secs (.getSeconds (Duration/between (Instant/parse t) now))]
    (cond (< secs 60) (str secs "s ago")
          (< secs 3600) (str (quot secs 60) "m ago")
          (< secs 86400) (str (quot secs 3600) "h ago")
          :else (str (quot secs 86400) "d ago"))))

;; Colour is for humans at a terminal. It wraps text that is already clipped
;; and padded, so it never changes visible width or line count. status/render
;; binds it.
(def ^:dynamic *color* false)

(def ^:private sgr {:dim "2" :yellow "33" :green "32" :red "31" :cyan "36"})

(defn paint [style s]
  (if *color* (str "\u001b[" (sgr style) "m" s "\u001b[0m") s))

(defn session-by-id
  "The session to read: the one named by id, else the current one."
  [root id]
  (or (if id (store/find-session root id) (store/current-session root))
      (fail! (if id
               (str "no session " id " for this repo; lume ls lists them")
               "no session for this repo; run lume init \"<goal>\""))))

;; The ntfy URLs never reach a child process (n8: env leaks into workers): a
;; worker that could read the notification topic could read an ask's code,
;; and one that could write the reply topic could answer its own ask. So lume
;; itself notifies for the asks its workers open.
(defn child-env
  "env for a process lume starts, with every LUME_NTFY* emptied."
  [env]
  (merge (into {"LUME_NTFY" "" "LUME_NTFY_REPLY" ""}
               (for [k (keys (System/getenv)) :when (str/starts-with? k "LUME_NTFY")] [k ""]))
         env))

(defn sh-out
  "cmd's trimmed stdout, run in root, or nil when it fails or prints nothing."
  [root & cmd]
  (try (let [{:keys [exit out]} (apply p/shell {:dir root :in "" :out :string :err :string :continue true} cmd)]
         (when (zero? exit) (not-empty (str/trim out))))
       (catch Exception _ nil)))

;; Gates and measurements: shell commands run in the repo, their output kept
;; under the session.

(defn run-repeated
  "Run cmd with sh -c in root up to n times, stopping at the first failure;
  returns the runs, {:exit :ms :output} in order. (on-run i run) sees each."
  [session root cmd n on-run]
  (reduce (fn [runs i]
            (let [t0 (System/currentTimeMillis)
                  {:keys [exit out]} @(p/process ["sh" "-c" cmd] {:dir root :in "" :out :string :err :out
                                                                   :extra-env (child-env {"LUME_SESSION" session})})
                  run {:exit exit :ms (- (System/currentTimeMillis) t0) :output out}
                  r (conj runs run)]
              (on-run i run)
              (if (zero? exit) r (reduced r))))
          [] (range n)))

(defn run-in-repo!
  "Run cmd in the repo up to n times (1 by default), stopping at the first
  failure, with all output in one file in the session's `kind` dir. Log the
  event that (event-for events runs) returns as [id event], where runs are
  {:exit :ms :output} in order; the last run's :exit and :ms, the output's
  :tail and :out are added unless the event has its own. The id is allocated
  under the log lock."
  [session kind cmd event-for & {:keys [n] :or {n 1}}]
  (let [root (:repo (project (store/read-events session)))
        dir (fs/create-dirs (fs/path session kind))
        tmp (fs/file (fs/create-temp-file {:dir dir :prefix "running-"}))
        runs (run-repeated session root cmd n
                           (fn [i {:keys [exit ms output]}]
                             (spit tmp (str (when (> n 1) (str "--- run " (inc i) "/" n " · exit " exit " · " (secs ms) "\n"))
                                            output)
                                   :append true)))
        {:keys [exit ms output]} (peek runs)
        tail (last (remove str/blank? (str/split-lines output)))
        ran (append! session (fn [events]
                               (let [[id e] (event-for events runs)]
                                 (cond-> (merge {:exit exit :ms ms} e
                                                {:out (str (fs/path dir (str id ".out"))) :by (actor)})
                                   tail (assoc :tail tail)))))]
    (fs/move tmp (:out ran))
    ran))
