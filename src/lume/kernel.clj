(ns lume.kernel
  "What every lume command shares: who is acting, failing with a user error,
  parsing flags, human-only confirmation, and running shell commands in the
  repo with their output kept under the session."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [lume.store :as store])
  (:import (java.security MessageDigest)
           (java.time Duration Instant ZoneId)
           (java.time.format DateTimeFormatter)))

(defn fail! [& msg]
  (throw (ex-info (str/join " " msg) {:lume/user-error true})))

;; Process and time helpers.

(defn sh
  "Run cmd quietly, and return {:exit :out :err} with both outputs as strings,
  whatever its exit. stdin is empty unless opts (babashka.process options, as
  :dir or :in) say otherwise."
  [opts & cmd]
  (apply p/shell (merge {:in "" :out :string :err :string :continue true} opts) cmd))

(defn timed
  "[(f) ms]: what f returns and how many milliseconds it took."
  [f]
  (let [t0 (System/currentTimeMillis)
        r (f)]
    [r (- (System/currentTimeMillis) t0)]))

(defn sha256-hex
  "The lowercase hex SHA-256 of the byte arrays, in order."
  [& byte-arrays]
  (let [md (MessageDigest/getInstance "SHA-256")]
    (doseq [^bytes b byte-arrays] (.update md b))
    (apply str (map #(format "%02x" (bit-and % 0xff)) (.digest md)))))

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
  (zero? (:exit (sh {:in :inherit} "test" "-t" "0"))))

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
  (or (some->> (sh {:in :inherit} "stty" "size")
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
;;    :guards {type (fn [state e] why-or-nil)}     what refuses an append
;;    :event-text {type (fn [e] text)}}            optional: how an event reads (event-text)
;; bound per run in *features*, never a global, so parallel tests bind their
;; own. Every type lume appends is declared by a feature with the keys it must
;; carry; :human types are policy, logged only when the human acts. Reading
;; stays lenient (project skips what no feature folds), so old logs still fold.

(def ^:dynamic *features*
  "The features in play, in order. Unbound until a run (cli/-main) or a test binds it."
  nil)

(defn features []
  (or *features* (throw (IllegalStateException. "no features bound: bind lume.kernel/*features*"))))

(def ns-lock
  "Held while lume changes namespaces at runtime (loading an extension, setting
  up a repl's). bb's require (an ns form's too) writes back the whole namespace
  map from a snapshot, so one running in another thread can drop interns made
  meanwhile; parallel tests do both at once."
  (Object.))

(defn events
  "Every declared event type: {type {:req #{keys} :human true?}}."
  []
  (apply merge (map :events (features))))

(defn read-events
  "The session's log, oldest first."
  [session]
  (store/read-events session))

(defn find-event
  "The first event in events of type whose id-key is id, or nil."
  [events type id-key id]
  (some #(when (and (= type (:type %)) (= id (id-key %))) %) events))

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

(defn state
  "The session's current state: its log, projected."
  [session]
  (project (store/read-events session)))

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

;; Reactions: a feature's :reactions [{:on type-or-prefix :run (fn [session e])}]
;; run after append! writes an event, in the appending process, given the
;; event's session. An extension's reactions are (fn [e]) and read *session*
;; instead: lume.ext adapts them. :on is a type (:ask/opened) or a namespace
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
  "The session whose event the running reactions see, for extensions'
  (fn [e]) reactions; nil outside them."
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
      (let [s (delay (state session))]
        (doseq [{:keys [on run]} (mapcat #(let [r (:reactions %)] (if (fn? r) (r @s) r)) (features))
                :when (reacts? on (:type e))]
          (run session e))))))

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

(defn exit-hook!
  "Append event to session if lume exits (ctrl-c, say) before the work
  finishes. Returns finished!, which marks the work finished and drops the
  hook; calling it again does nothing more."
  [session event]
  (let [done (promise)
        hook (Thread. ^Runnable (bound-fn []
                                  (when-not (realized? done)
                                    (append! session (constantly event)))))]
    (.addShutdownHook (Runtime/getRuntime) hook)
    (fn finished! []
      (deliver done true)
      (.removeShutdownHook (Runtime/getRuntime) hook))))

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
  "Split args into positionals and the values of the given flags, in order,
  plus true for each of the given switches (flags that take no value) present.
  A flag with no value, or any other flag-like arg, is a usage error."
  ([args flags usage] (take-flags args flags #{} usage))
  ([args flags switches usage]
   (loop [[a & more :as args] args, pos [], vals {}]
     (cond (empty? args) [pos vals]
           (switches a) (recur more pos (assoc vals a true))
           (flags a) (let [[v & more] more]
                       (when (or (str/blank? v) (str/starts-with? v "--")) (fail! usage))
                       (recur more pos (update vals a (fnil conj []) v)))
           (str/starts-with? a "--") (fail! usage)
           :else (recur more (conj pos a) vals)))))

(defn positionals
  "args as exactly n non-blank positionals and no flags, or a usage error."
  [args n usage]
  (let [[pos] (take-flags args #{} usage)]
    (when (or (not= n (count pos)) (some str/blank? pos)) (fail! usage))
    pos))

(defn secs [ms]
  (format "%.1fs" (/ ms 1000.0)))

(defn clock
  "An event's :t as local HH:mm."
  [t]
  (.format (.atZone (Instant/parse t) (ZoneId/systemDefault)) (DateTimeFormatter/ofPattern "HH:mm")))

(defn clip
  "s on one line (whitespace runs become a space), cut to at most n with … at
  the end when it was longer."
  [n s]
  (let [s (str/replace (str s) #"\s+" " ")]
    (if (> (count s) n) (str (subs s 0 (dec n)) "…") s)))

;; An event as text: its feature's :event-text for its type, else the values
;; of the keys most events carry, in this order.
(def ^:private event-keys
  [:drive/id :item/id :spawn/id :gate/id :measure/id :note/id :ask/id :disposition :exit :port
   :intent :gate :measure :command :text :question :answer :goal :why :result :tail])

(defn- event-keys-text [e]
  (str/join " " (keep #(when-some [v (e %)] (case % :exit (str "exit " v) (if (keyword? v) (name v) v)))
                      event-keys)))

(defn event-text
  "Event e's text: the :event-text fn a feature gives for its type, else its
  common keys' values."
  [e]
  ((or (some #(get-in % [:event-text (:type e)]) (features)) event-keys-text) e))

(defn event-line
  "Event e as one line: its :t through time-text, its type, then its text
  clipped to n."
  [time-text n e]
  (str (time-text (:t e)) " " (namespace (:type e)) "/" (name (:type e)) " " (clip n (event-text e))))

(def width "Status's width: no line is longer." 100)

(defn wrap
  "Lines of at most n, broken at spaces (a word longer than n is cut). With
  max-lines, the last line kept is clipped and ends in tail when there is more."
  ([s n] (wrap s n nil nil))
  ([s n max-lines tail]
   (let [lines (loop [words (str/split (str/trim (str/replace (str s) #"\s+" " ")) #" ")
                      line nil, out []]
                 (if-let [[w & more] (seq words)]
                   (cond (nil? line) (if (> (count w) n)
                                       (recur (cons (subs w n) more) nil (conj out (subs w 0 n)))
                                       (recur more w out))
                         (<= (+ (count line) 1 (count w)) n) (recur more (str line " " w) out)
                         :else (recur words nil (conj out line)))
                   (cond-> out line (conj line))))]
     (if (and max-lines (> (count lines) max-lines))
       (let [last-n (- n (count tail))]
         (conj (vec (take (dec max-lines) lines))
               (str (clip last-n (str/join " " (drop (dec max-lines) lines))) tail)))
       lines))))

(defn more
  "The footer for a bounded list: [\"+K <hint>\"] when shown leaves K of all
  out, else nil. Concat it where the list was cut; hint says how to see the rest."
  [all shown hint]
  (let [k (- (count all) (count shown))]
    (when (pos? k) [(str "+" k " " hint)])))

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
  (try (let [{:keys [exit out]} (apply sh {:dir root} cmd)]
         (when (zero? exit) (not-empty (str/trim out))))
       (catch Exception _ nil)))

;; Gates and measurements: shell commands run in the repo, their output kept
;; under the session.

(defn run-repeated
  "Run cmd with sh -c in root up to n times, stopping at the first failure;
  returns the runs, {:exit :ms :output} in order. (on-run i run) sees each."
  [session root cmd n on-run]
  (reduce (fn [runs i]
            (let [[{:keys [exit out]} ms] (timed #(deref (p/process ["sh" "-c" cmd] {:dir root :in "" :out :string :err :out
                                                                                     :extra-env (child-env {"LUME_SESSION" session})})))
                  run {:exit exit :ms ms :output out}
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
  (let [root (:repo (state session))
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
                                                {:out (str (fs/path dir (str id ".out")))})
                                   tail (assoc :tail tail)))))]
    (fs/move tmp (:out ran))
    ran))
