(ns lume.kernel
  "What every lume command shares: who is acting, failing with a user error,
  parsing flags, human-only confirmation, and running shell commands in the
  repo with their output kept under the session."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [lume.status :as status]
            [lume.store :as store]))

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

;; Events: every type lume appends is declared here with the keys it must
;; carry; :human types are policy, logged only when the human acts. Reading
;; stays lenient (project skips what it doesn't know), so old logs still fold.

(def events
  {:session/started {:req #{:goal :repo}}
   :session/gated {:req #{:gate}}
   :session/allowed {:req #{:command} :human true}
   :session/disallowed {:req #{:command} :human true}
   :session/budgeted {:req #{}}
   :session/cadence {:req #{}}
   :item/added {:req #{:item/id :intent}}
   :item/gated {:req #{:item/id :gate}}
   :item/measuring {:req #{:item/id :measure}}
   :item/closed {:req #{:item/id :disposition}}
   :gate/ran {:req #{:gate/id :item/id :gate :exit :out}}
   :measure/ran {:req #{:measure/id :item/id :measure :exit :out}}
   :note/added {:req #{:note/id :text}}
   :note/unpinned {:req #{:note/id}}
   :ask/opened {:req #{:ask/id :question}}
   :ask/answered {:req #{:ask/id :answer} :human true}
   :spawn/started {:req #{:spawn/id :item/id :agent :out}}
   :spawn/progress {:req #{:spawn/id}}
   :spawn/exited {:req #{:spawn/id}}
   :spawn/requested {:req #{:item/id}}
   :spawn/permission {:req #{:spawn/id :disposition}}
   :drive/started {:req #{:turns :agent}}
   :drive/turn {:req #{:drive/id :n}}
   :drive/exited {:req #{:drive/id}}
   :drive/stopped {:req #{:why}}
   :drive/handed-back {:req #{:drive/id :why}}
   :drive/reloaded {:req #{:n}}
   :notify/sent {:req #{:on :text}}
   :notify/failed {:req #{:on :text :error}}
   :repl/opened {:req #{:port}}})

(defn- checked
  "e with :by filled from the actor, or a refusal if events doesn't allow it."
  [{:keys [type] :as e}]
  (let [{:keys [req human] :as decl} (get events type)
        missing (sort (filter #(nil? (get e %)) req))]
    (cond (nil? decl) (fail! "refused: undeclared event type" (pr-str type))
          (seq missing) (fail! "refused:" (pr-str type) "needs" (str/join " " (map pr-str missing)))
          (and human (not= "human" (actor))) (fail! "refused:" (pr-str type) "is the human's, not" (str (actor) "'s"))
          :else (merge {:by (actor)} e))))

(defn append!
  "store/append!, checked: the event (f events) builds must be a declared type
  with its required keys, and a :human type only the human appends. :by is the
  actor unless the event names one."
  [session f]
  (store/append! session #(some-> (f %) checked)))

(defn open-item [events id]
  (let [item (get-in (status/project events) [:items id])]
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

(defn clip-to [n s]
  (let [s (str/replace (str s) #"\s+" " ")]
    (if (> (count s) n) (str (subs s 0 (dec n)) "…") s)))

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
        ran (append! session (fn [events]
                               (let [[id e] (event-for events runs)]
                                 (cond-> (merge {:exit exit :ms ms} e
                                                {:out (str (fs/path dir (str id ".out"))) :by (actor)})
                                   tail (assoc :tail tail)))))]
    (fs/move tmp (:out ran))
    ran))
