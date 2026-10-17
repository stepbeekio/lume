(ns lume.cli
  "The lume command line. Output is agent-first: short, structured, bounded."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [lume.drive :as drive]
            [lume.kernel :as kernel :refer [*default-agent* *read-line* actor child-env clip-to columns driver? fail! flag
                                            human-only! next-id open-item run-in-repo! session-by-id take-flags]]
            [lume.measure :as measure]
            [lume.note :as note]
            [lume.ntfy :as ntfy :refer [ask-item]]
            [lume.policy :as policy]
            [lume.status :as status]
            [lume.store :as store])
  (:import (java.time Instant)))

;; lume.agents and lume.spec load on first use, via requiring-resolve: most
;; lume calls (note, item, ask, a driver's spawn request) are short processes
;; that need neither, and loading code is most of what they cost.

;; Commands. Each takes the session dir (or repo root, for init) and its args,
;; and returns the text to print.

;; A session may have a default gate: items added without --gate get it, so
;; nothing is ungated by omission. --allow is lume allow, so it's human-only too.
(def ^:private init-usage "usage: lume init \"<goal>\" [--gate \"<cmd>\"] [--allow \"<cmd>\"]...")

(defn init [root args]
  (let [usage init-usage
        [[goal & extra] {gates "--gate" allows "--allow"}] (take-flags args #{"--gate" "--allow"} usage)
        gate (first gates)
        allows (distinct allows)]
    (when (or (str/blank? goal) (seq extra) (next gates)) (fail! usage))
    (when (seq allows)
      (human-only! "only the human changes permissions; ask with lume ask \"<question>\""
                   (str "Workers will be allowed to run: " (str/join " · " allows) ".") "yes"))
    (let [session (store/new-session! root)]
      (kernel/append! session (constantly {:type :session/started :goal goal :repo root :by (actor)}))
      (when gate
        (kernel/append! session (constantly {:type :session/gated :gate gate :by (actor)})))
      (doseq [command allows]
        (kernel/append! session (constantly {:type :session/allowed :command command :by (actor)})))
      (str "session " (store/session-id session) " started"
           (when gate (str " · default gate " gate))
           (when (seq allows) (str " · workers may run " (str/join " · " allows)))))))

;; --parent files work that emerged while on an open item under it. The tree
;; only makes that work visible: nothing is inherited and nothing orders spawns.
(def ^:private item-add-usage "usage: lume item add \"<intent>\" [--gate \"<cmd>\"] [--parent <id>]")

(defn item-add [session args]
  (let [usage item-add-usage
        [[intent & extra] {[gate & gates] "--gate" [parent & parents] "--parent"}]
        (take-flags args #{"--gate" "--parent"} usage)]
    (when (or (str/blank? intent) (seq extra) (seq gates) (seq parents)) (fail! usage))
    ;; Without --gate the item gets the session's default gate, written on the event.
    (let [e (kernel/append! session (fn [events]
                                      (when parent (open-item events parent))
                                      (let [gate (or gate (:default-gate (status/project events)))]
                                        (cond-> {:type :item/added :item/id (next-id "i" :item/added events)
                                                 :intent intent :by (actor)}
                                          gate (assoc :gate gate)
                                          parent (assoc :parent parent)))))]
      (:item/id e))))

(def ^:private item-gate-usage "usage: lume item gate <id> \"<cmd>\"")

(defn item-gate [session [id gate]]
  (when (str/blank? gate) (fail! item-gate-usage))
  (kernel/append! session (fn [events]
                            (open-item events id)
                            {:type :item/gated :item/id id :gate gate :by (actor)}))
  (str id " gate " gate))

;; Gates and measurements: shell commands run in the repo, their output kept
;; under the session. A gate's exit code decides done; a measurement's output
;; is numbers, one ledger row per run.

(defn- run-gate
  "Run the item's gate and log a :gate/ran event, which is returned."
  [session {:keys [id gate]}]
  (run-in-repo! session "gates" gate
               (fn [events _]
                 (let [gid (next-id "g" :gate/ran events)]
                   [gid {:type :gate/ran :gate/id gid :item/id id :gate gate}]))))

(defn- gate-summary [{:keys [exit tail out] gid :gate/id}]
  (str gid " " (if (zero? exit) "pass" (str "fail exit " exit))
       (when (and tail (not (zero? exit))) (str " · " (str/replace tail #"\s+" " ") "\n" out))))

;; dropped --revert restores the working copy to the item's before row (lume.measure).
(def ^:private item-close-usage "usage: lume item close <id> done|dropped [--revert] \"<why>\"")

(defn item-close [session [id disposition & more]]
  (let [usage item-close-usage
        revert (boolean (some #{"--revert"} more))
        why (first (remove #{"--revert"} more))]
    (when-not (#{"done" "dropped"} disposition) (fail! usage))
    (when (and revert (not= "dropped" disposition)) (fail! "--revert goes with dropped;" usage))
    (let [events (store/read-events session)
          item (open-item events id)
          ran (when (and (= "done" disposition) (:gate item)) (run-gate session item))
          _ (when (and ran (not (zero? (:exit ran))))
              (fail! id "not done: gate" (gate-summary ran)))
          restored (when revert
                     (measure/revert! (:repo (status/project events)) id (get-in (status/project events) [:items id])))]
      (kernel/append! session (fn [events]
                                (open-item events id)
                                (cond-> {:type :item/closed :item/id id :disposition (keyword disposition)
                                         :why why :by (actor)}
                                  ran (assoc :gate/id (:gate/id ran))
                                  restored (merge restored))))
      (str id " " disposition (when ran (str " · " (gate-summary ran)))
           (when restored
             (str " · restored " (if (:jj-commit restored) (str "jj " (:jj-commit restored)) (str "git " (:commit restored)))
                  " from " (:measure/id restored)))))))

;; Only the human grants permissions: workers and agent drivers would grant themselves
;; whatever they need, so they ask with lume ask instead.
(def ^:private allow-usage "usage: lume allow [--revoke] \"<cmd>\"")

(defn allow [session [a b]]
  (let [revoke (= "--revoke" a)
        command (if revoke b a)]
    (when (str/blank? command) (fail! allow-usage))
    (human-only! "only the human changes permissions; ask with lume ask \"<question>\""
                 (str (if revoke "Workers may no longer run: " "Workers may run: ") command ".") "yes")
    (kernel/append! session (constantly {:type (if revoke :session/disallowed :session/allowed)
                                         :command command :by (actor)}))
    (str (if revoke "workers may no longer run " "workers may run ") command)))

;; Questions for the human. Anyone may ask; only the human answers, and the
;; answer reaches status and workers alongside the notes, and LUME_NTFY.

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

(def ^:private ask-usage "usage: lume ask [\"<question>\"]")

(defn ask [session [question & extra]]
  (cond (and (nil? question) (nil? extra)) (open-asks session (columns))
        (or (str/blank? question) (seq extra)) (fail! ask-usage)
        :else (let [opened (kernel/append! session (fn [events]
                                                     {:type :ask/opened :ask/id (next-id "a" :ask/opened events)
                                                      :question question :by (actor)}))]
                (:ask/id opened))))

;; With no text, at a terminal, the human reads the question in full and types
;; the answer at the prompt; the typed-id confirmation still follows.
(def ^:private answer-usage "usage: lume answer <id> [\"<text>\"]")

(defn answer [session [id text]]
  (when (str/blank? id) (fail! answer-usage))
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
    (when (str/blank? text) (fail! answer-usage))
    (human-only! "only the human answers asks; record what you think with lume note"
                 (str "Answer " id ": " text) id)
    (kernel/append! session (fn [events]
                              (open-ask events)
                              {:type :ask/answered :ask/id id :answer text :by (actor)}))
    (str id " answered")))

;; Ask show: what status clips. The question in full, who asked and when, the
;; item it concerns (a plateau's, or the asking worker's), and the answer.
(def ^:private ask-show-usage "usage: lume ask show <id>")

(defn ask-show [session [id]]
  (when (str/blank? id) (fail! ask-show-usage))
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
                 [(str "answer   " (:answer answered))
                  (str "answered " (at answered) (when-let [via (:via answered)] (str " via " (name via))))]
                 [(str "answer   none yet; only the human answers: lume answer " id)])))))

(defn- worker-prompt [events item]
  (let [{:keys [goal] :as state} (status/project events)]
    (str/join "\n"
              (concat
               ["You are a worker in a lume session."
                (str "Session goal: " goal)
                (str "Your work item (" (:id item) "): " (:intent item))]
               (status/prompt-lines state item)
               ["Work in the current directory. Record findings with `lume note \"<text>\"`"
                (str "and follow-up work with `lume item add \"<intent>\"` (add `--parent " (:id item)
                     "` if it emerged from this item).")
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
            (do (kernel/append! session (constantly (merge {:type :spawn/progress :spawn/id sid}
                                                           (cond-> prog (:last prog) (update :last clip-last)))))
                (recur prog prog now))
            (recur prog logged at)))
        prog))))

(defn- sh-out [root & cmd]
  (try (let [{:keys [exit out]} (apply p/shell {:dir root :in "" :out :string :err :string :continue true} cmd)]
         (when (zero? exit) (not-empty (str/trim out))))
       (catch Exception _ nil)))

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
  (let [agent (keyword (or (flag opts "--agent") *default-agent*))
        events (store/read-events session)
        item (open-item events id)
        started (kernel/append! session (fn [events]
                                          (let [sid (next-id "s" :spawn/started events)]
                                            {:type :spawn/started :spawn/id sid :item/id id :agent agent
                                             :out (str (fs/path session "spawns" (str sid ".out")))
                                             :by (actor)})))
        {sid :spawn/id out :out} started
        done (promise)
        hook (Thread. ^Runnable (bound-fn []
                                  (when-not (realized? done)
                                    (kernel/append! session (constantly {:type :spawn/exited :spawn/id sid
                                                                         :interrupted true})))))]
    (fs/create-dirs (fs/parent out))
    (.addShutdownHook (Runtime/getRuntime) hook)
    (let [;; A before row is taken once the spawn has started, before the
          ;; worker runs, so every row after it compares with it.
          before-row (when (and (:measure item) (:before item))
                       (str (measure/measure-row session (measure/run-measure session item :before-spawn sid)) "\n"))
          t0 (System/currentTimeMillis)
          before (worktree-fingerprint root)
          ;; A worker may run the session's allowed commands, and its own gate.
          allowed (distinct (cond-> (:allow (status/project events)) (:gate item) (conj (:gate item))))
          proc (p/process ((requiring-resolve 'lume.agents/command) agent (worker-prompt events item) {:allow allowed})
                          {:dir root :in ""
                           :err :write :err-file (fs/file (str out ".err"))
                           :extra-env (child-env {"LUME_SESSION" session "LUME_ACTOR" sid "LUME_SPAWNED_AS" sid})})
          {:keys [steps]} (follow! session sid agent proc out)
          {:keys [exit]} @proc
          summary ((requiring-resolve 'lume.agents/summarize) agent (slurp out))
          exited (kernel/append! session (constantly (cond-> (merge {:type :spawn/exited :spawn/id sid :exit exit
                                                                     :ms (- (System/currentTimeMillis) t0)}
                                                                    summary)
                                                       steps (assoc :steps steps))))]
      (deliver done true)
      (.removeShutdownHook (Runtime/getRuntime) hook)
      (kernel/react-for! session sid)
      (str before-row sid " " id " exit " exit " in " (quot (:ms exited) 1000) "s"
           (when-let [r (:result exited)] (str " · " (str/replace (str r) #"\s+" " ")))
           "\n" out
           ;; Measure what the worker left, then: the worker may have closed the
           ;; item itself; if it asked or changed nothing, a passing gate says
           ;; nothing about its work, so the item stays open. A measured item
           ;; stays open too: its gate runs, but keeping or rolling back the
           ;; change is the human's or driver's call. Otherwise its gate decides.
           (let [item (get-in (status/project (store/read-events session)) [:items id])]
             (when (:measure item)
               (str "\n" (measure/measure-row session (measure/run-measure session item)))))
           (let [item (get-in (status/project (store/read-events session)) [:items id])
                 why (when (and (= :open (:state item)) (or (:gate item) (:measure item)))
                       (idle-why session sid before (worktree-fingerprint root)))]
             (cond
               (not= :open (:state item)) nil
               why (str "\n" id " left open: " why " · " (note/note session [(str id " left open, not gated: " why)]))
               (:measure item) (str "\n" id " left open to decide"
                                    (when (:gate item) (str " · " (gate-summary (run-gate session item))))
                                    " · lume item close " id " done|dropped \"<why>\"")
               (:gate item) (str "\n" (try (item-close session [id "done" (str "gate passed after " sid)])
                                           (catch clojure.lang.ExceptionInfo e (ex-message e))))))))))

;; A driver's spawn is only a request: its turn ends first, then drive runs it.
;; The cadence's batch is how many requests a turn may make (its guard).
(def ^:private spawn-usage "usage: lume spawn <id> [--agent claude|claude-acp|devin]")

(defn spawn [session root [id & opts]]
  (when (str/blank? id) (fail! spawn-usage))
  (if (driver?)
    (do
      (kernel/append! session (fn [events]
                                (open-item events id)
                                (cond-> {:type :spawn/requested :item/id id :by (actor)}
                                  (flag opts "--agent") (assoc :agent (keyword (flag opts "--agent"))))))
      (let [state (status/project (store/read-events session))
            n (get-in state [:requested (actor)])
            batch (:batch (policy/cadence state))]
        (if (< n batch)
          (str id " spawn requested (" n " of " batch " this turn); requests run in order when your turn ends")
          (str id " spawn requested; it runs when your turn ends, so finish the turn now"))))
    (run-spawn session root id opts)))

;; Item show: what status clips. The intent and gate in full, then the item's
;; events: its own, its spawns' (bar progress), and notes and asks naming it.

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
                      :measure/ran (str (:measure/id e) " " (measure/measure-text e)
                                        (when (:env e) (str " · " (measure/env-text (:env e)))))
                      :item/measuring (measure/spec-text e)
                      (str/join " " (keep #(when-some [v (e %)] (case % :exit (str "exit " v) (if (keyword? v) (name v) v)))
                                          [:spawn/id :gate/id :note/id :ask/id :disposition :exit
                                           :command :gate :measure :text :question :answer :why :result :tail]))))
       (when (:by e) (str " [" (:by e) "]"))))

(def ^:private item-show-usage "usage: lume item show <id>")

(defn item-show [session [id]]
  (when (str/blank? id) (fail! item-show-usage))
  (let [events (store/read-events session)
        item (or (get-in (status/project events) [:items id]) (fail! "no such item:" id))
        related (filter (item-event? id (set (:spawns item))) events)
        shown (take-last 20 related)]
    (str/join "\n"
              (concat
               [(str id " " (name (:state item)) (when (:why item) (str " — " (clip-to 200 (:why item)))))
                (str "intent  " (clip-to 2000 (:intent item)))
                (str "gate    " (or (:gate item) "none"))]
               (when (:measure item) [(str "measure " (measure/spec-text item))])
               (when (> (count related) (count shown))
                 [(str "+" (- (count related) (count shown)) " earlier events — lume log --item " id " -n N")])
               (map item-event-line shown)))))

;; Install: set up the agent side of this machine. Safe to re-run; writes only
;; what differs and nothing inside any repo.

(defn install [[a b]]
  (let [agent (keyword (if (= "--agent" a) b *default-agent*))
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
  (let [items (rest (tree-seq map? :items {:items items}))]
    (str (count items) (if (= 1 (count items)) " item" " items")
         (when-let [m (seq (filter :measure items))] (str " · " (count m) " measured")))))

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

(def ^:private run-usage "usage: lume run <spec.edn> [\"<goal>\"] [--turns N] [--agent claude|claude-acp|devin]")

(defn- spec-policy
  "The human-only parts of a spec, as text for the confirmation, or nil."
  [{:keys [allow cadence]}]
  (let [raises (when cadence
                 (policy/cadence-raises policy/default-cadence (merge policy/default-cadence cadence)))]
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
          (kernel/append! session (constantly (assoc e :by (actor)))))
        (say (str "session " (store/session-id session) " started from " path " · " (items-text (:items sp))))
        (when within
          (say (str "not made current: run inside " (store/session-id within)
                    "; lume status " (store/session-id session) " reads it")))
        (binding [store/*session* (when within session)]
          (doseq [item (->> (vals (:items (status/project (store/read-events session)))) (filter :measure) (sort-by :order))]
            (say (str "baseline " (measure/measure-row session (measure/run-measure session item)))))
          (if (zero? n)
            "--turns 0: not driving; lume drive, or drive by hand from lume status"
            (drive/drive session root (cond-> ["--turns" (str n)] agent (conj "--agent" agent)))))))))

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
                                ;; nREPL evaluates outside the run's binding.
                                ([cmd opts] (binding [kernel/*features* (or kernel/*features* status/features)]
                                              (measure/try-measure session cmd opts)))))
      (refresh!))))

(defn start-repl
  "Start an nREPL whose user namespace (or opts :ns) is set up on session.
  Returns {:server :port :text}; stop it with babashka.nrepl.server/stop-server!."
  [session {:keys [port ns] :or {port 0 ns 'user}}]
  (let [server ((requiring-resolve 'babashka.nrepl.server/start-server!) {:port port :quiet true})
        port (.getLocalPort ^java.net.ServerSocket (:socket server))
        ;; The one write: the log shows prototyping happened, e.g. before a spec.
        _ (kernel/append! session (constantly {:type :repl/opened :port port :by (actor)}))
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

;; The built-in commands (see kernel/run), by feature name: joined to the
;; built-in features here until those move out of status.clj (i87+); a split
;; feature (lume.note) carries its own. Each :help is its entry in lume help,
;; which lists them in feature order.

(def ^:private commands
  {"install"
   [{:path ["install"] :needs :none :usage "usage: lume install [--agent claude|claude-acp|devin]"
     :run (fn [_ args] (install args))
     :help "lume install [--agent A]                    set up agents on this machine (a lume skill); safe to re-run.
                                            Agents: claude, claude-acp, devin; LUME_AGENT sets the default (claude)"}]
   "session"
   [{:path ["init"] :needs :root :usage init-usage
     :run (fn [{:keys [root]} args] (init root args))
     :help "lume init \"<goal>\" [--gate \"<cmd>\"] [--allow \"<cmd>\"]...
                                            start a session; items added without --gate get its --gate"}
    {:path ["status"] :needs :root :usage status-usage
     :run (fn [{:keys [root]} args] (show-status root args))
     :help "lume status [<session>] [--color]           one screen: goal, items, notes, recent events (colour on a tty);
                                            a past session's by id (lume ls)"}]
   "item"
   [{:path ["item" "add"] :needs :session :usage item-add-usage
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
     :help "lume item show <id>                         one item in full: intent, gate, disposition and its events"}]
   ;; ask show needs an id: with anything else, show is the text.
   "ask"
   [{:path ["ask"] :needs :session :usage ask-usage
     :run (fn [{:keys [session]} args] (ask session args))
     :help "lume ask \"<question>\"                       put a question to the human; open asks top status
lume ask                                    open asks in full, wrapped to the terminal, each with its answer command"}
    {:path ["ask" "show"] :needs :session :usage ask-show-usage
     :run (fn [{:keys [session]} args] (if (= 1 (count args)) (ask-show session args) (ask session (cons "show" args))))
     :help "lume ask show <id>                          one ask in full: question, who asked and when, its item, its answer"}
    {:path ["answer"] :needs :session :usage answer-usage
     :run (fn [{:keys [session]} args] (answer session args))
     :help "lume answer <id> [\"<text>\"]                 answer an ask (human at a tty: retype the id); no text: prompt for it
                                            after the full question. Answers join the notes"}]
   "spawn"
   [{:path ["spawn"] :needs #{:session :root} :usage spawn-usage
     :run (fn [{:keys [session root]} args] (spawn session root args))
     :help "lume spawn <id> [--agent A]                 run a worker on an item, in the foreground (a driver's is a request)"}
    {:path ["allow"] :needs :session :usage allow-usage
     :run (fn [{:keys [session]} args] (allow session args))
     :help "lume allow [--revoke] \"<cmd>\"               human at a tty, types yes: let workers run commands starting with <cmd> (their gate is always allowed)"}]
   "spec"
   [{:path ["check"] :needs :none :usage check-usage
     :run (fn [_ args] (check args))
     :help "lume check <spec.edn>                       validate a harness spec: every problem, one per line, exit 1 on any"}
    {:path ["spec"] :needs :session :usage spec-usage
     :run (fn [{:keys [session]} args] (show-spec session args))
     :help "lume spec [--all]                           this session's harness as spec EDN (open items; --all, closed too)"}
    {:path ["run"] :needs :root :usage run-usage
     :run (fn [{:keys [root]} args] (run-spec root args))
     :help "lume run <spec.edn> [\"<goal>\"] [--turns N] [--agent A]
                                            check a spec, start a session from it, baseline its measurements,
                                            then drive (--turns 0: don't). A spec's allow, or a cadence above
                                            every-spawn, is the human's. Run by a worker or driver, the new
                                            session starts alongside; the current one stays"}]
   "inspect"
   [{:path ["ls"] :needs :root :usage ls-usage
     :run (fn [{:keys [root]} args] (ls root args))
     :help "lume ls [--all]                             this repo's sessions, newest first (* current): goal, open/closed
                                            items, last event, outcome; 10 unless --all"}
    {:path ["log"] :needs :root :usage log-usage
     :run (fn [{:keys [root]} args] (show-log root args))
     :help "lume log [-n N] [--session <id>] [--type <t>] [--item <id>] [--by <actor>] [--grep <re>]
                                            raw events as EDN (default last 20), of a past session by id;
                                            filters combine: type prefix (note, spawn/), an item's events,
                                            actor, regex over the event; a ;; line counts what -n left out"}
    {:path ["replay"] :needs :root :usage replay-usage
     :run (fn [{:keys [root]} args] (replay root args))
     :help "lume replay [<session>] [--at N] [--step]   status as it stood after event N, from the log alone; no flags:
                                            the decisions (drive turns, spawns), each with its N (-n, 20);
                                            --step: the screen after each decision in turn, at a terminal"}
    {:path ["repl"] :needs :root :usage repl-usage
     :run (fn [{:keys [root]} args] (repl root args))
     :help "lume repl [<session>] [--port N]            an nREPL (free port) with the session's events, state, render
                                            and lume.* in user; prints the port; writes nothing; runs until killed"}]
   "clean"
   [{:path ["clean"] :needs :root :usage clean-usage
     :run (fn [{:keys [root]} args] (clean root args))
     :help "lume clean [--session <id>] [--yes]         remove every trace of lume for this repo (or one session):
                                            its state, refs/lume/*, worktrees; lists them, refused without --yes"}]})

(def features
  "The built-in features with their commands, in order: lume help lists the
  commands in this order. -main binds kernel/*features* to these."
  (let [named (into {} (map (juxt :name identity)) (conj status/features drive/feature))
        order ["install" "session" "item" "ask" "note" "measure" "spawn" "budget" "cadence" "drive"
               "ntfy" "spec" "inspect" "clean"]]
    (assert (every? (set order) (keys named)) "every built-in feature has a place in the order")
    (mapv #(-> (get named % {:name %})
               (update :commands (fn [own] (or own (get commands % [])))))
          order)))

(defn -main [& args]
  (try
    (println (binding [kernel/*features* features] (kernel/run args)))
    (catch clojure.lang.ExceptionInfo e
      (if (:lume/user-error (ex-data e))
        (do (binding [*out* *err*] (println (ex-message e)))
            (System/exit 1))
        (throw e)))))
