(ns lume.agent
  "Agents: lume spawn runs an adapter (lume.agents) on an item, follows its
  progress and lets its gate decide; lume allow is what workers may run."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [lume.item :as item]
            [lume.kernel :as kernel :refer [*default-agent* actor ago child-env clip driver? fail! human-only!
                                            next-id open-item paint sh sh-out sha256-hex take-flags timed]]
            [lume.measure :as measure]
            [lume.note :as note]
            [lume.policy :as policy]))

;; lume.agents loads on first use, via requiring-resolve: most lume calls
;; (note, item, ask, a driver's spawn request) are short processes that don't
;; need it, and loading code is most of what they cost.

;; Only the human grants permissions: workers and agent drivers would grant themselves
;; whatever they need, so they ask with lume ask instead.
(def ^:private allow-usage "usage: lume allow \"<cmd>\"")
(def ^:private revoke-usage "usage: lume allow --revoke \"<cmd>\"")

(def ^:private permissions-why "only the human changes permissions; ask with lume ask \"<question>\"")

(defn allow [session args]
  (let [[command] (kernel/positionals args 1 allow-usage)]
    (human-only! permissions-why (str "Workers may run: " command ".") "yes")
    (kernel/append! session (constantly {:type :session/allowed :command command}))
    (str "workers may run " command)))

(defn revoke [session args]
  (let [[command] (kernel/positionals args 1 revoke-usage)]
    (human-only! permissions-why (str "Workers may no longer run: " command ".") "yes")
    (kernel/append! session (constantly {:type :session/disallowed :command command}))
    (str "workers may no longer run " command)))

(defn prompt-lines
  "What the bound features add to a worker's prompt for item: each :prompt
  (fn [state item] lines), in feature order."
  [state item]
  (mapcat #((:prompt %) state item) (filter :prompt (kernel/features))))

(defn- worker-prompt [events item]
  (let [{:keys [goal] :as state} (kernel/project events)]
    (str/join "\n"
              (concat
               ["You are a worker in a lume session."
                (str "Session goal: " goal)
                (str "Your work item (" (:id item) "): " (:intent item))]
               (prompt-lines state item)
               ["Work in the current directory. Record findings with `lume note \"<text>\"`"
                (str "and follow-up work with `lume item add \"<intent>\"` (add `--parent " (:id item)
                     "` if it emerged from this item).")
                "When you finish, end with a short summary"
                "of what you did and anything left undone."]))))

;; Spawns. A worker's stdout is copied to its .out file line by line, and the
;; adapter folds each line into progress, logged at most every *progress-ms*.

(def ^:dynamic *progress-ms* 15000)

;; Not kernel/clip: :last is logged as the adapter saw it, whitespace and all,
;; and cut without an ellipsis; status clips it when it shows it.
(defn- clip-last
  "s cut to at most 200 characters, otherwise as it was."
  [s]
  (subs s 0 (min 200 (count s))))

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

(defn- worktree-fingerprint
  "A hash of the repo's git HEAD, its changed paths and their contents, so a
  spawn that changed nothing can be told apart from one that did. Read-only
  (no index refresh), and nil when the repo isn't a git repo (jj colocates
  one) or git can't answer."
  [root]
  (when (fs/exists? (fs/path root ".git"))
    (try
      (let [{:keys [exit out]} (sh {:dir root} "git" "--no-optional-locks" "status" "--porcelain=v1" "-z" "-uall")]
        (when (zero? exit)
          (apply sha256-hex (.getBytes (str (sh-out root "git" "rev-parse" "HEAD") "\n" out))
                 (for [entry (str/split out #"\x00")
                       :let [f (fs/path root (subs entry (min 3 (count entry))))]
                       :when (fs/regular-file? f)]
                   (fs/read-all-bytes f)))))
      (catch Exception _ nil))))

(defn- idle-why
  "Why a spawn's item shouldn't close on its gate, or nil: it opened an ask,
  or the repo is exactly as it was before it ran."
  [session sid before after]
  (let [asks (keep #(when (and (= :ask/opened (:type %)) (= sid (:by %))) (:ask/id %))
                   (kernel/read-events session))]
    (cond (seq asks) (str sid " asked " (str/join ", " asks))
          (and before (= before after)) (str sid " changed nothing in the repo"))))

(defn- run-worker
  "Run agent's worker on item until it exits, its stdout copied to out, and
  return {:exit :steps :summary}."
  [session root agent sid events item out]
  (let [;; A worker may run the session's allowed commands, and its own gate.
        allowed (distinct (cond-> (:allow (kernel/project events)) (:gate item) (conj (:gate item))))
        proc (p/process ((requiring-resolve 'lume.agents/command) agent (worker-prompt events item) {:allow allowed})
                        {:dir root :in ""
                         :err :write :err-file (fs/file (str out ".err"))
                         :extra-env (child-env {"LUME_SESSION" session "LUME_ACTOR" sid "LUME_SPAWNED_AS" sid})})
        {:keys [steps]} (follow! session sid agent proc out)
        {:keys [exit]} @proc]
    {:exit exit :steps steps
     :summary ((requiring-resolve 'lume.agents/summarize) agent (slurp out))}))

(defn- start-spawn!
  "Log the spawn of agent on open item id, hook its exit in case lume is
  interrupted, and take the item's before row if it has one. Returns
  {:sid :out :events :item :finished! :before-row}."
  [session id agent]
  (let [events (kernel/read-events session)
        item (open-item events id)
        {sid :spawn/id out :out} (kernel/append! session (fn [events]
                                                           (let [sid (next-id "s" :spawn/started events)]
                                                             {:type :spawn/started :spawn/id sid :item/id id :agent agent
                                                              :out (str (fs/path session "spawns" (str sid ".out")))})))
        finished! (kernel/exit-hook! session {:type :spawn/exited :spawn/id sid :interrupted true})]
    (fs/create-dirs (fs/parent out))
    ;; A before row is taken once the spawn has started, before the worker
    ;; runs, so every row after it compares with it.
    {:sid sid :out out :events events :item item :finished! finished!
     :before-row (when (and (:measure item) (:before item))
                   (str (measure/measure-row session (measure/run-measure session item :before-spawn sid)) "\n"))}))

(defn- work!
  "Run the started spawn's worker until it exits, log :spawn/exited, run the
  reactions to what the worker logged, and return the exited event."
  [session root agent {:keys [sid out events item finished!]}]
  (let [[{:keys [exit steps summary]} ms] (timed #(run-worker session root agent sid events item out))
        exited (kernel/append! session (constantly (cond-> (merge {:type :spawn/exited :spawn/id sid :exit exit :ms ms}
                                                                  summary)
                                                     steps (assoc :steps steps))))]
    (finished!)
    (kernel/react-for! session sid)
    exited))

(defn- exited-line [id out {sid :spawn/id :keys [exit ms result]}]
  (str sid " " id " exit " exit " in " (quot ms 1000) "s"
       (when result (str " · " (kernel/one-line result)))
       "\n" out))

;; After the worker exits, its item is measured, then: the worker may have
;; closed it itself; if it asked or changed nothing, a passing gate says
;; nothing about its work, so the item stays open. A measured item stays open
;; too: its gate runs, but keeping or rolling back the change is the human's
;; or driver's call. Otherwise its gate decides.

(defn- gated?
  "Whether the item is open with a gate or a measure to decide it."
  [item]
  (and (= :open (:state item)) (or (:gate item) (:measure item))))

(defn- after-spawn
  "What follows a spawn on item, given its idle-why: nil (closed, or nothing
  decides it), [:left-open why], [:decide] or [:gate]."
  [item why]
  (cond
    (not= :open (:state item)) nil
    why [:left-open why]
    (:measure item) [:decide]
    (:gate item) [:gate]))

(defn- follow-up!
  "Act on after-spawn's step for item and return its line, or nil."
  [session sid {:keys [id] :as item} [step why]]
  (case step
    nil nil
    :left-open (str "\n" id " left open: " why " · " (note/added-line (note/add! session (str id " left open, not gated: " why))))
    :decide (str "\n" id " left open to decide"
                 (when (:gate item) (str " · " (item/gate-summary (item/run-gate session item))))
                 " · lume item close " id " done|dropped \"<why>\"")
    :gate (str "\n" (try (item/close-line (item/close! session id :done (str "gate passed after " sid)))
                         (catch clojure.lang.ExceptionInfo e (ex-message e))))))

(defn- run-spawn [session root id agent]
  (let [agent (keyword (or agent *default-agent*))
        {:keys [sid out before-row] :as started} (start-spawn! session id agent)
        before (worktree-fingerprint root)
        exited (work! session root agent started)
        ;; A :measure/ran row changes no item's state, gate or measure, so
        ;; one projection serves the measure and the decision after it.
        item (get-in (kernel/state session) [:items id])
        after-row (when (:measure item)
                    (str "\n" (measure/measure-row session (measure/run-measure session item))))
        why (when (gated? item) (idle-why session sid before (worktree-fingerprint root)))]
    (str before-row (exited-line id out exited) after-row
         (follow-up! session sid item (after-spawn item why)))))

;; A driver's spawn is only a request: its turn ends first, then drive runs it.
;; The cadence's batch is how many requests a turn may make (its guard).
(def ^:private spawn-usage "usage: lume spawn <id> [--agent claude|claude-acp|devin]")

(defn- spawn-args
  "spawn's args as {:id :agent}, or a usage error."
  [args]
  (let [[[id & extra] {[agent & agents] "--agent"}] (take-flags args #{"--agent"} spawn-usage)]
    (when (or (str/blank? id) (seq extra) (seq agents)) (fail! spawn-usage))
    {:id id :agent agent}))

(defn spawn [session root args]
  (let [{:keys [id agent]} (spawn-args args)]
    (if (driver?)
      (do
        (kernel/append! session (fn [events]
                                  (open-item events id)
                                  (cond-> {:type :spawn/requested :item/id id}
                                    agent (assoc :agent (keyword agent)))))
        (let [state (kernel/state session)
              n (get-in state [:requested (actor)])
              batch (:batch (policy/cadence state))]
          (if (< n batch)
            (str id " spawn requested (" n " of " batch " this turn); requests run in order when your turn ends")
            (str id " spawn requested; it runs when your turn ends, so finish the turn now"))))
      (run-spawn session root id agent))))

;; Status: what workers may run (with the session's default gate and the
;; cadence), and each open item's latest spawn.

(defn- allow-lines [{:keys [allow default-gate] :as state} _ _]
  (let [cadence-line (policy/cadence-line state)]
    (when (or (seq allow) default-gate cadence-line)
      [(str/join " · " (cond->> allow
                         default-gate (cons (str "default gate " default-gate))
                         cadence-line (cons cadence-line)))])))

(defn- spawn-line [now {:keys [exit interrupted result t progress] :as sp}]
  (let [running (not (or interrupted (contains? sp :exit)))]
    (str (:spawn/id sp) " "
         (cond interrupted "interrupted"
               running (paint :cyan "running")
               :else (str "exit " exit))
         " · " (ago now t)
         (when-let [steps (or (:steps sp) (:steps progress))] (str " · " steps (if (= 1 steps) " step" " steps")))
         (cond result (str " · " (clip 60 result))
               (and running (:last progress)) (str " · " (clip 50 (:last progress)) " (" (ago now (:t progress)) ")")))))

(def feature
  {:name "spawn"
   :events {:spawn/started {:req #{:spawn/id :item/id :agent :out}}
            :spawn/progress {:req #{:spawn/id}}
            :spawn/exited {:req #{:spawn/id}}
            :spawn/permission {:req #{:spawn/id :disposition}}
            :session/allowed {:req #{:command} :human true}
            :session/disallowed {:req #{:command} :human true}}
   :init {:spawns {} :allow []}
   :fold {:spawn/started (fn [s e]
                           (-> s
                               (assoc-in [:spawns (:spawn/id e)] e)
                               (update-in [:items (:item/id e) :spawns] (fnil conj []) (:spawn/id e))))
          :spawn/progress (fn [s e] (assoc-in s [:spawns (:spawn/id e) :progress] e))
          :spawn/exited (fn [s e] (update-in s [:spawns (:spawn/id e)] merge e))
          :session/allowed (fn [s e] (update s :allow #(vec (distinct (conj % (:command e))))))
          :session/disallowed (fn [s e] (update s :allow #(vec (remove #{(:command e)} %))))}
   :status [{:label "allow" :order 30 :cap 1 :lines allow-lines}]
   :item-lines (fn [s item now] (when-let [sp (get (:spawns s) (peek (:spawns item)))] [(spawn-line now sp)]))
   :commands [{:path ["spawn"] :needs #{:session :root} :usage spawn-usage
               :run (fn [{:keys [session root]} args] (spawn session root args))
               :help "lume spawn <id> [--agent A]                 run a worker on an item, in the foreground (a driver's is a request)"}
              {:path ["allow"] :needs :session :usage allow-usage
               :run (fn [{:keys [session]} args] (allow session args))
               :help "lume allow \"<cmd>\"                          human at a tty, types yes: let workers run commands starting with <cmd> (their gate is always allowed)"}
              {:path ["allow" "--revoke"] :needs :session :usage revoke-usage
               :run (fn [{:keys [session]} args] (revoke session args))
               :help "lume allow --revoke \"<cmd>\"                 human at a tty, types yes: workers may no longer run <cmd>"}]})
