(ns lume.drive
  "Drive: a loop of fresh driver agents, each making one decision from
  status. A driver's spawns are requests, run in order once its turn ends,
  until the cadence hands back early. A drive re-execs itself when the lume
  source it loaded changes, and with --wait-on-ask waits for answers on ntfy
  instead of stopping on an open ask."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [lume.ask :as ask]
            [lume.kernel :as kernel :refer [child-env fail! next-id take-flags timed]]
            [lume.note :as note]
            [lume.ntfy :as ntfy]
            [lume.policy :as policy]
            [lume.status :as status])
  (:import (java.time Instant)))

(defn- driver-prompt [session did closing]
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
             (if closing
               (str "\nNo item is open, but " closing " exited after the last driver turn, so this is the closing\n"
                    "turn: review its changes and commit them, or say what's wrong.\n")
               "")
             (status/render (kernel/session-id session) (kernel/read-events session) (Instant/now))]))

(defn- drive-turn
  "Run one driver agent until it exits, and return its summary."
  [session root agent did allow closing]
  (let [out (fs/path session "drives" (str did ".out"))]
    (fs/create-dirs (fs/parent out))
    (let [[{:keys [exit]} ms] (timed #(deref (p/process ((requiring-resolve 'lume.agents/command) agent (driver-prompt session did closing) {:allow allow :role :driver})
                                                        {:dir root :in ""
                                                         :out :write :out-file (fs/file out)
                                                         :err :write :err-file (fs/file (str out ".err"))
                                                         :extra-env (child-env {"LUME_SESSION" session "LUME_ACTOR" did "LUME_SPAWNED_AS" did})})))]
      (assoc ((requiring-resolve 'lume.agents/summarize) agent (slurp (fs/file out)))
             :exit exit :out (str out) :ms ms))))

(defn- hand-back-early
  "Why a batch stops before its next spawn, from the events since the last
  one started: an ask is open (a plateau's, say), the budget is reached, or
  under on gate-fail, the spawn's gate failed. nil to carry on."
  [events since id]
  (let [state (kernel/project events)
        gate (->> (drop since events) (filter #(and (= :gate/ran (:type %)) (= id (:item/id %)))) last)]
    (cond (seq (ask/open-asks state)) "an ask is open"
          (policy/over-budget state) "the budget is reached"
          (and (:gate-fail (policy/cadence state)) gate (not (zero? (:exit gate))))
          (str "on gate-fail: " id " gate " (:gate/id gate) " failed"))))

(defn- spawn!
  "Run lume spawn id with agent, as the spawn feature's command does."
  [session root id agent]
  (let [{:keys [run]} (or (some #(when (= ["spawn"] (:path %)) %) (kernel/commands))
                          (fail! "drive needs the spawn command"))]
    (run {:session session :root root} [id "--agent" agent])))

(defn- run-requests
  "Run a turn's spawn requests in order until the cadence hands back early; a
  cut batch logs :drive/handed-back with the items it skipped. A request that
  names no agent runs the drive's."
  [session root agent did say requests]
  (loop [[{id :item/id a :agent} & more] requests]
    (when id
      (let [since (count (kernel/read-events session))]
        (say (try (spawn! session root id (name (or a agent)))
                  (catch clojure.lang.ExceptionInfo e (ex-message e))))
        (let [why (when (seq more) (hand-back-early (kernel/read-events session) since id))]
          (if why
            (let [skipped (mapv :item/id more)]
              (kernel/append! session (constantly {:type :drive/handed-back :drive/id did :why why
                                                   :skipped skipped}))
              (say (str did " handed back: " why "; skipped " (str/join " " skipped))))
            (recur more)))))))

;; A spawn that closes the last item leaves its diff in the working copy, and
;; "no open items" would end the drive before any driver reviewed it. So a
;; spawn that exited after the last driver turn earns one closing turn.

(defn- unreviewed-spawn
  "The id of the last spawn that exited after the last driver turn, or nil."
  [events]
  (->> (reverse events) (take-while #(not= :drive/turn (:type %)))
       (filter #(= :spawn/exited (:type %))) first :spawn/id))

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
  "Replace this process with lume from src, run with args and env added."
  (fn [src args env] (p/exec {:extra-env env} (into ["bb" "-cp" src "-m" "lume.cli"] args))))

(def ^:private drive-usage "usage: lume drive [--turns N] [--agent claude|claude-acp|devin] [--wait-on-ask]")

(defn- drive-args
  "drive's args as {:turns :agent :from :wait}, or a usage error.
  --from is the turn a reloaded drive resumes at."
  [args]
  (let [[pos {[turns] "--turns" [agent] "--agent" [from] "--from" wait "--wait-on-ask"}]
        (take-flags args #{"--turns" "--agent" "--from"} #{"--wait-on-ask"} drive-usage)
        turns (parse-long (or turns "10"))
        from (parse-long (or from "1"))]
    (when-not (and (empty? pos) turns (pos? turns) from (pos? from)) (fail! drive-usage))
    (when (and wait (not ntfy/*ntfy*))
      (fail! "--wait-on-ask needs LUME_NTFY (where asks are sent, and answers read unless LUME_NTFY_REPLY is set)"))
    {:turns turns :agent agent :from from :wait (boolean wait)}))

(defn- source-changed?
  "Whether the lume source changed since the drive loaded it and the new code
  loads, so the drive should re-exec before turn n. A change that doesn't
  load is noted once (it becomes the loaded fingerprint) and the drive runs on.
  Returns [changed? loaded], loaded the fingerprint to compare next time."
  [session src loaded n]
  (let [now (fingerprint src)
        broken (when (not= now loaded) (load-error src))]
    (cond
      (= now loaded) [false loaded]
      broken (do (note/add! session (str "drive: lume source changed but doesn't load, so turn " n
                                         " runs the code the drive started with: " broken))
                 [false now])
      :else [true loaded])))

(defn- next-step
  "What the drive does before turn n, from the log and opts: [:wait] for an
  answer to the open ask, [:stop why], [:reload] onto changed source, or
  [:turn closing], closing the spawn an idle session's turn reviews (or nil)."
  [state events n {:keys [turns wait]} changed]
  (let [idle (not-any? #(= :open (:state %)) (vals (:items state)))
        closing (when idle (unreviewed-spawn events))
        ask (first (ask/open-asks state))
        over (policy/over-budget state)]
    (cond
      (and ask wait) [:wait]
      ask [:stop (str "ask " (:id ask) " is open: " (:question ask) " — answer with lume answer " (:id ask) " \"<text>\"")]
      over [:stop over]
      (and idle (not closing)) [:stop "no open items"]
      (and idle (> n turns)) [:stop (str "no open items, and no turn left to review " closing "'s changes: they're uncommitted")]
      (> n turns) [:stop (str turns " turns run")]
      changed [:reload]
      :else [:turn closing])))

(defn- reload!
  "Log :drive/reloaded and re-exec the drive on src, resuming at turn n of
  this session: named in its env, since the drive may not be on LUME_SESSION's
  (lume run inside a session drives a new one)."
  [session src {:keys [turns agent wait]} n]
  (kernel/append! session (constantly {:type :drive/reloaded :n n}))
  (*reexec* src (cond-> ["drive" "--turns" (str turns) "--agent" agent "--from" (str n)]
                  wait (conj "--wait-on-ask"))
            {"LUME_SESSION" (str session)}))

(defn- turn!
  "Run driver turn n, then the spawns it requested. Returns why the drive
  stops if the turn logged nothing and wasn't a closing turn, else nil."
  [session root {:keys [agent]} n closing allow say]
  (let [{did :drive/id} (kernel/append! session (fn [events]
                                                  {:type :drive/turn :drive/id (next-id "d" :drive/turn events)
                                                   :n n}))
        {:keys [exit result out] :as turn} (drive-turn session root (keyword agent) did allow closing)
        logged (rest (drop-while #(not= did (:drive/id %)) (kernel/read-events session)))]
    ;; Logged after reading what the turn did, so it never counts as the turn's work.
    (kernel/append! session (constantly (merge {:type :drive/exited :drive/id did}
                                               turn)))
    (kernel/react-for! session did)
    (say (str did " exit " exit (when result (str " · " (kernel/one-line result))) "\n" out))
    (if (and (empty? logged) (not closing))
      (str did " logged no events")
      (do (run-requests session root agent did say (filter #(= :spawn/requested (:type %)) logged))
          nil))))

(defn drive
  "Run up to :turns (10) driver turns from turn :from (1), each followed by
  the spawns it requested, with :agent (the default agent) and, on :wait, an
  open ask waited on rather than stopped at. Prints a line per turn and spawn
  and returns why it stopped."
  [session root opts]
  (let [{:keys [turns agent from wait] :as opts} (merge {:turns 10 :from 1 :wait false} opts
                                                        {:agent (or (:agent opts) kernel/*default-agent*)})
        replies (delay (ntfy/subscribe! (ntfy/reply-url)))
        src *source*
        say #(do (println %) (flush))]
    (when (= 1 from)
      (kernel/append! session (constantly {:type :drive/started :turns turns :agent (keyword agent)})))
    (let [finished! (kernel/exit-hook! session {:type :drive/stopped :why "interrupted"})
          stop! (fn [why]
                  (finished!)
                  (kernel/append! session (constantly {:type :drive/stopped :why why}))
                  (str "drive stopped: " why))]
      ;; Waiting, the drive sends asks itself, with their codes.
      ;; Bound, not passed: the ntfy reactions to the drive's appends read them.
      (binding [ntfy/*ask-notices* (not wait) ntfy/*sent* (atom #{})]
        (try
          ;; loaded is the source fingerprint the drive runs on; codes the
          ;; reply codes of the asks it sent, {id code}.
          (loop [n from loaded (fingerprint src) codes {}]
            (let [events (kernel/read-events session)
                  state (kernel/project events)
                  [changed loaded] (if (and src (> n from))
                                     (source-changed? session src loaded n)
                                     [false loaded])
                  [step arg] (next-step state events n opts changed)]
              (case step
                :wait (recur n loaded (ntfy/wait-for-answer! session codes replies say))
                :stop (stop! arg)
                :reload (reload! session src opts n)
                :turn (if-let [why (turn! session root opts n arg (:allow state) say)]
                        (stop! why)
                        (recur (inc n) loaded codes)))))
          (finally (when (realized? replies) ((:stop @replies)))
                   (finished!)))))))

;; The fold: :drive is the latest drive, {:running :why :turn}, its stop's why
;; once stopped and the id of its last turn.

(defn- started [s _] (assoc s :drive {:running true}))

(defn- stopped [s {:keys [why]}] (update s :drive assoc :running false :why why))

(defn- turned [s e] (assoc-in s [:drive :turn] (:drive/id e)))

(def feature
  {:name "drive"
   :init {:drive nil}
   :fold {:drive/started started
          :drive/stopped stopped
          :drive/turn turned}
   :events {:spawn/requested {:req #{:item/id}}
            :drive/started {:req #{:turns :agent}}
            :drive/turn {:req #{:drive/id :n}}
            :drive/exited {:req #{:drive/id}}
            :drive/stopped {:req #{:why}}
            :drive/handed-back {:req #{:drive/id :why}}
            :drive/reloaded {:req #{:n}}}
   :commands [{:path ["drive"] :needs #{:session :root} :usage drive-usage
               :run (fn [{:keys [session root]} args] (drive session root (drive-args args)))
               :help "lume drive [--turns N] [--agent A] [--wait-on-ask]
                                            driver turns until an ask is open, a budget is reached, no items are open, or N (10) run;
                                            workers run the drive's agent. LUME_NTFY=<ntfy topic URL>: each
                                            ask opened and each drive stop is POSTed there (logged :notify/*).
                                            --wait-on-ask: on an open ask, send it with a one-time code and wait
                                            for 'aN <code> <answer>' on that topic (LUME_NTFY_REPLY, if set,
                                            is a separate reply topic) or lume answer, then go on"}]})
