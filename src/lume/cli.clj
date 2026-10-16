(ns lume.cli
  "The lume command line. Output is agent-first: short, structured, bounded."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [lume.agents :as agents]
            [lume.status :as status]
            [lume.store :as store])
  (:import (java.time Instant)))

(defn- fail! [& msg]
  (throw (ex-info (str/join " " msg) {:lume/user-error true})))

(defn- next-id [prefix type events]
  (str prefix (inc (count (filter #(= type (:type %)) events)))))

(def ^:dynamic *actor*
  "Who is acting: a spawn id inside a worker, else \"human\". Tests bind it."
  (or (System/getenv "LUME_ACTOR") "human"))

(defn- actor [] *actor*)

(defn- driver? [] (boolean (re-matches #"d\d+" (actor))))

(defn- open-item [events id]
  (let [item (get-in (status/project events) [:items id])]
    (cond (nil? item) (fail! "no such item:" id)
          (not= :open (:state item)) (fail! id "is already" (name (:state item)))
          :else item)))

;; Commands. Each takes the session dir (or repo root, for init) and its args,
;; and returns the text to print.

(defn init [root [goal]]
  (when (str/blank? goal) (fail! "usage: lume init \"<goal>\""))
  (let [session (store/new-session! root)]
    (store/append! session (constantly {:type :session/started :goal goal :repo root :by (actor)}))
    (str "session " (store/session-id session) " started")))

(defn- flag [opts name]
  (second (drop-while #(not= name %) opts)))

(defn item-add [session [intent & opts]]
  (when (str/blank? intent) (fail! "usage: lume item add \"<intent>\" [--gate \"<cmd>\"]"))
  (let [gate (flag opts "--gate")
        e (store/append! session (fn [events]
                                   (cond-> {:type :item/added :item/id (next-id "i" :item/added events)
                                            :intent intent :by (actor)}
                                     gate (assoc :gate gate))))]
    (:item/id e)))

(defn item-gate [session [id gate]]
  (when (str/blank? gate) (fail! "usage: lume item gate <id> \"<cmd>\""))
  (store/append! session (fn [events]
                           (open-item events id)
                           {:type :item/gated :item/id id :gate gate :by (actor)}))
  (str id " gate " gate))

;; Gates: a shell command, run in the repo, whose exit code decides done.

(defn- run-gate
  "Run the item's gate and log a :gate/ran event, which is returned."
  [session {:keys [id gate]}]
  (let [root (:repo (status/project (store/read-events session)))
        dir (fs/create-dirs (fs/path session "gates"))
        tmp (fs/file (fs/create-temp-file {:dir dir :prefix "running-"}))
        t0 (System/currentTimeMillis)
        {:keys [exit]} @(p/process ["sh" "-c" gate] {:dir root :in "" :out :write :out-file tmp
                                                      :err :out :extra-env {"LUME_SESSION" session}})
        tail (last (remove str/blank? (str/split-lines (slurp tmp))))
        ran (store/append! session (fn [events]
                                     (let [gid (next-id "g" :gate/ran events)]
                                       (cond-> {:type :gate/ran :gate/id gid :item/id id :gate gate
                                                :exit exit :ms (- (System/currentTimeMillis) t0)
                                                :out (str (fs/path dir (str gid ".out"))) :by (actor)}
                                         tail (assoc :tail tail)))))]
    (fs/move tmp (:out ran))
    ran))

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

;; Only the human grants permissions: workers and agent drivers would grant themselves
;; whatever they need, so they ask with lume ask instead.
(defn allow [session [a b]]
  (let [revoke (= "--revoke" a)
        command (if revoke b a)]
    (when (str/blank? command) (fail! "usage: lume allow [--revoke] \"<cmd>\""))
    (when-not (= "human" (actor))
      (fail! "only the human changes permissions; ask with lume ask \"<question>\""))
    (store/append! session (constantly {:type (if revoke :session/disallowed :session/allowed)
                                        :command command :by (actor)}))
    (str (if revoke "workers may no longer run " "workers may run ") command)))

;; Questions for the human. Anyone may ask; only the human answers, and the
;; answer reaches status and workers alongside the notes.
(defn ask [session [question]]
  (when (str/blank? question) (fail! "usage: lume ask \"<question>\""))
  (:ask/id (store/append! session (fn [events]
                                    {:type :ask/opened :ask/id (next-id "a" :ask/opened events)
                                     :question question :by (actor)}))))

(defn answer [session [id text]]
  (when (or (str/blank? id) (str/blank? text)) (fail! "usage: lume answer <id> \"<text>\""))
  (when-not (= "human" (actor))
    (fail! "only the human answers asks; record what you think with lume note"))
  (store/append! session (fn [events]
                           (case (get-in (status/project events) [:asks id :state])
                             nil (fail! "no such ask:" id)
                             :answered (fail! id "is already answered")
                             :open {:type :ask/answered :ask/id id :answer text :by (actor)})))
  (str id " answered"))

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
        (let [prog (agents/progress agent prog line)
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

(defn- run-spawn [session root id opts]
  (let [agent (keyword (or (flag opts "--agent") "claude"))
        events (store/read-events session)
        item (open-item events id)
        started (store/append! session (fn [events]
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
          ;; A worker may run the session's allowed commands, and its own gate.
          allowed (distinct (cond-> (:allow (status/project events)) (:gate item) (conj (:gate item))))
          proc (p/process (agents/command agent (worker-prompt events item) {:allow allowed})
                          {:dir root :in ""
                           :err :write :err-file (fs/file (str out ".err"))
                           :extra-env {"LUME_SESSION" session "LUME_ACTOR" sid}})
          {:keys [steps]} (follow! session sid agent proc out)
          {:keys [exit]} @proc
          summary (agents/summarize agent (slurp out))
          exited (store/append! session (constantly (cond-> (merge {:type :spawn/exited :spawn/id sid :exit exit
                                                                    :ms (- (System/currentTimeMillis) t0)}
                                                                   summary)
                                                      steps (assoc :steps steps))))]
      (deliver done true)
      (.removeShutdownHook (Runtime/getRuntime) hook)
      (str sid " " id " exit " exit " in " (quot (:ms exited) 1000) "s"
           (when-let [r (:result exited)] (str " · " (str/replace (str r) #"\s+" " ")))
           "\n" out
           ;; The worker may have closed the item itself; otherwise its gate decides.
           (let [item (get-in (status/project (store/read-events session)) [:items id])]
             (when (and (:gate item) (= :open (:state item)))
               (str "\n" (try (item-close session [id "done" (str "gate passed after " sid)])
                              (catch clojure.lang.ExceptionInfo e (ex-message e))))))))))

;; A driver's spawn is only a request: its turn ends first, then drive runs it.
(defn spawn [session root [id & opts]]
  (when (str/blank? id) (fail! "usage: lume spawn <id> [--agent claude]"))
  (if (driver?)
    (do (store/append! session (fn [events]
                                 (open-item events id)
                                 (cond-> {:type :spawn/requested :item/id id :by (actor)}
                                   (flag opts "--agent") (assoc :agent (keyword (flag opts "--agent"))))))
        (str id " spawn requested; it runs when your turn ends, so finish the turn now"))
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
             "when only the human can decide (the loop stops until they answer)."
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
    (let [{:keys [exit]} @(p/process (agents/command agent (driver-prompt session did) {:allow allow :role :driver})
                                     {:dir root :in ""
                                      :out :write :out-file (fs/file out)
                                      :err :write :err-file (fs/file (str out ".err"))
                                      :extra-env {"LUME_SESSION" session "LUME_ACTOR" did}})]
      (assoc (agents/summarize agent (slurp (fs/file out))) :exit exit :out (str out)))))

(defn drive
  "Run up to --turns driver turns, each followed by the spawns it requested,
  printing a line per turn and spawn. Returns why it stopped."
  [session root opts]
  (let [turns (parse-long (or (flag opts "--turns") "10"))
        agent (or (flag opts "--agent") "claude")
        _ (when-not (and turns (pos? turns)) (fail! "usage: lume drive [--turns N] [--agent claude]"))
        say #(do (println %) (flush))
        done (promise)
        stop! (fn [why]
                (deliver done true)
                (store/append! session (constantly {:type :drive/stopped :why why :by (actor)}))
                (str "drive stopped: " why))
        hook (Thread. #(when-not (realized? done)
                         (store/append! session (constantly {:type :drive/stopped :why "interrupted" :by (actor)}))))]
    (store/append! session (constantly {:type :drive/started :turns turns :agent (keyword agent) :by (actor)}))
    (.addShutdownHook (Runtime/getRuntime) hook)
    (try
      (loop [n 1]
        (let [{:keys [items asks allow]} (status/project (store/read-events session))
              ask (->> (vals asks) (filter #(= :open (:state %))) (sort-by #(parse-long (subs (:id %) 1))) first)]
          (cond
            ask (stop! (str "ask " (:id ask) " is open: " (:question ask) " — answer with lume answer " (:id ask) " \"<text>\""))
            (not-any? #(= :open (:state %)) (vals items)) (stop! "no open items")
            (> n turns) (stop! (str turns " turns run"))
            :else
            (let [{did :drive/id} (store/append! session (fn [events]
                                                           {:type :drive/turn :drive/id (next-id "d" :drive/turn events)
                                                            :n n :by (actor)}))
                  {:keys [exit result out]} (drive-turn session root (keyword agent) did allow)
                  logged (rest (drop-while #(not= did (:drive/id %)) (store/read-events session)))]
              (say (str did " exit " exit (when result (str " · " (str/replace (str result) #"\s+" " "))) "\n" out))
              (if (empty? logged)
                (stop! (str did " logged no events"))
                (do (doseq [{id :item/id a :agent} (filter #(= :spawn/requested (:type %)) logged)]
                      (say (try (run-spawn session root id ["--agent" (name (or a :claude))])
                                (catch clojure.lang.ExceptionInfo e (ex-message e)))))
                    (recur (inc n))))))))
      (finally (.removeShutdownHook (Runtime/getRuntime) hook)))))

(defn show-log [session [flag n]]
  (let [events (store/read-events session)
        n (if (= "-n" flag) (parse-long n) 20)]
    (str/join "\n" (map pr-str (take-last n events)))))

(defn show-status [session _]
  (status/render (store/session-id session) (store/read-events session) (Instant/now)))

(def usage
  "lume init \"<goal>\"                         start a session for this repo
lume status                                 one screen: goal, items, notes, recent events
lume item add \"<intent>\" [--gate \"<cmd>\"]   add a work item; the gate's exit code decides done
lume item gate <id> \"<cmd>\"                 set or replace an open item's gate
lume item close <id> done|dropped \"<why>\"   record a disposition; done runs the gate, refused if it fails
lume note [--pin] \"<text>\"                  steering for drivers and workers; pinned notes never age out
lume note --unpin <id>                      let a pinned note age out like the rest
lume ask \"<question>\"                       put a question to the human; open asks top status
lume answer <id> \"<text>\"                   answer an ask (human only); answers join the notes
lume allow [--revoke] \"<cmd>\"               let workers run commands starting with <cmd> (their gate is always allowed)
lume spawn <id> [--agent claude]            run a worker on an item, in the foreground (a driver's is a request)
lume drive [--turns N] [--agent claude]     driver turns until an ask is open, no items are open, or N (10) run
lume log [-n N]                             raw events (default last 20)")

(defn run [args]
  (let [root (store/repo-root)
        [cmd sub & more] args
        session #(or (store/current-session root) (fail! "no session for this repo; run lume init \"<goal>\""))]
    (case cmd
      "init" (init root (rest args))
      "status" (show-status (session) nil)
      "item" (case sub
               "add" (item-add (session) more)
               "close" (item-close (session) more)
               "gate" (item-gate (session) more)
               (fail! "usage: lume item add|gate|close ..."))
      "note" (note (session) (rest args))
      "ask" (ask (session) (rest args))
      "answer" (answer (session) (rest args))
      "allow" (allow (session) (rest args))
      "spawn" (spawn (session) root (rest args))
      "drive" (drive (session) root (rest args))
      "log" (show-log (session) (rest args))
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
