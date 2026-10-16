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

(defn- actor [] (or (System/getenv "LUME_ACTOR") "human"))

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

(defn note [session [text]]
  (when (str/blank? text) (fail! "usage: lume note \"<text>\""))
  (store/append! session (constantly {:type :note/added :text text :by (actor)}))
  "noted")

(defn- worker-prompt [events item]
  (let [{:keys [goal notes]} (status/project events)]
    (str/join "\n"
              (concat
               ["You are a worker in a lume session."
                (str "Session goal: " goal)
                (str "Your work item (" (:id item) "): " (:intent item))]
               (when (seq notes)
                 (cons "Notes from the session:" (map #(str "- " (:text %)) notes)))
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

(defn spawn [session root [id & opts]]
  (when (str/blank? id) (fail! "usage: lume spawn <id> [--agent claude]"))
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
          proc (p/process (agents/command agent (worker-prompt events item))
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
lume note \"<text>\"                          steering for drivers and workers
lume spawn <id> [--agent claude]            run a worker on an item, in the foreground
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
      "spawn" (spawn (session) root (rest args))
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
