(ns lume.cli
  "The lume command line. Output is agent-first: short, structured, bounded."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
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

(defn item-add [session [intent]]
  (when (str/blank? intent) (fail! "usage: lume item add \"<intent>\""))
  (let [e (store/append! session (fn [events]
                                   {:type :item/added :item/id (next-id "i" :item/added events)
                                    :intent intent :by (actor)}))]
    (:item/id e)))

(defn item-close [session [id disposition why]]
  (when-not (#{"done" "dropped"} disposition)
    (fail! "usage: lume item close <id> done|dropped \"<why>\""))
  (store/append! session (fn [events]
                           (open-item events id)
                           {:type :item/closed :item/id id :disposition (keyword disposition)
                            :why why :by (actor)}))
  (str id " " disposition))

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
               ["Work in the current directory. When you finish, end with a short summary"
                "of what you did and anything left undone."]))))

(defn spawn [session root [id & opts]]
  (when (str/blank? id) (fail! "usage: lume spawn <id> [--agent claude]"))
  (let [agent (keyword (or (second (drop-while #(not= "--agent" %) opts)) "claude"))
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
          {:keys [exit]} @(p/process (agents/command agent (worker-prompt events item))
                                     {:dir root :in "" :out :write :out-file (fs/file out)
                                      :err :write :err-file (fs/file (str out ".err"))
                                      :extra-env {"LUME_SESSION" session "LUME_ACTOR" sid}})
          summary (agents/summarize agent (slurp out))
          exited (store/append! session (constantly (merge {:type :spawn/exited :spawn/id sid :exit exit
                                                           :ms (- (System/currentTimeMillis) t0)}
                                                          summary)))]
      (deliver done true)
      (.removeShutdownHook (Runtime/getRuntime) hook)
      (str sid " " id " exit " exit " in " (quot (:ms exited) 1000) "s"
           (when-let [r (:result exited)] (str " · " (str/replace (str r) #"\s+" " ")))
           "\n" out))))

(defn show-log [session [flag n]]
  (let [events (store/read-events session)
        n (if (= "-n" flag) (parse-long n) 20)]
    (str/join "\n" (map pr-str (take-last n events)))))

(defn show-status [session _]
  (status/render (store/session-id session) (store/read-events session) (Instant/now)))

(def usage
  "lume init \"<goal>\"                         start a session for this repo
lume status                                 one screen: goal, items, notes, recent events
lume item add \"<intent>\"                    add a work item
lume item close <id> done|dropped \"<why>\"   record a disposition
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
               (fail! "usage: lume item add|close ..."))
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
