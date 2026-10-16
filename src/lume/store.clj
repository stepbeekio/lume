(ns lume.store
  "Where lume keeps state, and the append-only session log.

  All state lives under LUME_HOME (default ~/.lume), keyed by repo; nothing is
  written inside the project. The log is one EDN map per line and is the whole
  truth: everything else is a projection of it."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.nio.channels FileChannel)
           (java.nio.file OpenOption StandardOpenOption)
           (java.time Instant LocalDateTime)
           (java.time.format DateTimeFormatter)))

(def ^:dynamic *home*
  (or (System/getenv "LUME_HOME") (str (fs/path (fs/home) ".lume"))))

(def ^:dynamic *session*
  "The session workers inherit; tests bind it to nil so they never touch it."
  (System/getenv "LUME_SESSION"))

(defn repo-root
  "The git toplevel of the current directory, or the directory itself."
  []
  (let [{:keys [exit out]} (try (p/sh ["git" "rev-parse" "--show-toplevel"])
                                (catch Exception _ {:exit 1}))]
    (if (zero? exit) (str/trim out) (str (fs/cwd)))))

(defn repo-dir [root]
  (str (fs/path *home* "repos" (format "%s-%08x" (fs/file-name root) (hash root)))))

(defn- log-file [session] (str (fs/path session "log.edn")))

(defn read-events [session]
  (let [f (log-file session)]
    (if (fs/exists? f)
      (with-open [r (java.io.PushbackReader. (io/reader f))]
        (loop [events (transient [])]
          (let [e (edn/read {:eof r} r)]
            (if (identical? r e) (persistent! events) (recur (conj! events e))))))
      [])))

(defn- with-lock [session f]
  (fs/create-dirs session)
  (with-open [ch (FileChannel/open (fs/path session ".lock")
                                   (into-array OpenOption [StandardOpenOption/CREATE
                                                           StandardOpenOption/WRITE]))]
    (.lock ch)
    (f)))

(defn append!
  "Append the event built by (f events) and return it. f runs under the session
  lock and sees the log as it is at the moment of writing, so ids derived from
  it can't collide between concurrent writers. If f returns nil, nothing is
  written and nil is returned."
  [session f]
  (with-lock session
    (fn []
      (when-let [e (f (read-events session))]
        (let [event (merge {:t (str (Instant/now))} e)]
          (spit (log-file session) (str (pr-str event) "\n") :append true)
          event)))))

;; Sessions

(defn new-session!
  "Start a session with id yyyyMMdd-HHmmss, suffixed -2, -3… if that second is
  taken. The directory is claimed with an atomic create, so two inits in the
  same second (even in different processes) never share a log."
  [root]
  (let [stamp (.format (LocalDateTime/now) (DateTimeFormatter/ofPattern "yyyyMMdd-HHmmss"))
        sessions (fs/path (repo-dir root) "sessions")]
    (fs/create-dirs sessions)
    (loop [n 1]
      (let [id (if (= 1 n) stamp (str stamp "-" n))
            session (str (fs/path sessions id))
            claimed (try (fs/create-dir session) true
                         (catch java.nio.file.FileAlreadyExistsException _ false))]
        (if claimed
          (do (spit (str (fs/path (repo-dir root) "current")) id)
              session)
          (recur (inc n)))))))

(defn current-session
  "The session to act on: LUME_SESSION if set (workers inherit it), else the
  repo's current session. nil if there is none."
  [root]
  (or *session*
      (let [current (fs/path (repo-dir root) "current")]
        (when (fs/exists? current)
          (str (fs/path (repo-dir root) "sessions" (str/trim (slurp (str current)))))))))

(defn session-id [session] (fs/file-name session))

(defn- id-order
  "Ids sort by their stamp, then by suffix as a number, so -10 follows -9."
  [id]
  (let [[date time n] (str/split id #"-")]
    [(str date "-" time) (or (some-> n parse-long) 1)]))

(defn sessions
  "The repo's sessions, newest first."
  [root]
  (let [dir (fs/path (repo-dir root) "sessions")]
    (if (fs/exists? dir)
      (->> (fs/list-dir dir) (filter fs/directory?) (map str)
           (sort-by (comp id-order session-id) #(compare %2 %1)))
      [])))

(defn find-session
  "The repo's session with this id, or nil."
  [root id]
  (first (filter #(= id (session-id %)) (sessions root))))
