(ns lume.inspect
  "Reading sessions: lume status, ls, log, replay and repl. They read any of a
  repo's sessions by id, and write nothing to it but repl/opened."
  (:require [clojure.string :as str]
            [lume.ask :as ask]
            [lume.item :as item]
            [lume.kernel :as kernel :refer [*read-line* ago clip clock fail! more session-by-id take-flags]]
            [lume.measure :as measure]
            [lume.status :as status]
            [lume.store :as store])
  (:import (java.time Instant)))

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

(defn- status-args
  "status's args as {:id :color}, or a usage error."
  [args]
  (let [[[id & stray] {color "--color"}] (take-flags args #{} #{"--color"} status-usage)]
    (when (seq stray) (fail! status-usage))
    {:id id :color color}))

(defn show-status [root args]
  (let [{:keys [id color]} (status-args args)
        session (session-by-id root id)]
    (status/render (store/session-id session) (store/read-events session) (Instant/now)
                   {:color (color? color (terminal?) (System/getenv "NO_COLOR"))
                    :past (not= (store/session-id session) (some-> (store/current-session root) store/session-id))})))

;; Past sessions: lume run makes several per repo. ls lists them; status and
;; log read any of them by id, and write nothing to it.

;; A session as one line. The outcome is what the session is doing or how it
;; ended, most pressing first.
(defn- outcome [{:keys [items spawns drive] :as state}]
  (let [running (->> (vals spawns) (remove #(or (:interrupted %) (contains? % :exit))) (sort-by :t) last)
        ask (first (ask/open-asks state))]
    (cond (:running drive) "driving"
          running (str "running " (:spawn/id running))
          ask (str "ask " (:id ask) " open")
          (and (seq items) (not-any? #(= :open (:state %)) (vals items))) "all closed"
          (:why drive) (str "stopped: " (:why drive))
          (empty? items) "no items"
          :else "idle")))

(defn session-line
  "One session for lume ls: id (* if current), goal, items, last event, outcome."
  [session-id events now current?]
  (let [{:keys [goal items] :as state} (status/project events)
        open (count (filter #(= :open (:state %)) (vals items)))]
    (str (if current? "* " "  ")
         (clip (- kernel/width 2)
               (str session-id " " (clip 40 (or goal "(no goal)"))
                    " · " open " open " (- (count items) open) " closed"
                    (when-let [t (:t (last events))] (str " · " (ago now t)))
                    " · " (outcome state))))))

(def ^:private ls-usage "usage: lume ls [--all]")

(defn- ls-args
  "ls's args as {:all}, or a usage error."
  [args]
  (let [[pos {all "--all"}] (take-flags args #{} #{"--all"} ls-usage)]
    (when (seq pos) (fail! ls-usage))
    {:all all}))

(defn ls [root args]
  (let [{:keys [all]} (ls-args args)
        sessions (store/sessions root)
        current (some-> (store/current-session root) store/session-id)
        now (Instant/now)
        shown (if all sessions (take 10 sessions))]
    (if (empty? sessions)
      "no sessions for this repo; run lume init \"<goal>\" or lume run <spec.edn>"
      (str/join "\n" (concat (for [s shown]
                               (session-line (store/session-id s) (store/read-events s) now (= (store/session-id s) current)))
                             (more sessions shown "more — lume ls --all"))))))

(def ^:private log-usage
  "usage: lume log [-n N] [--session <id>] [--type <t>] [--item <id>] [--by <actor>] [--grep <re>]")

;; Filters narrow the events before -n takes the last N, so a query is as
;; bounded as the plain log. --type matches a prefix of ns/name (note, spawn/);
;; --item is item show's events plus its spawns' progress; --grep searches the
;; event's EDN. Output stays EDN: the hint for what -n left out is a comment.
(defn- log-args
  "log's args as {:n :session :type :item :by :grep}, :grep a pattern, or a
  usage error. Each flag is given at most once."
  [args]
  (let [[pos flags] (take-flags args #{"-n" "--session" "--type" "--item" "--by" "--grep"} log-usage)
        {[n] "-n" [session] "--session" [t] "--type" [item] "--item" [by] "--by" [re] "--grep"} flags
        n (if n (parse-long n) 20)]
    (when (or (seq pos) (some #(next %) (vals flags)) (not (and n (<= 0 n)))) (fail! log-usage))
    {:n n :session session :type t :item item :by by
     :grep (when re (try (re-pattern re) (catch Exception e (fail! "bad --grep regex:" (ex-message e)))))}))

(defn- log-filter
  "The events matching every filter given."
  [events {t :type iid :item by :by re :grep}]
  (let [spawn-ids (when iid (set (get-in (status/project events) [:items iid :spawns])))
        of-item (when iid (item/item-event-pred iid spawn-ids))]
    (filterv (fn [e]
               (and (or (nil? t) (str/starts-with? (subs (str (:type e)) 1) t))
                    (or (nil? iid) (of-item e) (contains? spawn-ids (:spawn/id e)))
                    (or (nil? by) (= by (:by e)))
                    (or (nil? re) (re-find re (pr-str e)))))
             events)))

(defn show-log [root args]
  (let [{:keys [n session item] :as q} (log-args args)
        all (store/read-events (session-by-id root session))]
    (when (and item (not (get-in (status/project all) [:items item])))
      (fail! "no such item:" item))
    (let [events (log-filter all q)
          shown (take-last n events)]
      (str/join "\n" (concat (map #(str ";; " %) (more events shown (str "earlier events — -n " (count events))))
                             (map pr-str shown))))))

;; Replay: the session as it stood after its first N events, from the log
;; alone, with the clock stopped at event N so ages read as they did then. A
;; decision is a drive turn or a spawn; --step shows the screen after each in
;; turn, for a human at a terminal. Agents get the list of decisions instead,
;; each with the N that replays it.

(def ^:private replay-usage "usage: lume replay [<session>] [--at N] [--step] [-n N] [--color]")

;; A decision is a drive turn or a spawn, and ends with its exit event.
(defn decision? [e] (#{:drive/exited :spawn/exited} (:type e)))

(defn decision-text
  "What a decision did, in one line: its id, exit, then its result."
  [e]
  (str (or (:drive/id e) (:spawn/id e)) " " (if (:interrupted e) "interrupted" (str "exit " (:exit e)))
       (when-let [r (:result e)] (str " · " r))))

(defn- replay-screen
  "Status as it stood after the first n of events, with the clock stopped at
  event n and what the decision ending there did, if one does."
  [sid events n color]
  (let [shown (subvec events 0 n)
        e (last shown)]
    (status/render sid shown (Instant/parse (:t e))
                   {:color color :replay {:of (count events) :decided (when (decision? e) (decision-text e))}})))

(defn- decision-index [events from n]
  (let [ds (filter (fn [[_ e]] (decision? e)) (drop from (map-indexed (fn [i e] [(inc i) e]) events)))
        shown (take-last n ds)]
    (if (empty? ds)
      (str "no decisions" (when (pos? from) (str " after event " from)) " (drive turns or spawns); "
           (count events) " events — lume replay --at N")
      (str/join "\n" (concat (more ds shown (str "earlier decisions — -n " (count ds)))
                             (for [[i e] shown]
                               (clip 100 (str i " " (kernel/stamp (:t e)) " " (decision-text e))))
                             [(str "lume replay --at N: the screen after event N (of " (count events) ") · --step: each decision in turn")])))))

(defn- replay-args
  "replay's args as {:id :at :n :step :color}, or a usage error."
  [args]
  (let [[[id & stray] {[at & ats] "--at" [n & ns] "-n" step "--step" color "--color"}]
        (take-flags args #{"--at" "-n"} #{"--step" "--color"} replay-usage)
        n (if n (parse-long n) 20)
        at (when at (or (parse-long at) (fail! replay-usage)))]
    (when (or (seq stray) (seq ats) (seq ns) (not (and n (<= 0 n)))) (fail! replay-usage))
    {:id id :at at :n n :step step :color color}))

(defn replay [root args]
  (let [{:keys [id at n step color]} (replay-args args)
        color (color? color (terminal?) (System/getenv "NO_COLOR"))
        session (session-by-id root id)
        sid (store/session-id session)
        events (store/read-events session)
        m (count events)
        screen #(replay-screen sid events % color)]
    (when (and at (not (<= 1 at m))) (fail! (str "--at " at " is outside the log: events 1.." m)))
    (cond
      (and at (not step)) (screen at)
      (not step) (decision-index events 0 n)
      :else
      (let [ds (keep-indexed (fn [i e] (when (and (decision? e) (>= (inc i) (or at 1))) (inc i))) events)]
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
    (letfn [(refresh! []
              (let [events (store/read-events session)
                    state (status/project events)]
                (locking kernel/ns-lock
                  (intern ns-sym 'events events)
                  (intern ns-sym 'state state))
                (count events)))
            (render
              ([] (status/render sid @(ns-resolve ns-sym 'events) (Instant/now) {}))
              ([n] (replay-screen sid @(ns-resolve ns-sym 'events) n false)))]
      (locking kernel/ns-lock
        (binding [*ns* (create-ns ns-sym)]
          (refer-clojure)
          (require '[clojure.string :as str] '[lume.store :as store] '[lume.status :as status]
                   '[lume.spec :as spec] '[lume.agents :as agents] '[lume.kernel :as kernel]))
        (intern ns-sym 'session session)
        (intern ns-sym 'sid sid)
        (intern ns-sym 'refresh! refresh!)
        (intern ns-sym 'render render)
        (intern ns-sym 'measure (fn measure
                                  ([cmd] (measure cmd {}))
                                  ;; nREPL evaluates outside the run's binding.
                                  ([cmd opts] (binding [kernel/*features* (or kernel/*features*
                                                                              @(requiring-resolve 'lume.features/all))]
                                                (measure/try-measure session cmd opts))))))
      (refresh!))))

(defn start-repl
  "Start an nREPL whose user namespace (or opts :ns) is set up on session.
  Returns {:server :port :text}; stop it with babashka.nrepl.server/stop-server!."
  [session {:keys [port ns] :or {port 0 ns 'user}}]
  (let [server ((requiring-resolve 'babashka.nrepl.server/start-server!) {:port port :quiet true})
        port (.getLocalPort ^java.net.ServerSocket (:socket server))
        ;; The one write: the log shows prototyping happened, e.g. before a spec.
        _ (kernel/append! session (constantly {:type :repl/opened :port port}))
        n (repl-ns! ns session)]
    {:server server
     :port port
     :text (str/join "\n"
                     [(str "lume repl · " (store/session-id session) " · " n " events · nREPL on 127.0.0.1:" port)
                      (str "connect: clj-nrepl-eval -p " port " \"(count events)\"")
                      (str "in " ns ": events, state (status/project events), (render) status now, (render N) after event N,")
                      "  (refresh!) re-reads the log, (measure \"<cmd>\" {:runs N :drop K :higher [..]}) runs a"
                      "  candidate measurement as a spec would and returns the row, unlogged;"
                      "  aliases store status spec agents kernel str"
                      "runs until killed; logs repl/opened and writes nothing else"])}))

(defn- repl-args
  "repl's args as {:id :port}, or a usage error."
  [args]
  (let [[[id & stray] {[port & ports] "--port"}] (take-flags args #{"--port"} repl-usage)
        port (if port (or (parse-long port) (fail! repl-usage)) 0)]
    (when (or (seq stray) (seq ports)) (fail! repl-usage))
    {:id id :port port}))

(defn repl [root args]
  (let [{:keys [id port]} (repl-args args)
        {:keys [text]} (start-repl (session-by-id root id) {:port port})]
    (println text)
    (flush)
    @(promise)))

(def feature
  {:name "inspect"
   :events {:repl/opened {:req #{:port}}}
   ;; Progress is shown on its spawn; in recent it would crowd out everything else.
   :status [{:label "recent" :order 90 :cap 3
             :lines (fn [_ events _] (map #(kernel/event-line clock 70 %) (take-last 3 (remove #(= :spawn/progress (:type %)) events))))}]
   :commands [{:path ["status"] :needs :root :usage status-usage
               :run (fn [{:keys [root]} args] (show-status root args))
               :help "lume status [<session>] [--color]           one screen: goal, items, notes, recent events (colour on a tty);
                                            a past session's by id (lume ls)"}
              {:path ["ls"] :needs :root :usage ls-usage
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
                                            and lume.* in user; prints the port; writes nothing; runs until killed"}]})
