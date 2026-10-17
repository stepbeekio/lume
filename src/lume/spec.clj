(ns lume.spec
  "Harness specs: a session's feedback loop as one page of EDN.

    {:goal    \"cut cold build time; tests must stay green\"
     :gate    \"bb test\"                         ; default gate for items
     :allow   [\"bb test\" \"jj\"]                  ; commands workers may run
     :budget  {:usd 20 :spawns 30 :minutes 120}
     :cadence {:batch 3 :gate-fail true :plateau 4}
     :items   [{:intent \"cut cold build time\"
                :measure \"bb bench\" :runs 10 :drop 1 ; :gate too, if not the default
                :higher [\"hit_rate\"]               ; numbers where higher is better
                :before true}                      ; each spawn measures first too
               {:intent \"document the cache\"
                :items [{:intent \"note the eviction rule\"}]}]} ; emerged while on it

  Only :goal is required. `problems` is the schema, `from-state` folds a
  session back into a spec, and `events` are the log events a spec starts a
  session with. lume check, lume spec and lume run are its commands."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [lume.drive :as drive]
            [lume.kernel :as kernel :refer [actor clip-to fail! human-only! take-flags]]
            [lume.measure :as measure]
            [lume.policy :as policy]
            [lume.store :as store]))

;; The schema: each key's check returns a problem (without its path) or nil.

(defn- text? [v] (and (string? v) (not (str/blank? v))))
(defn- whole? [v] (and (int? v) (not (neg? v))))
(defn- positive? [v] (and (int? v) (pos? v)))

(defn- must [pred what] (fn [v] (when-not (pred v) (str "must be " what ", got " (pr-str v)))))

(def ^:private command (must text? "a command (non-blank string)"))

(def ^:private item-keys
  {:intent (must text? "a non-blank string")
   :gate command
   :measure command
   :runs (must positive? "a positive whole number")
   :drop (must whole? "a whole number")
   :higher (must #(and (sequential? %) (seq %) (every? text? %)) "a vector of number names")
   :before (must boolean? "true or false")
   :items nil})

(def ^:private spec-keys
  {:goal (must text? "a non-blank string")
   :gate command
   :allow nil
   :budget nil
   :cadence nil
   :items nil})

(defn- keys-problems
  "Problems with map m against schema: unknown keys, required keys missing,
  and each present key's check, as [path message]."
  [path m schema required]
  (concat
   (for [k (keys m) :when (not (contains? schema k))]
     [(conj path k) (str "unknown key; expected one of " (str/join " " (sort (keys schema))))])
   (for [k required :when (not (contains? m k))]
     [(conj path k) "is required"])
   (for [[k f] schema :let [v (get m k ::absent)]
         :when (and f (not= ::absent v))
         :let [p (f v)] :when p]
     [(conj path k) p])))

(declare items-problems)

(defn- item-problems [path item]
  (if-not (map? item)
    [[path (str "must be a map with :intent, got " (pr-str item))]]
    (concat (keys-problems path item item-keys [:intent])
            (when (and (some #(contains? item %) [:runs :drop :higher :before]) (not (contains? item :measure)))
              [[path ":runs, :drop, :higher and :before need a :measure"]])
            (let [{:keys [runs drop] :or {runs 1 drop 0}} item]
              (when (and (positive? runs) (whole? drop) (>= drop runs))
                [[(conj path :drop) (str "must be less than :runs (" runs "), got " drop)]]))
            (when (contains? item :items)
              (items-problems (conj path :items) (:items item))))))

;; An item's :items are items too, as the spec's are.
(defn- items-problems [path items]
  (if-not (sequential? items)
    [[path (str "must be a vector of items, got " (pr-str items))]]
    (apply concat (map-indexed (fn [i item] (item-problems (conj path i) item)) items))))

(defn problems
  "Every problem with a spec, as [path message] pairs, in a stable order; empty
  when it's valid. A path is where the problem is, as for get-in."
  [spec]
  (if-not (map? spec)
    [[[] (str "a spec must be a map, got " (pr-str spec))]]
    (let [{:keys [allow budget cadence items]} spec]
      (vec
       (concat
        (keys-problems [] spec spec-keys [:goal])
        (when (contains? spec :allow)
          (if-not (sequential? allow)
            [[[:allow] (str "must be a vector of commands, got " (pr-str allow))]]
            (keep-indexed (fn [i c] (when-let [p (command c)] [[:allow i] p])) allow)))
        (when (contains? spec :budget)
          (if-not (map? budget)
            [[[:budget] (str "must be a map of :usd :minutes :spawns, got " (pr-str budget))]]
            (keys-problems [:budget] budget
                           {:usd (must #(and (number? %) (not (neg? %))) "a non-negative number")
                            :minutes (must #(and (number? %) (not (neg? %))) "a non-negative number")
                            :spawns (must whole? "a whole number")}
                           [])))
        (when (contains? spec :cadence)
          (if-not (map? cadence)
            [[[:cadence] (str "must be a map of :batch :gate-fail :plateau, got " (pr-str cadence))]]
            (keys-problems [:cadence] cadence
                           {:batch (must positive? "a positive whole number")
                            :gate-fail (must boolean? "true or false")
                            :plateau (must positive? "a positive whole number")}
                           [])))
        (when (contains? spec :items)
          (items-problems [:items] items)))))))

(defn problem-text [[path message]]
  (if (seq path) (str (pr-str path) " " message) message))

(defn read-spec
  "Read a spec file: [spec nil], or [nil problem] when it can't be read as
  exactly one EDN form."
  [path]
  (try
    (let [r (java.io.PushbackReader. (java.io.StringReader. (slurp path)))
          spec (edn/read {:eof ::eof} r)]
      (cond (= ::eof spec) [nil [[] "is empty"]]
            (not= ::eof (edn/read {:eof ::eof} r)) [nil [[] "has more than one form; a spec is one map"]]
            :else [spec nil]))
    (catch java.io.FileNotFoundException _ [nil [[] "no such file"]])
    (catch Exception e [nil [[] (str "is not EDN: " (ex-message e))]])))

;; Spec ⇄ session

(defn- item-spec [default-gate {:keys [intent gate measure runs drop higher before]}]
  (cond-> {:intent intent}
    (and gate (not= gate default-gate)) (assoc :gate gate)
    measure (assoc :measure measure)
    (and measure runs (> runs 1)) (assoc :runs runs)
    (and measure drop (pos? drop)) (assoc :drop drop)
    (and measure (seq higher)) (assoc :higher higher)
    (and measure before) (assoc :before true)))

(defn from-state
  "The harness of a projected session (lume.status/project) as a spec: its
  goal, default gate, allow, budget, cadence and items in order, open ones
  only unless :all, each with its children (:parent) as its :items; a child
  whose parent is left out stands alone. Defaults are left out, so it reads
  as written."
  [{:keys [goal default-gate allow budget cadence items]} & {:keys [all]}]
  (let [items (->> (vals items) (sort-by :order) (filter #(or all (= :open (:state %)))))
        ids (set (map :id items))
        kids (group-by #(when (ids (:parent %)) (:parent %)) items)
        nest (fn nest [item]
               (cond-> (item-spec default-gate item)
                 (kids (:id item)) (assoc :items (mapv nest (kids (:id item))))))
        items (kids nil)]
    (cond-> {:goal goal}
      default-gate (assoc :gate default-gate)
      (seq allow) (assoc :allow allow)
      (seq budget) (assoc :budget budget)
      cadence (assoc :cadence (cond-> (select-keys cadence [:batch :plateau])
                                (:gate-fail cadence) (assoc :gate-fail true)))
      (seq items) (assoc :items (mapv nest items)))))

(defn- flat
  "Items with their :items after each, depth first, as [item parent] pairs,
  where parent is the index of the item's parent in the result, or nil."
  ([items] (flat [] nil items))
  ([out parent items]
   (reduce (fn [out item] (flat (conj out [(dissoc item :items) parent]) (count out) (:items item)))
           out items)))

(defn events
  "The log events (without :t or :by) that start a session from a valid spec,
  in order: started, gated, allowed, budgeted, cadence, then each item with its
  gate (the default unless its own) and measurement. Nested items follow their
  parent, numbered depth first, with :parent on their :item/added."
  [{:keys [goal gate allow budget cadence items] :as spec} repo]
  (concat
   [{:type :session/started :goal goal :repo repo}]
   (when gate [{:type :session/gated :gate gate}])
   (for [c (distinct allow)] {:type :session/allowed :command c})
   (when (seq budget) [(assoc (select-keys budget [:usd :spawns :minutes]) :type :session/budgeted)])
   (when cadence [(assoc (select-keys cadence [:batch :gate-fail :plateau]) :type :session/cadence)])
   (apply concat
          (map-indexed
           (fn [i [{:keys [intent measure runs drop higher before] :as item} parent]]
             (let [id (str "i" (inc i))
                   gate (or (:gate item) (:gate spec))]
               (concat [(cond-> {:type :item/added :item/id id :intent intent}
                          gate (assoc :gate gate)
                          parent (assoc :parent (str "i" (inc parent))))]
                       (when measure
                         [(cond-> {:type :item/measuring :item/id id :measure measure}
                            (and runs (> runs 1)) (assoc :runs runs)
                            (and drop (pos? drop)) (assoc :drop drop)
                            (seq higher) (assoc :higher (vec (sort (distinct higher))))
                            before (assoc :before true))]))))
           (flat items)))))

(defn pretty
  "A spec as EDN, one top-level key and one item per line, nested items too, so
  it reads as a page."
  [{:keys [items] :as spec}]
  (let [order [:goal :gate :allow :budget :cadence :items]
        line (fn [k v] (str (pr-str k) " " (pr-str v)))
        top (for [k order :when (and (contains? spec k) (not= :items k))] (line k (get spec k)))
        ;; Items at column col; an item's own :items start on the next line,
        ;; one step in.
        pad #(apply str "\n" (repeat % " "))
        items-str (fn items-str [col items]
                    (str/join (pad col)
                              (for [{kids :items :as item} items
                                    :let [s (pr-str (dissoc item :items))]]
                                (if-not (seq kids)
                                  s
                                  (str (subs s 0 (dec (count s))) " :items" (pad (inc col))
                                       "[" (items-str (+ 2 col) kids) "]}")))))
        item-lines (when (seq items)
                     [(str ":items [" (items-str 9 items) "]")])]
    (str "{" (str/join "\n " (concat top item-lines)) "}")))

;; Commands: check validates a file; spec prints the current session's
;; harness, folded from the log, so it can be saved and reused.

(def ^:private check-usage "usage: lume check <spec.edn>")

(defn- checked-spec
  "The spec in the file at path, or a failure listing every problem with it."
  [path]
  (let [[s unreadable] (read-spec path)
        ps (if unreadable [unreadable] (problems s))
        shown (take 20 ps)]
    (when (seq ps)
      (fail! (str/join "\n" (concat [(str path ": " (count ps) (if (= 1 (count ps)) " problem" " problems"))]
                                   (map #(str "  " (clip-to 200 (problem-text %))) shown)
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
        state (kernel/project (kernel/read-events session))
        closed (count (remove #(= :open (:state %)) (vals (:items state))))]
    (str (pretty (from-state state :all all))
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
        (doseq [e (events sp root)]
          (kernel/append! session (constantly (assoc e :by (actor)))))
        (say (str "session " (store/session-id session) " started from " path " · " (items-text (:items sp))))
        (when within
          (say (str "not made current: run inside " (store/session-id within)
                    "; lume status " (store/session-id session) " reads it")))
        (binding [store/*session* (when within session)]
          (doseq [item (->> (vals (:items (kernel/project (kernel/read-events session)))) (filter :measure) (sort-by :order))]
            (say (str "baseline " (measure/measure-row session (measure/run-measure session item)))))
          (if (zero? n)
            "--turns 0: not driving; lume drive, or drive by hand from lume status"
            (drive/drive session root (cond-> ["--turns" (str n)] agent (conj "--agent" agent)))))))))

(def feature
  {:name "spec"
   :commands [{:path ["check"] :needs :none :usage check-usage
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
                                            session starts alongside; the current one stays"}]})
