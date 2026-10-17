(ns lume.ext
  "Extensions: features of your own, as .clj files in LUME_HOME/ext (user),
  <repo-dir>/ext (repo) or <session>/ext (session). One loads only once the
  human approves it (lume ext approve <path>, at a tty, typing its name) and
  only while its sha256 is the one approved; a changed or unapproved file is
  skipped and status says so in one line.

  An extension is <name>.clj, ns lume.ext.<name>, defining `feature`: a
  feature map (lume.kernel) kept to its own corner. It requires only
  lume.kernel and clojure.string/set/edn; its event types are :<name>/…; its
  folds and guards see only (get-in state [:ext name]) and its folds return
  only that; its commands are lume <name> …; its status sections are
  (fn [ext-state] lines), at most 2 lines each and 4 for all extensions. These
  checks are about shape, not a sandbox: approval, by a human who read the
  source, is what trusts the code."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [lume.kernel :as kernel :refer [actor fail! human-only!]]
            [lume.store :as store])
  (:import (java.security MessageDigest)))

(defn ext-dir [session] (str (fs/path session "ext")))

;; Scopes, loaded in this order: user (LUME_HOME/ext, every repo), repo
;; (<repo-dir>/ext under LUME_HOME, every session of the repo), session. A
;; session extension's approval is in its log; user and repo ones outlive any
;; session, so theirs are in the home log, LUME_HOME/log.edn (a7).

(defn- scopes
  "[{:scope :dir :log}] for session in repo root, in load order."
  [session root]
  (cond-> [{:scope "user" :dir (str (fs/path store/*home* "ext")) :log store/*home*}]
    root (conj {:scope "repo" :dir (str (fs/path (store/repo-dir root) "ext")) :log store/*home*})
    :always (conj {:scope "session" :dir (ext-dir session) :log session})))

(defn- sha256 [path]
  (let [d (.digest (MessageDigest/getInstance "SHA-256") (fs/read-all-bytes path))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) d))))

(defn- ext-name [path] (str/replace (fs/file-name path) #"\.clj$" ""))

;; Static checks: run on the source before it is approved and before it loads.

(def ^:private allowed-requires '#{lume.kernel clojure.string clojure.set clojure.edn})

;; What could reach past the ns form: loading or resolving code at run time.
(def ^:private forbidden
  '#{require use load load-file load-string load-reader import in-ns eval refer refer-clojure
     resolve ns-resolve requiring-resolve find-var intern alter-var-root var-set var
     create-ns the-ns find-ns ns-publics ns-interns ns-map})

(defn- read-forms [src]
  (with-open [r (java.io.PushbackReader. (java.io.StringReader. src))]
    (binding [*read-eval* false]
      (doall (take-while #(not= ::eof %) (repeatedly #(read {:eof ::eof} r)))))))

(defn- source-why
  "Why the source of extension `name` can't load, or nil: its ns form, what it
  requires, and what its body names."
  [name src]
  (let [[[head ns-sym & clauses] & body] (try (read-forms src)
                                              (catch Exception x [[:unreadable (ex-message x)]]))
        clauses (drop-while #(or (string? %) (map? %)) clauses)
        specs (mapcat rest clauses)
        required (set (map #(if (sequential? %) (first %) %) specs))
        aliases (set (keep #(when (sequential? %) (some-> (second (drop-while (partial not= :as) %)) str)) specs))
        ok-ns (into (conj aliases "clojure.core") (map str allowed-requires))
        syms (filter symbol? (mapcat #(tree-seq coll? seq %) body))
        bad (first (filter #(or (forbidden %)
                                (str/includes? (clojure.core/name %) ".")
                                (and (namespace %) (not (ok-ns (namespace %)))))
                           syms))]
    (cond (= :unreadable head) (str "can't read it: " ns-sym)
          (not= 'ns head) (str "its first form isn't (ns lume.ext." name " ...)")
          (not= (symbol (str "lume.ext." name)) ns-sym) (str "its ns is " ns-sym ", not lume.ext." name)
          (not-every? #(and (seq? %) (= :require (first %))) clauses) "its ns form may only :require"
          (seq (remove allowed-requires required))
          (str "it requires " (str/join " " (sort (remove allowed-requires required)))
               "; extensions require only lume.kernel and clojure.string/set/edn")
          bad (str "it names " bad "; extensions don't load, resolve or reach Java at run time"))))

(defn- taken
  "Every name a built-in feature uses: command words, event namespaces,
  section labels and feature names. An extension can't take one."
  [features]
  (set (concat (map (comp first :path) (mapcat :commands features))
               (keep namespace (mapcat (comp keys :events) features))
               (map :label (mapcat :status features))
               (map :name features))))

(defn- name-why [name features]
  (cond (not (re-matches #"[a-z][a-z0-9-]*" name)) (str "its name " name " isn't lowercase letters, digits and -")
        ((taken features) name) (str name " is a built-in name")))

;; Loading: the feature map's shape, then a feature kept to its corner.

(def ^:private feature-keys #{:name :events :init :fold :guards :reactions :commands :status})

(defn- feature-why [name f]
  (let [own? #(and (keyword? %) (= name (namespace %)))]
    (cond (not (map? f)) "it doesn't define feature as a map"
          (seq (remove feature-keys (keys f)))
          (str "its feature has " (str/join " " (sort (remove feature-keys (keys f))))
               "; extensions have " (str/join " " (sort feature-keys)))
          (not (contains? #{nil name} (:name f))) (str "its feature is named " (:name f) ", not " name)
          (seq (remove own? (keys (:events f))))
          (str "it declares " (pr-str (first (remove own? (keys (:events f))))) "; its event types are :" name "/…")
          (seq (remove own? (keys (:guards f))))
          (str "it guards " (pr-str (first (remove own? (keys (:guards f))))) "; it guards only its own :" name "/… types")
          (not-every? fn? (concat (vals (:fold f)) (vals (:guards f)))) "its folds and guards aren't all fns"
          (and (some? (:reactions f))
               (not (and (sequential? (:reactions f)) (every? #(and (keyword? (:on %)) (fn? (:run %))) (:reactions f)))))
          "its :reactions aren't [{:on type :run fn}]"
          (some #(not= name (first (:path %))) (:commands f))
          (str "it has command lume " (str/join " " (:path (first (remove #(= name (first (:path %))) (:commands f)))))
               "; its commands are lume " name " …")
          (not-every? #(and (every? string? (:path %)) (fn? (:run %))) (:commands f)) "its commands need a :path of words and a :run fn"
          (not-every? (comp fn? :lines) (:status f)) "its status sections need :lines (fn [ext-state] lines)")))

(def ^:private section-cap 2)
(def ^:private all-cap 4)

(defn- warn [& msg] (binding [*out* *err*] (println (str/join " " (cons "lume:" msg)))))

(defn- section-lines
  "A section's lines for the ext state, at most 2: the second says how many
  more there are when it cuts."
  [name lines s]
  (let [ls (try (map str (lines s)) (catch Exception x [(str "status failed: " (ex-message x))]))]
    (if (> (count ls) section-cap)
      [(first ls) (str "+" (dec (count ls)) " more — lume " name)]
      (vec ls))))

(defn- contained
  "f wrapped to see, and when fold? return, only the ext's own state."
  [name f fold?]
  (if fold?
    (fn [s e] (update-in s [:ext name] f e))
    (fn [s e] (f (get-in s [:ext name]) e))))

(defn- wrap-feature
  "The extension's feature as the kernel runs it. labels: every extension's
  status label, which together share all-cap lines."
  [name path f labels]
  {:name name
   :ext/path path
   :events (:events f)
   :fold (into {} (for [[type fold] (:fold f)] [type (contained name fold true)]))
   :guards (into {} (for [[type guard] (:guards f)] [type (contained name guard false)]))
   :reactions (for [{:keys [on run] :as r} (:reactions f)]
                (assoc r :run (fn [e] (try (run e)
                                           (catch Exception x (warn "extension" name "on" (pr-str (:type e)) "failed:" (ex-message x)))))))
   :commands (:commands f)
   :status (for [{:keys [lines order]} (:status f)
                 :let [ls #(section-lines name lines (get-in % [:ext name]))]]
             {:label name :order (or order 86)
              ;; Whole lines or none: cut at the room left, the frame's
              ;; "+N more" would be one line over it.
              :cap (fn [s shown]
                     (let [room (- all-cap (reduce + (map #(shown % 0) labels)))
                           n (count (ls s))]
                       (if (<= n room) n (max 0 (dec room)))))
              :more (str "— lume " name)
              :lines (fn [s _ _] (ls s))})})

(defn- load-ext
  "[the extension's feature, nil] or [nil why]."
  [name path src features]
  (if-let [why (or (name-why name features) (source-why name src))]
    [nil why]
    (try
      (locking kernel/ns-lock (load-file path))
      (let [f (some-> (ns-resolve (symbol (str "lume.ext." name)) 'feature) deref)]
        (if-let [why (feature-why name f)] [nil why] [f nil]))
      (catch Exception x [nil (str "it failed to load: " (ex-message x))]))))

(defn- approvals
  "{path sha256}, by the :ext/approved events in log."
  [log]
  (:extensions (kernel/project (kernel/read-events log))))

(defn scan
  "Each .clj in the session's scopes, user then repo then session, with its
  approval, as {:name :scope :path :sha256 :state} where :state is :approved,
  :changed or :unapproved by its scope's log, or :refused when an earlier
  scope has an extension of that name."
  [session root]
  (let [approved (memoize approvals)]
    (reduce (fn [rs {:keys [name] :as r}]
              (if-let [earlier (some #(when (= name (:name %)) %) rs)]
                (conj rs (assoc r :state :refused :why (str "the " (:scope earlier) " extension " name " has its name")))
                (conj rs r)))
            []
            (for [{:keys [scope dir log]} (scopes session root)
                  :when (fs/directory? dir)
                  f (sort (map str (fs/glob dir "*.clj")))
                  :let [path (str (fs/absolutize f)) sha (sha256 path) ok (get (approved log) path)]]
              {:name (ext-name path) :scope scope :path path :sha256 sha
               :state (cond (nil? ok) :unapproved (= sha ok) :approved :else :changed)}))))

(defn with-extensions
  "features, then every approved extension in session's scopes that passes
  its checks, kept to its corner, after one that holds their initial states
  and what happened to each (:ext/report, which lume ext and status read)."
  [features session]
  (let [report (when session
                 (binding [kernel/*features* features]
                   (scan session (:repo (kernel/project (kernel/read-events session))))))]
    (if (empty? report)
      features
      (let [loaded (for [{:keys [name path state] :as r} report]
                     (if (= :approved state)
                       (let [[f why] (load-ext name path (slurp path) features)]
                         (if f (assoc r :state :loaded :feature f) (assoc r :state :refused :why why)))
                       r))
            ok (filter :feature loaded)
            labels (cons "ext" (map :name ok))]
        (-> (vec features)
            (conj {:name "extensions"
                   :init {:ext (into {} (for [{:keys [name feature]} ok] [name (:init feature)]))}
                   :ext/report (map #(dissoc % :feature) loaded)})
            (into (for [{:keys [name path feature]} ok] (wrap-feature name path feature labels))))))))

(defn run-session
  "The session a run acts on, for loading its extensions: LUME_SESSION, else
  the repo's current one, else nil."
  []
  (or store/*session* (try (store/current-session (store/repo-root)) (catch Exception _ nil))))

;; lume ext

(defn- report [] (some :ext/report (kernel/features)))

(defn- state-text [{:keys [state why]}]
  (case state
    :loaded "loaded"
    :unapproved "unapproved"
    :changed "changed since approval"
    :refused (str "refused: " why)
    (name state)))

(defn- session-scopes [session]
  (scopes session (:repo (kernel/project (kernel/read-events session)))))

(defn ext-list [session]
  (let [rs (report)
        st (when (seq rs) (kernel/project (kernel/read-events session)))]
    (if (empty? rs)
      (str "no extensions; put <name>.clj in one of these and lume ext approve it:\n"
           (str/join "\n" (for [{:keys [scope dir]} (session-scopes session)] (str scope " " dir))))
      (str/join "\n"
                (for [{:keys [name scope sha256 state] :as r} rs]
                  (str name " " (state-text r) " · " scope " · sha256 " (subs sha256 0 12)
                       (case state
                         :loaded (str " · state " (kernel/clip-to 60 (pr-str (get-in st [:ext name]))))
                         (:unapproved :changed) (str " — lume ext approve " (if (= "session" scope) name (:path r)))
                         "")))))))

(def ^:private approve-usage "usage: lume ext approve <path|name>")

(defn- ext-file
  "The scope and path of the extension arg names: a path, or a file or
  extension name in one scope's dir."
  [session arg]
  (let [ss (session-scopes session)
        in (fn [{:keys [dir]} f] (and (fs/directory? dir)
                                      (= (str (fs/canonicalize dir)) (str (fs/canonicalize (fs/parent f))))))
        found (if (fs/regular-file? arg)
                (let [f (str (fs/absolutize arg))]
                  (or (some #(when (in % f) [[% f]]) ss)
                      (fail! "refused: extensions load only from" (str/join ", " (map :dir ss)))))
                (for [s ss
                      f [(fs/path (:dir s) arg) (fs/path (:dir s) (str arg ".clj"))]
                      :when (fs/regular-file? f)]
                  [s (str f)]))
        [[scope f] more] found]
    (cond (nil? f) (fail! "no extension" arg "in" (str/join ", " (map :dir ss)))
          more (fail! arg "is a" (str/join " and a " (map (comp :scope first) found)) "extension; approve one by its path")
          (not (str/ends-with? f ".clj")) (fail! "refused: an extension is a .clj file")
          :else [scope (str (fs/path (fs/absolutize (:dir scope)) (fs/file-name f)))])))

(defn approve [session [arg & more]]
  (when (or (str/blank? arg) (seq more)) (fail! approve-usage))
  (let [[{:keys [scope log]} path] (ext-file session arg)
        name (ext-name path)
        src (slurp path)
        sha (sha256 path)
        reach (case scope "user" "every lume command, in every repo" "repo" "every lume command in this repo" "every lume command")]
    (some->> (or (name-why name (remove :ext/path (kernel/features))) (source-why name src)) (fail! "refused:"))
    (human-only! "only the human approves extensions; ask with lume ask \"<question>\""
                 (str src "\n\nApprove " scope " extension " name " (" path ", sha256 " (subs sha 0 12) ") to load into " reach "?")
                 name)
    (kernel/append! log (constantly {:type :ext/approved :ext/name name :path path :sha256 sha :by (actor)}))
    (str "approved " scope " extension " name " · sha256 " (subs sha 0 12) " · it loads into " reach " from the next one")))

(defn- report-lines [_ _ _]
  (when-let [off (seq (remove #(= :loaded (:state %)) (report)))]
    [(str (str/join " · " (map #(str (:name %) " " (state-text %)) off)) " — lume ext")]))

(def feature
  {:name "ext"
   :events {:ext/approved {:req #{:ext/name :path :sha256} :human true}}
   :init {:extensions {}}
   :fold {:ext/approved (fn [s e] (assoc-in s [:extensions (:path e)] (:sha256 e)))}
   :status [{:label "ext" :order 85 :cap 1 :lines report-lines}]
   :commands [{:path ["ext"] :needs :session :usage "usage: lume ext | lume ext approve <path|name>"
               :run (fn [{:keys [session]} args]
                      (if (seq args) (fail! "usage: lume ext | lume ext approve <path|name>") (ext-list session)))
               :help "lume ext                                    extensions (user, repo, session): loaded, unapproved, changed or refused"}
              {:path ["ext" "approve"] :needs :session :usage approve-usage
               :run (fn [{:keys [session]} args] (approve session args))
               :help "lume ext approve <path|name>                human at a tty, reads the source, types its name: load it while unchanged"}]})
