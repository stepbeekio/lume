(ns lume.clean
  "Clean: every trace of lume for this repo, or one session's. Lume writes
  nothing inside the repo, so that is its state under LUME_HOME, plus any
  refs/lume/* and git worktrees kept under that state. The log is the whole
  truth, so workers and drivers may not delete it."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [lume.kernel :refer [actor fail! sh-out take-flags]]
            [lume.store :as store]))

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
  (let [[pos {[id & ids] "--session" yes "--yes"}] (take-flags args #{"--session"} #{"--yes"} clean-usage)]
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

(def feature
  {:name "clean"
   :commands [{:path ["clean"] :needs :root :usage clean-usage
               :run (fn [{:keys [root]} args] (clean root args))
               :help "lume clean [--session <id>] [--yes]         remove every trace of lume for this repo (or one session):
                                            its state, refs/lume/*, worktrees; lists them, refused without --yes"}]})
