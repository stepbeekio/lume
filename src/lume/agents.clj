(ns lume.agents
  "Adapters: the only place that knows which agent CLI is in play.

  An adapter turns a prompt into a command line, and the command's output
  into a short summary for the log."
  (:require [cheshire.core :as json]
            [clojure.string :as str]))

(defmulti command
  "argv that runs `agent` on `prompt`, in the current directory."
  (fn [agent _prompt] agent))

(defmulti summarize
  "{:result \"short text\"} plus anything else worth logging, from raw stdout."
  (fn [agent _out] agent))

(defmethod summarize :default [_ out]
  {:result (last (remove str/blank? (str/split-lines (str out))))})

;; Claude Code. Extra flags (permissions, model) come from LUME_CLAUDE_ARGS.
;; The default lets workers edit files and run lume, so they can add notes and items.

(defn- claude-args []
  (if-let [args (System/getenv "LUME_CLAUDE_ARGS")]
    (remove str/blank? (str/split args #"\s+"))
    ["--permission-mode" "acceptEdits"
     "--allowedTools" "Bash(lume:*),Bash(bin/lume:*)"]))

(defmethod command :claude [_ prompt]
  (into ["claude" "-p" prompt "--output-format" "json"] (claude-args)))

(defmethod summarize :claude [_ out]
  (try
    (let [{:strs [result total_cost_usd session_id is_error]} (json/parse-string out)]
      (cond-> {:result result :agent-session session_id}
        total_cost_usd (assoc :cost-usd total_cost_usd)
        is_error (assoc :agent-error true)))
    (catch Exception _ (summarize :default out))))
