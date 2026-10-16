(ns lume.agents
  "Adapters: the only place that knows which agent CLI is in play.

  An adapter turns a prompt into a command line, the command's output lines
  into progress while it runs, and its whole output into a short summary for
  the log."
  (:require [cheshire.core :as json]
            [clojure.string :as str]))

(defmulti command
  "argv that runs `agent` on `prompt`, in the current directory."
  (fn [agent _prompt] agent))

(defmulti summarize
  "{:result \"short text\"} plus anything else worth logging, from raw stdout."
  (fn [agent _out] agent))

(defmulti progress
  "Fold one line of stdout into a running spawn's progress, a small map like
  {:steps 3 :last \"short text\"} that is logged as it changes."
  (fn [agent _progress _line] agent))

(defmethod summarize :default [_ out]
  {:result (last (remove str/blank? (str/split-lines (str out))))})

(defmethod progress :default [_ prog line]
  (if (str/blank? line)
    prog
    (-> prog (update :steps (fnil inc 0)) (assoc :last line))))

;; Claude Code. Extra flags (permissions, model) come from LUME_CLAUDE_ARGS.
;; The default lets workers edit files and run lume, so they can add notes and items.

(defn- claude-args []
  (if-let [args (System/getenv "LUME_CLAUDE_ARGS")]
    (remove str/blank? (str/split args #"\s+"))
    ["--permission-mode" "acceptEdits"
     "--allowedTools" "Bash(lume:*),Bash(bin/lume:*)"]))

;; stream-json writes one event per line as it happens, so a running spawn can be followed.
(defmethod command :claude [_ prompt]
  (into ["claude" "-p" prompt "--output-format" "stream-json" "--verbose"] (claude-args)))

(defn- parse [line]
  (try (json/parse-string line) (catch Exception _ nil)))

(defmethod summarize :claude [_ out]
  (if-let [{:strs [result total_cost_usd session_id is_error]}
           (->> (str/split-lines (str out)) (keep parse) (filter #(= "result" (get % "type"))) last)]
    (cond-> {:result result :agent-session session_id}
      total_cost_usd (assoc :cost-usd total_cost_usd)
      is_error (assoc :agent-error true))
    (summarize :default out)))

(defn- tool-call [{:strs [name input]}]
  (let [arg (some #(get input %) ["command" "file_path" "pattern" "path" "url" "description"])]
    (str name (when arg (str " " arg)))))

;; Steps are tool calls; :last is the latest tool call, or text when the agent is talking.
(defmethod progress :claude [_ prog line]
  (let [{:strs [type message]} (parse line)
        content (when (= "assistant" type) (get message "content"))
        tools (filter #(= "tool_use" (get % "type")) content)
        text (some #(when (= "text" (get % "type")) (get % "text")) content)]
    (cond (seq tools) (-> prog
                          (update :steps (fnil + 0) (count tools))
                          (assoc :last (tool-call (last tools))))
          (not (str/blank? text)) (assoc prog :last text)
          :else prog)))
