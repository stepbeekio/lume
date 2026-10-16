(ns lume.agents
  "Adapters: the only place that knows which agent CLI is in play.

  An adapter turns a prompt into a command line, the command's output lines
  into progress while it runs, and its whole output into a short summary for
  the log."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(defmulti command
  "argv that runs `agent` on `prompt`, in the current directory. `opts` has
  :allow, shell command prefixes the agent may run (e.g. \"bb test\"), and
  :role, :worker (edits files) or :driver (runs lume and jj, reads, never edits)."
  (fn [agent _prompt _opts] agent))

(defmulti summarize
  "{:result \"short text\"} plus anything else worth logging, from raw stdout."
  (fn [agent _out] agent))

(defmulti progress
  "Fold one line of stdout into a running spawn's progress, a small map like
  {:steps 3 :last \"short text\"} that is logged as it changes."
  (fn [agent _progress _line] agent))

(defmulti setup
  "What `lume install` does for `agent` on this machine: {:files [[path content]]
  :manual [\"line\"]}. Files are written by install (only when they differ);
  manual lines are printed, never applied, because they change the user's own
  agent settings."
  (fn [agent] agent))

(defmethod setup :default [agent]
  (throw (ex-info (str "lume install knows no agent " (name agent)) {:lume/user-error true})))

(defmethod summarize :default [_ out]
  {:result (last (remove str/blank? (str/split-lines (str out))))})

(defmethod progress :default [_ prog line]
  (if (str/blank? line)
    prog
    (-> prog (update :steps (fnil inc 0)) (assoc :last line))))

;; Claude Code. Extra flags (permissions, model) come from LUME_CLAUDE_ARGS, an EDN
;; vector of strings so arguments with spaces survive: ["--model" "opus"].
;; The default lets workers edit files and run lume, so they can add notes and items.
;; Allowed commands are added either way, so workers can run the repo's feedback loop.

(defn- bash-rule [cmd] (str "Bash(" cmd ":*)"))

(defn- user-args [s]
  (let [v (try (edn/read-string s) (catch Exception _ nil))]
    (if (and (vector? v) (every? string? v))
      v
      (throw (ex-info (str "LUME_CLAUDE_ARGS must be an EDN vector of strings, e.g. [\"--model\" \"opus\"]; got: " s)
                      {:lume/user-error true})))))

(defn claude-args
  "Flags after the prompt: `env` (LUME_CLAUDE_ARGS) replaces the defaults, and
  the allowed commands are added either way."
  [env allow]
  (if env
    (cond-> (user-args env)
      (seq allow) (into ["--allowedTools" (str/join "," (map bash-rule allow))]))
    ["--permission-mode" "acceptEdits"
     "--allowedTools" (str/join "," (map bash-rule (into ["lume" "bin/lume"] allow)))]))

(defn claude-driver-args
  "A driver gets Bash and Read only, may run lume, jj and the allowed commands,
  and may never push. `env` (LUME_CLAUDE_ARGS) is kept, e.g. for --model."
  [env allow]
  (into (if env (user-args env) [])
        ["--tools" "Bash,Read"
         "--allowedTools" (str/join "," (concat (map bash-rule (into ["lume" "bin/lume" "jj"] allow)) ["Read"]))
         "--disallowedTools" (str/join "," (map bash-rule ["jj git push" "git push"]))]))

;; stream-json writes one event per line as it happens, so a running spawn can be followed.
;; --strict-mcp-config with no --mcp-config loads no MCP servers: the user's own
;; servers and claude.ai connectors (which can write to external services) stay out.
(defmethod command :claude [_ prompt {:keys [allow role]}]
  (let [env (System/getenv "LUME_CLAUDE_ARGS")]
    (into ["claude" "-p" prompt "--output-format" "stream-json" "--verbose" "--strict-mcp-config"]
          (if (= :driver role) (claude-driver-args env allow) (claude-args env allow)))))

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

;; Install: a user-level skill that teaches any Claude session to work through lume.
;; Tests bind *claude-home* to a temp dir; nothing else should touch the real one.

(def ^:dynamic *claude-home*
  (or (System/getenv "CLAUDE_CONFIG_DIR") (str (fs/path (fs/home) ".claude"))))

(def claude-skill
  "---
name: lume
description: Work in a repo through lume, the durable log for long agent tasks. Use when a repo has a lume session (`lume status` prints one), or when asked to plan, drive, or hand off work with lume.
---

# Working through lume

Lume keeps a long task's state in an event log, not in your context. `lume status`
is the handoff: there is no handoff file, and the next session starts from it too.

## Start

Run `lume status` first. It is one screen:
- goal: what the session is for
- asks: questions waiting for the human; don't redo work they block
- pinned notes: standing lessons; follow them
- open items: the backlog, each with the gate that decides done, and its latest
  measurement row if it has one (`lume measure <id>` runs it again)
- recent notes and events: what the last session did

`lume item show <id>` prints an item's full intent, gate and history when status clips it.
`lume ls` lists the repo's sessions, newest first; `lume status <session>` and
`lume log --session <session>` read a past one without touching it.
If there is no session, don't start one unprompted; ask the human for the goal.

## Work

- Add work as an item, intent first, then its gate:
  `lume item add \"<what and why>\" --gate \"<cmd>\"`.
  The gate is a command whose exit 0 means done (e.g. the repo's tests). An ungated
  item can't close itself when its worker exits.
- `lume spawn <id>` runs a worker on an item; `lume drive` runs driver turns, each
  making one decision, until an ask is open, a budget limit is reached, or nothing
  is left. `lume budget` shows spend; you may lower a limit, never raise one.
- `lume cadence` says when drive hands back: how many spawns a driver turn may
  request (`batch N`, run in order), whether a failed gate ends the batch
  (`on gate-fail`), and when a stalled measurement asks the human (`on plateau K`).
  You may hand back more often than the human's cadence, never less.
- `lume spec` prints the session's harness (goal, gate, allow, budget, cadence,
  items) as one page of EDN; `lume check <spec.edn>` validates a spec file, and
  `lume run <spec.edn>` starts a session from one, baselines it and drives
  (`--turns 0` doesn't drive). A spec with `:allow` or a cadence above
  every-spawn is the human's to run.
- After a worker: review its diff, commit it with a message saying what and why, then
  `lume note \"committed <id>: <change id> <what>\"`.
- `lume item close <id> done \"<why>\"` runs the gate and is refused if it fails;
  `dropped` needs only a reason.
- Record what you learn with `lume note \"<text>\"`; `lume note --pin` for anything the
  next session must not forget. When status lacked something you needed, add an item.

## The human

- When only the human can decide, `lume ask \"<question>\"` and stop that line of work.
- Never run `lume allow` or `lume answer`: permissions and answers are the human's,
  and lume refuses them from agents anyway: they need a terminal and a typed confirmation.
- Never push. Commit locally; pushing is up to the human.
- Lume writes nothing in the repo; its state lives under ~/.lume. Don't read or edit
  it directly; use the commands.
")

(defn- claude-actor-set? [settings]
  (try (= "claude" (get-in (json/parse-string (slurp settings)) ["env" "LUME_ACTOR"]))
       (catch Exception _ false)))

;; LUME_ACTOR=claude marks interactive sessions as agents, not the human (i25). It goes
;; in the user's own settings, which lume reads but never edits.
(defmethod setup :claude [_]
  (let [settings (str (fs/path *claude-home* "settings.json"))]
    {:files [[(str (fs/path *claude-home* "skills" "lume" "SKILL.md")) claude-skill]]
     :manual (if (claude-actor-set? settings)
               [(str "ok        LUME_ACTOR=claude is set in " settings)]
               [(str "todo      add to the \"env\" object in " settings ", so interactive Claude sessions act as 'claude', not the human:")
                "          \"LUME_ACTOR\": \"claude\""])}))
