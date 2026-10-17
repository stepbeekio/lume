(ns lume.agents
  "Adapters: the only place that knows which agent CLI is in play.

  An adapter turns a prompt into a command line, the command's output lines
  into progress while it runs, and its whole output into a short summary for
  the log."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [lume.kernel :as kernel]
            [lume.store :as store]))

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
  "What `lume install` does for `agent` on this machine, given the text of lume's
  skill: {:files [[path content]]
  :manual [\"line\"]}. Files are written by install (only when they differ);
  manual lines are printed, never applied, because they change the user's own
  agent settings."
  (fn [agent _skill] agent))

(defmethod setup :default [agent _skill]
  (throw (ex-info (str "lume install knows no agent " (name agent)) {:lume/user-error true})))

(defmethod summarize :default [_ out]
  {:result (kernel/tail-line out)})

(defmethod progress :default [_ prog line]
  (if (str/blank? line)
    prog
    (-> prog (update :steps (fnil inc 0)) (assoc :last line))))

;; Claude Code. Extra flags (permissions, model) come from LUME_CLAUDE_ARGS, an EDN
;; vector of strings so arguments with spaces survive: ["--model" "opus"].
;; The default lets workers edit files and run lume, so they can add notes and items.
;; Allowed commands are added either way, so workers can run the repo's feedback loop.

(defn- bash-rule [cmd] (str "Bash(" cmd ":*)"))

(defn- user-args
  ([s] (user-args "LUME_CLAUDE_ARGS" s))
  ([var s]
   (let [v (try (edn/read-string s) (catch Exception _ nil))]
     (if (and (vector? v) (every? string? v))
       v
       (throw (ex-info (str var " must be an EDN vector of strings, e.g. [\"--model\" \"opus\"]; got: " s)
                       {:lume/user-error true}))))))

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

;; Claude Code over ACP (Agent Client Protocol, JSON-RPC on stdio), through
;; @agentclientprotocol/claude-agent-acp. Message shapes are checked against a
;; real transcript, test/data/acp-transcript.jsonl (dev/acp_record.clj).
;;
;; The spawn's child is a bridge, `bb -m lume.agents acp <opts> <prompt>`, that
;; runs the ACP agent, prints one JSON line per session/update (so progress and
;; summarize stay line-based), answers each permission request from the allow
;; list, logging it as a :spawn/permission event, and ends with a result line.
;; Every ACP agent shares the bridge, its summary and its progress; only argv differs.

(derive :claude-acp ::acp)
(derive :devin ::acp)

(def ^:dynamic *acp-agent*
  "argv of the ACP agent; nil finds it. Tests bind a fake."
  nil)

(defn acp-agent-argv []
  (or *acp-agent*
      (when-let [bin (not-empty (System/getenv "LUME_ACP_AGENT"))] [bin])
      (when-let [bin (fs/which "claude-agent-acp")] [(str bin)])
      ;; npm -g with prefix ~/.npm-global, which isn't on a spawn's PATH (n127).
      (let [bin (fs/path (fs/home) ".npm-global" "bin" "claude-agent-acp")]
        (when (fs/executable? bin) [(str bin)]))
      (throw (ex-info "no ACP agent: install @agentclientprotocol/claude-agent-acp (npm -g), or set LUME_ACP_AGENT to its path"
                      {:lume/user-error true}))))

(defn- lume-src []
  (str (fs/parent (fs/parent (io/file (io/resource "lume/agents.clj"))))))

(defmethod command :claude-acp [_ prompt {:keys [allow role]}]
  ["bb" "-cp" (lume-src) "-m" "lume.agents" "acp"
   (pr-str {:agent (acp-agent-argv) :allow (vec allow) :role role}) prompt])

(defn- mask-quoted
  "`cmd` with every quoted or backslash-escaped character replaced by _, so
  only what the shell would interpret is left; nil when a quote is unbalanced
  or a double-quoted string substitutes with $ or a backtick."
  [cmd]
  (loop [[c & more] cmd, quote nil, out (StringBuilder.)]
    (cond
      (nil? c) (when-not quote (str out))
      (#{\newline \return} c) nil
      (= quote \') (if (= c \')
                     (recur more nil (.append out c))
                     (recur more quote (.append out \_)))
      (= c \\) (if (seq more)
                 (recur (rest more) quote (.append out "__"))
                 nil)
      (= quote \") (cond
                     (= c \") (recur more nil (.append out c))
                     (#{\$ \`} c) nil
                     :else (recur more quote (.append out \_)))
      (#{\' \"} c) (recur more c (.append out c))
      :else (recur more nil (.append out c)))))

(defn- allowed-command?
  "Whether shell `cmd` is only allowed commands: exactly an allowed one (a gate
  like `test -f x && grep -q y x`), or each part of a pipeline or list (split
  on && || ; | and a lone &) starts with an allowed prefix (or is head, tail,
  grep or wc), and nothing substitutes, redirects (bar 2>&1) or opens a
  subshell. Quoted text is an argument, so its metachars don't count."
  [prefixes cmd]
  (boolean
   (or (some #{(str/trim cmd)} prefixes)
       (when-let [cmd (some-> (mask-quoted (str/trim cmd)) (str/replace "2>&1" ""))]
         (let [parts (map str/trim (str/split cmd #"&&|\|\||;|\||&"))
               ok? (fn [part] (some #(or (= part %) (str/starts-with? part (str % " ")))
                                    (concat prefixes ["head" "tail" "grep" "wc"])))]
           (and (not (re-find #"[`$<>()]" cmd))
                (every? ok? parts)))))))

(defn acp-permit?
  "Whether to allow a session/request_permission. Workers may edit and run the
  allowed commands, plus lume; drivers may run lume, jj and the allowed commands
  but never edit or push. Both may read and search."
  [{:keys [allow role]} {:strs [toolCall]}]
  (let [{:strs [kind rawInput]} toolCall
        cmd (get rawInput "command")
        driver (= :driver role)]
    (case kind
      "execute" (and (string? cmd)
                     (allowed-command? (concat ["lume" "bin/lume"] (when driver ["jj"]) allow) cmd)
                     (not (and driver (re-find #"\bgit\s+push\b" cmd))))
      ("edit" "delete" "move") (not driver)
      ("read" "search") true
      false)))

(defn- acp-answer [permit? {:strs [options]}]
  (if-let [opt (first (filter #(= (if permit? "allow_once" "reject_once") (get % "kind")) options))]
    {:outcome "selected" :optionId (get opt "optionId")}
    {:outcome "cancelled"}))

;; What a permission is for: the command, or the tool and its file.
(defn- acp-subject [{:strs [name title rawInput]}]
  (or (get rawInput "command")
      (str/join " " (remove nil? [name (or (get rawInput "file_path") title)]))))

;; The bridge is a pure step over its state, (acp-step state msg) -> [state effects],
;; and a loop that does the IO. Effects run in order: [:send m] writes a JSON-RPC
;; message to the agent, [:emit m] prints a line, [:permission e] logs a
;; :spawn/permission event, [:exit code] ends the bridge.
;;
;; State: :opts and :prompt and :cwd as given; :sid, the agent's session;
;; :cost, its latest USD cost; :text, the message being collected
;; ({:id :parts}: text comes in chunks and each message is printed whole, once
;; it ends); :calls, tool calls by id (a permission request's toolCall may be
;; just its id, as Devin's are, so it's read over the tool_call that announced it).

;; The handshake: lume's requests in order, each sent when the one before
;; replies. A request's id is its place here.
(def ^:private acp-handshake ["initialize" "session/new" "session/prompt"])

(defn- acp-request [method params]
  {:jsonrpc "2.0" :id (.indexOf acp-handshake method) :method method :params params})

(defn- acp-open
  "The bridge's first state and effects: initialize."
  [opts prompt cwd]
  [{:opts opts :prompt prompt :cwd cwd :calls {}}
   [[:send (acp-request "initialize"
                        {:protocolVersion 1
                         :clientCapabilities {:fs {:readTextFile false :writeTextFile false} :terminal false}})]]])

(defn- text-effects
  "Effects printing the message being collected, if there is one."
  [{:keys [text]}]
  (if-let [{:keys [id parts]} text]
    [[:emit {:sessionUpdate "agent_message_chunk" :messageId id
             :content {:type "text" :text (apply str parts)}}]]
    []))

(defn- acp-finish [{:keys [sid cost text] :as state} m code]
  [state (conj (text-effects state)
               [:emit (cond-> (assoc m :type "result" :sessionId sid :text (some->> (:parts text) (apply str)))
                        cost (assoc :costUsd cost))]
               [:exit code])])

(defn- acp-permission [{:keys [opts calls] :as state} {:strs [id params]}]
  (let [call (merge (get calls (get-in params ["toolCall" "toolCallId"])) (get params "toolCall"))
        params (assoc params "toolCall" call)
        permit? (acp-permit? opts params)
        tool (or (get call "name") (get call "kind"))
        spawn (:spawn opts)]
    [state [[:send {:jsonrpc "2.0" :id id :result {:outcome (acp-answer permit? params)}}]
            [:emit {:type "permission" :tool tool :kind (get call "kind")
                    :subject (acp-subject call) :allowed permit?}]
            [:permission {:type :spawn/permission :spawn/id spawn :tool tool :command (acp-subject call)
                          :disposition (if permit? :allow :reject) :by spawn}]]]))

(def ^:private quiet-updates #{"available_commands_update" "usage_update" "agent_thought_chunk"})

(defn- acp-update [state {:strs [sessionUpdate messageId content toolCallId] :as u}]
  (let [usd (when (= "USD" (get-in u ["cost" "currency"])) (get-in u ["cost" "amount"]))
        state (cond-> (update state :cost #(or usd %))
                (#{"tool_call" "tool_call_update"} sessionUpdate) (update-in [:calls toolCallId] merge u))
        text (:text state)]
    (cond
      ;; A tool call ends the message before it, so the result is the text
      ;; after the last call even when chunks have no messageId.
      (= "tool_call" sessionUpdate) [(assoc state :text nil) (conj (text-effects state) [:emit u])]

      ;; Devin's chunks have no messageId, so nil is an id too.
      (= "agent_message_chunk" sessionUpdate)
      (let [new? (or (nil? text) (not= messageId (:id text)))]
        [(cond-> state
           new? (assoc :text {:id messageId :parts []})
           :always (update-in [:text :parts] conj (get content "text")))
         (if new? (text-effects state) [])])

      (quiet-updates sessionUpdate) [state []]
      :else [state [[:emit u]]])))

(defn- acp-reply
  "Step on the agent's reply to a handshake request: send the next one, or finish."
  [{:keys [cwd prompt] :as state} {:strs [id result]}]
  (case (get acp-handshake id)
    "initialize" [state [[:send (acp-request "session/new" {:cwd cwd :mcpServers []})]]]
    "session/new" (let [sid (get result "sessionId")]
                    [(assoc state :sid sid)
                     [[:send (acp-request "session/prompt" {:sessionId sid :prompt [{:type "text" :text prompt}]})]]])
    "session/prompt" (acp-finish state {:stopReason (get result "stopReason")} 0)
    [state []]))

(defn acp-step
  "The bridge's next [state effects] on `msg`, a parsed line from the agent;
  nil means its output ended."
  [state {:strs [id method params error] :as msg}]
  (cond
    (nil? msg) (acp-finish state {:error "the ACP agent exited before the prompt finished"} 1)
    (= "session/request_permission" method) (acp-permission state msg)
    (and id method) [state [[:send {:jsonrpc "2.0" :id id :error {:code -32601 :message (str "lume: no " method)}}]]]
    (= "session/update" method) (acp-update state (get params "update"))
    error (acp-finish state {:error (get error "message")} 1)
    :else (acp-reply state msg)))

(defn- read-acp-line
  "A line from the agent as a message: nil at the end, {} if it isn't JSON."
  [line]
  (when line (or (try (json/parse-string line) (catch Exception _ nil)) {})))

(defn- emit-line! [m] (println (json/generate-string m)) (flush))

(defn- acp-effects!
  "Carry out `effects`; returns the exit code if they end the bridge."
  [w session effects]
  (doseq [[kind m] effects]
    (case kind
      :send (do (.write w (str (json/generate-string m) "\n")) (.flush w))
      :emit (emit-line! m)
      :permission (when session (kernel/append! session (constantly m)))
      :exit nil))
  (some (fn [[kind code]] (when (= :exit kind) code)) effects))

(defn acp-bridge
  "Run the ACP agent on `prompt` in the current directory, printing lines to
  *out*. `opts` has :agent (argv), :allow and :role, and :session and :spawn
  to log permission decisions to. Returns the exit code."
  [{:keys [agent session] :as opts} prompt]
  (let [proc (p/process agent {:err :inherit})
        w (io/writer (:in proc))]
    (try
      ;; Never read ahead a line the agent won't send until we answer this one:
      ;; lazy-seq, since line-seq reads its first line at once (before
      ;; initialize is sent), and first and rest, not [line & more].
      (loop [lines (lazy-seq (line-seq (io/reader (:out proc))))
             [state effects] (acp-open opts prompt (str (fs/cwd)))]
        (or (acp-effects! w session effects)
            (recur (rest lines) (acp-step state (read-acp-line (first lines))))))
      (catch java.io.IOException e
        (emit-line! {:type "result" :error (str "the ACP agent's pipe closed: " (ex-message e))})
        1)
      (finally (p/destroy-tree proc)))))

(defn- acp-result [out]
  (->> (str/split-lines (str out)) (keep parse) (filter #(= "result" (get % "type"))) last))

(defmethod summarize ::acp [_ out]
  (if-let [{:strs [text sessionId costUsd stopReason error]} (acp-result out)]
    (cond-> {:result (or error text) :agent-session sessionId}
      costUsd (assoc :cost-usd costUsd)
      (not= "end_turn" stopReason) (assoc :agent-error true))
    (summarize :default out)))

;; Steps are tool calls; :last is the latest call's title, or text when the agent is talking.
(defmethod progress ::acp [_ prog line]
  (let [{:strs [sessionUpdate title content]} (parse line)]
    (case sessionUpdate
      "tool_call" (cond-> (update prog :steps (fnil inc 0)) title (assoc :last title))
      "tool_call_update" (cond-> prog title (assoc :last title))
      "agent_message_chunk" (let [t (get content "text")]
                              (cond-> prog (not (str/blank? t)) (assoc :last t)))
      prog)))

;; Devin CLI over ACP, `devin acp`. Its ACP sessions start in accept-edits mode, which
;; edits the workspace and runs commands it deems read-only without asking, and no
;; mode asks for everything: --permission-mode doesn't reach acp, and ask mode can't
;; run commands. So devin runs with a config of lume's own (--config replaces the
;; user's ~/.config/devin/config.json) whose ask rules send every command and write
;; through the bridge. Checked against test/data/devin-acp-transcript.jsonl. Devin
;; reports tokens, not dollars, so its spawns add nothing to the budget's $.
;; Flags for `devin acp` (e.g. --model) come from LUME_DEVIN_ARGS, an EDN vector.

(def devin-config {:version 1 :permissions {:ask ["exec" "edit" "Write(/**)"]}})

(defn- devin-config-path [] (fs/path store/*home* "agents" "devin.json"))

(defn- devin-config-json [] (json/generate-string devin-config {:pretty true}))

(defn- devin-config-file
  "Path of lume's devin config under LUME_HOME, written when missing or stale."
  []
  (let [f (devin-config-path)
        s (devin-config-json)]
    (when-not (and (fs/exists? f) (= s (slurp (fs/file f))))
      (fs/create-dirs (fs/parent f))
      (let [tmp (fs/create-temp-file {:dir (fs/parent f) :prefix "devin." :suffix ".tmp"})]
        (spit (fs/file tmp) s)
        (fs/move tmp f {:replace-existing true :atomic-move true})))
    (str f)))

(defn devin-agent-argv []
  (or *acp-agent*
      (if-let [bin (fs/which "devin")]
        (into [(str bin) "--config" (devin-config-file) "acp"]
              (some->> (not-empty (System/getenv "LUME_DEVIN_ARGS")) (user-args "LUME_DEVIN_ARGS")))
        (throw (ex-info "no devin on PATH: install Devin CLI, then devin auth login"
                        {:lume/user-error true})))))

(defmethod command :devin [_ prompt {:keys [allow role]}]
  ["bb" "-cp" (lume-src) "-m" "lume.agents" "acp"
   (pr-str {:agent (devin-agent-argv) :allow (vec allow) :role role}) prompt])

(defn -main [& [cmd opts prompt]]
  (case cmd
    "acp" (System/exit (binding [kernel/*features* @(requiring-resolve 'lume.features/all)]
                         (acp-bridge (assoc (edn/read-string opts)
                                            :session (not-empty (System/getenv "LUME_SESSION"))
                                            :spawn (System/getenv "LUME_SPAWNED_AS"))
                                     prompt)))
    (binding [*out* *err*]
      (println "usage: bb -m lume.agents acp <opts-edn> <prompt>")
      (System/exit 2))))

;; Install: where each agent keeps lume's skill (lume.install has its text) and what
;; else it needs. Tests bind *claude-home* and *devin-home* to temp dirs; nothing else should touch the real ones.

(def ^:dynamic *claude-home*
  (or (System/getenv "CLAUDE_CONFIG_DIR") (str (fs/path (fs/home) ".claude"))))

(defn- claude-actor-set? [settings]
  (try (= "claude" (get-in (json/parse-string (slurp settings)) ["env" "LUME_ACTOR"]))
       (catch Exception _ false)))

;; LUME_ACTOR=claude marks interactive sessions as agents, not the human (i25). It goes
;; in the user's own settings, which lume reads but never edits.
(defmethod setup :claude [_ skill]
  (let [settings (str (fs/path *claude-home* "settings.json"))]
    {:files [[(str (fs/path *claude-home* "skills" "lume" "SKILL.md")) skill]]
     :manual (if (claude-actor-set? settings)
               [(str "ok        LUME_ACTOR=claude is set in " settings)]
               [(str "todo      add to the \"env\" object in " settings ", so interactive Claude sessions act as 'claude', not the human:")
                "          \"LUME_ACTOR\": \"claude\""])}))

(def ^:dynamic *devin-home*
  (str (fs/path (or (System/getenv "XDG_CONFIG_HOME") (fs/path (fs/home) ".config")) "devin")))

;; Devin has no settings env, so the actor and the default agent go in the shell
;; rc; the alias reaches only interactive sessions, since spawns run devin by path.
(defmethod setup :devin [_ skill]
  (let [bin (fs/which "devin")]
    {:files [[(str (fs/path *devin-home* "skills" "lume" "SKILL.md")) skill]
             [(str (devin-config-path)) (devin-config-json)]]
     :manual (concat
              [(if bin
                 (str "ok        devin is " bin "; if devin auth status isn't logged in, run devin auth login")
                 "todo      install Devin CLI so devin is on PATH, then run devin auth login")]
              (if (= "devin" (System/getenv "LUME_AGENT"))
                ["ok        LUME_AGENT=devin is set"]
                ["todo      add to your shell rc, so lume runs devin by default and interactive devin"
                 "          sessions act as 'devin', not the human:"
                 "          export LUME_AGENT=devin"
                 "          alias devin='LUME_ACTOR=devin devin'"]))}))
