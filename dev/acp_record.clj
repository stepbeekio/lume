;; Records one real ACP stdio transcript, as test data for the claude-acp adapter (i58, n10).
;;
;;   bb dev/acp_record.clj [out-file] [prompt]
;;
;; Runs the ACP agent (LUME_ACP_AGENT, a path or an EDN argv like ["devin" "acp"], else
;; ~/.npm-global/bin/claude-agent-acp) in a
;; fresh temp dir, sends initialize, session/new and one session/prompt that should
;; make it run a command and write a file (so it asks permission), allows each
;; permission once, and stops when the prompt's response arrives (or after 5 min).
;; Every JSON-RPC line is written to out-file (default test/data/acp-transcript.jsonl)
;; prefixed "> " (client to agent) or "< " (agent to client); the agent's stderr
;; goes to out-file.stderr. LUME_ACP_MODE, if set, is sent as session/set_mode first. It costs one short agent turn.
(require '[babashka.fs :as fs]
         '[babashka.process :as p]
         '[cheshire.core :as json]
         '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.string :as str])

(def agent-argv (let [s (System/getenv "LUME_ACP_AGENT")]
                  (cond (nil? s) [(str (fs/path (fs/home) ".npm-global" "bin" "claude-agent-acp"))]
                        (str/starts-with? (str/trim s) "[") (edn/read-string s)
                        :else [s])))
(def out-file (or (first *command-line-args*) "test/data/acp-transcript.jsonl"))
(def prompt (or (second *command-line-args*)
                "Run the shell command `echo hello`, then create a file hello.txt containing its output. Reply with one short sentence."))
(def work-dir (str (fs/create-temp-dir {:prefix "lume-acp-"})))
(fs/create-dirs (fs/parent (fs/absolutize out-file)))

(def proc (p/process agent-argv {:dir work-dir :err (io/file (str out-file ".stderr"))}))
(def in (io/writer (:in proc)))
(def out (io/writer out-file))
(def lock (Object.))

(defn record [prefix line]
  (locking lock (.write out (str prefix line "\n")) (.flush out)))

(defn send! [msg]
  (let [line (json/generate-string msg)]
    (record "> " line)
    (.write in (str line "\n"))
    (.flush in)))

(def responses (atom {}))
(def done (promise))

(defn handle [{:strs [id method params] :as msg}]
  (cond
    (and id method (= method "session/request_permission"))
    (let [opt (or (first (filter #(= "allow_once" (get % "kind")) (get params "options")))
                  (first (get params "options")))]
      (send! {:jsonrpc "2.0" :id id
              :result {:outcome {:outcome "selected" :optionId (get opt "optionId")}}}))
    (and id method)
    (send! {:jsonrpc "2.0" :id id :error {:code -32601 :message (str "lume recorder: no " method)}})
    id (do (swap! responses assoc id msg)
           (when (= 2 id) (deliver done msg)))))

(future
  (doseq [line (line-seq (io/reader (:out proc)))]
    ;; _auth/status_update carries the account's email; keep it out of the repo.
    (when-not (str/includes? line "\"method\":\"_auth/")
      (record "< " line))
    (when-let [msg (try (json/parse-string line) (catch Exception _ nil))]
      (handle msg)))
  (deliver done :eof))

(defn await-response [id]
  (loop [n 0]
    (or (get @responses id)
        (when (and (< n 600) (not (realized? done)))
          (Thread/sleep 100)
          (recur (inc n))))))

(send! {:jsonrpc "2.0" :id 0 :method "initialize"
        :params {:protocolVersion 1
                 :clientCapabilities {:fs {:readTextFile false :writeTextFile false} :terminal false}}})
(await-response 0)
(send! {:jsonrpc "2.0" :id 1 :method "session/new" :params {:cwd work-dir :mcpServers []}})
(let [sid (get-in (await-response 1) ["result" "sessionId"])]
  (when-not sid
    (println "no sessionId from session/new; see" out-file)
    (p/destroy-tree proc)
    (System/exit 1))
  (when-let [mode (System/getenv "LUME_ACP_MODE")]
    (send! {:jsonrpc "2.0" :id 3 :method "session/set_mode" :params {:sessionId sid :modeId mode}})
    (await-response 3))
  (send! {:jsonrpc "2.0" :id 2 :method "session/prompt"
          :params {:sessionId sid
                   :prompt [{:type "text" :text prompt}]}}))
(println "result:" (pr-str (deref done 300000 :timeout)))
(p/destroy-tree proc)
(.close out)
(println "transcript in" out-file)
(shutdown-agents)
(System/exit 0)
