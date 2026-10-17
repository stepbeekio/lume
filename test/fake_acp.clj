;; A fake ACP agent that replays a recorded transcript (test/data/acp-transcript.jsonl).
;;
;;   bb test/fake_acp.clj <transcript> <log>
;;
;; Lines the agent sent ("< ") are written to stdout in order; at each line the
;; client sent ("> "), it reads one line from stdin instead and appends it to log,
;; so a test can check what the client said (e.g. its permission answers).
(require '[clojure.java.io :as io]
         '[clojure.string :as str])

(let [[transcript log] *command-line-args*
      in (io/reader System/in)]
  (doseq [line (str/split-lines (slurp transcript))]
    (cond
      (str/starts-with? line "< ") (do (println (subs line 2)) (flush))
      (str/starts-with? line "> ") (when-let [sent (.readLine in)]
                                     (spit log (str sent "\n") :append true)))))
