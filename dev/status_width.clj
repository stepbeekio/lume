;; bb dev/status_width.clj <session> [--max N]
;;
;; The widest line lume status has shown at any decision in a session's
;; history, replayed from its log: prints width=W over=K, where K counts lines
;; wider than status's 100 columns. With --max N, exit 1 when W is over N, so
;; it can gate. The forms are the ones tried in lume repl (i55), evaluated in
;; the namespace lume repl sets up.
(require '[lume.cli :as cli] '[lume.store :as store])

(let [[id & {:strs [--max]}] *command-line-args*
      session (or (some->> id (store/find-session (store/repo-root)))
                  (do (binding [*out* *err*] (println "usage: bb dev/status_width.clj <session> [--max N]"))
                      (System/exit 2)))]
  (cli/repl-ns! 'width session)
  (let [{:keys [width over]}
        (binding [*ns* (the-ns 'width)]
          (eval '(let [ds (keep-indexed (fn [i e] (when (status/decision? e) (inc i))) events)
                       lines (mapcat #(str/split-lines (render %)) ds)]
                   {:width (apply max 0 (map count lines))
                    :over (count (filter #(> (count %) 100) lines))})))]
    (println (str "width=" width " over=" over))
    (when (and --max (> width (parse-long --max)))
      (System/exit 1))))
