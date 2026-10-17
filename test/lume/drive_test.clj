(ns lume.drive-test
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [lume.agents :as agents]
            [lume.ask :as ask]
            [lume.drive :as drive]
            [lume.features :as features]
            [lume.item :as item]
            [lume.kernel :as kernel]
            [lume.ntfy :as ntfy]
            [lume.session :as session]
            [lume.status :as status]
            [lume.store :as store]))

(def ^:dynamic *root* nil)

;; The fake driver's scripts and calls, per test so tests can run in parallel.
(def ^:private ^:dynamic *driver-turns* nil)
(def ^:private ^:dynamic *driver-calls* nil)

(use-fixtures :each
  (fn [t]
    (let [home (str (fs/create-temp-dir {:prefix "lume-home"}))
          root (str (fs/create-temp-dir {:prefix "lume-repo"}))]
      (try
        (binding [store/*home* home store/*session* nil kernel/*actor* "human" kernel/*spawned-as* nil *root* root
                  kernel/*features* features/all
                  kernel/*confirm* (constantly true) kernel/*read-line* (constantly nil)
                  drive/*source* nil ntfy/*ntfy* nil drive/*reexec* (fn [& _] (throw (ex-info "no exec in tests" {})))
                  *driver-turns* (atom []) *driver-calls* (atom [])]
          (t))
        (finally (fs/delete-tree home) (fs/delete-tree root))))))

(defn- new-session [goal]
  (session/init *root* [goal])
  (store/current-session *root*))

(defn- lume-cmd [& args]
  (str/join " " (concat ["bb" "-cp" (str (fs/absolutize "src")) "-m" "lume.cli"]
                        (map #(str "'" % "'") args))))

;; A worker that reports who it ran as and fails, and a fake driver: each turn
;; runs the next shell script, which can call lume.
(defmethod agents/command :drive-worker [_ _prompt _opts]
  ["sh" "-c" "printf 'working\\ndone by %s\\n' \"$LUME_ACTOR\"; exit 3"])

(defmethod agents/command :drive-driver [_ prompt opts]
  (swap! *driver-calls* conj (assoc opts :prompt prompt))
  (let [script (or (first @*driver-turns*) "true")]
    (swap! *driver-turns* #(vec (rest %)))
    ["sh" "-c" script]))

(defmethod agents/summarize :drive-driver [_ out] (agents/summarize :claude out))

(defn- drive! [s & turns]
  (reset! *driver-turns* (vec turns))
  (reset! *driver-calls* [])
  (with-out-str (is (string? (drive/drive s *root* {:agent "drive-driver" :turns 3}))))
  (last (keep :why (store/read-events s))))

(deftest drive-runs-driver-turns
  (testing "a driver's spawn is a request, run after its turn"
    (let [s (new-session "goal")]
      (item/item-add s ["do it" "--gate" "true"])
      ;; LUME_ACTOR=claude is what Claude Code's settings.json env does to the
      ;; driver's commands (i46): the drive's id must still win.
      (is (= "no open items"
             (drive! s (str "LUME_ACTOR=claude " (lume-cmd "spawn" "i1" "--agent" "drive-worker") "; echo \"me $LUME_ACTOR\""))))
      (let [events (store/read-events s)
            {:keys [items spawns]} (status/project events)]
        (is (= [:drive/started :drive/turn :spawn/requested :drive/exited :spawn/started]
               (take 5 (drop 2 (map :type events)))))
        (is (= "d1" (:by (first (filter #(= :spawn/requested (:type %)) events)))))
        (is (= "done by s1" (get-in spawns ["s1" :result])))
        (is (= :done (get-in items ["i1" :state])) "the spawn's gate closed it"))
      (let [[{:keys [prompt role]} closing] @*driver-calls*]
        (is (= :driver role))
        (is (str/includes? prompt "turn d1"))
        (is (str/includes? prompt "open    i1  do it") "the prompt carries status")
        (is (not (str/includes? prompt "closing")))
        (is (str/includes? (:prompt closing) "s1 exited after the last driver turn, so this is the closing")
            "the spawn closed the last item, so a closing turn reviews its diff, though it logged nothing")
        (is (= 2 (count @*driver-calls*))))))
  (testing "with no turn left for the closing turn, the stop line says the diff is uncommitted"
    (let [s (new-session "goal")]
      (item/item-add s ["do it" "--gate" "true"])
      (reset! *driver-turns* [(lume-cmd "spawn" "i1" "--agent" "drive-worker")])
      (with-out-str (drive/drive s *root* {:agent "drive-driver" :turns 1}))
      (is (= "no open items, and no turn left to review s1's changes: they're uncommitted"
             (last (keep :why (store/read-events s)))))))
  (testing "a request that names no agent runs the drive's"
    (let [s (new-session "goal")]
      (item/item-add s ["do it" "--gate" "true"])
      (drive! s (lume-cmd "spawn" "i1") "echo worked")
      (is (= :drive-driver (:agent (first (filter #(= :spawn/started (:type %)) (store/read-events s))))))
      (is (= [:driver nil] (take 2 (map :role @*driver-calls*))) "the worker ran the drive's agent, as a worker")))
  (testing "it stops when a turn logs nothing, when turns run out, and when an ask is open"
    (let [s (new-session "goal")]
      (item/item-add s ["never done"])
      (is (= "d1 logged no events" (drive! s "true")))
      (is (= "3 turns run" (drive! s (lume-cmd "note" "a") (lume-cmd "note" "b") (lume-cmd "note" "c"))))
      (is (= ["d2" "d3" "d4"] (map :by (:notes (status/project (store/read-events s)))))
          "driver ids keep counting across drives")
      (is (re-find #"^ask a1 is open: which\? " (drive! s (lume-cmd "ask" "which?") (lume-cmd "note" "never"))))
      (is (= 1 (count @*driver-calls*)) "no turn runs while an ask is open"))))

;; i49: a drive started before a commit ran the old code for every later turn.
(deftest drive-reloads-changed-source
  (let [src (str (fs/path *root* "lume-src"))
        _ (fs/create-dirs (fs/path src "lume"))
        cli-file (str (fs/path src "lume" "cli.clj"))
        write-cli (fn [code] (str "printf '" code "' > " cli-file))
        execs (atom [])
        drive (fn [& [opts]]
                (with-out-str (drive/drive (store/current-session *root*) *root*
                                           (merge {:agent "drive-driver" :turns 3} opts))))
        s (new-session "goal")]
    (spit cli-file "(ns lume.cli)")
    (item/item-add s ["never done"])
    (binding [drive/*source* src
              drive/*reexec* (fn [src args] (swap! execs conj [src args]) "re-exec'd")]
      (testing "when the source changes under it and loads, the drive re-execs on it from the next turn"
        (reset! *driver-turns* [(str (write-cli "(ns lume.cli) ;; new") "; " (lume-cmd "note" "a"))])
        (drive)
        (is (= [[src ["drive" "--turns" "3" "--agent" "drive-driver" "--from" "2"]]] @execs))
        (is (= [:drive/turn :note/added :drive/exited :drive/reloaded] (map :type (take-last 4 (store/read-events s)))))
        (is (= 1 (count @*driver-calls*)) "turn 2 runs in the new process"))
      (testing "the new process carries on from that turn, without starting another drive"
        (reset! *driver-turns* ["true"])
        (drive {:from 2})
        (let [events (store/read-events s)]
          (is (= 1 (count (filter #(= :drive/started (:type %)) events))))
          (is (= 2 (:n (last (filter #(= :drive/turn (:type %)) events)))))))
      (testing "when the new source doesn't load, the drive notes it and keeps running the old code"
        (reset! execs [])
        (reset! *driver-turns* [(str (write-cli "(ns lume.cli") "; " (lume-cmd "note" "b")) (lume-cmd "note" "c") "true"])
        (drive)
        (is (empty? @execs))
        (is (= 3 (count (filter #(= :drive/turn (:type %)) (take-while #(not= :drive/started (:type %)) (reverse (store/read-events s)))))))
        (is (some #(str/includes? % "doesn't load, so turn 2 runs the code the drive started with")
                  (map :text (:notes (status/project (store/read-events s))))))))))

(deftest next-step-decides-from-the-log
  (let [s (new-session "goal")
        step (fn [n opts changed]
               (let [events (store/read-events s)]
                 (#'drive/next-step (kernel/project events) events n (merge {:turns 3} opts) changed)))]
    (is (= [:stop "no open items"] (step 1 {} false)))
    (item/item-add s ["do it"])
    (is (= [:turn nil] (step 1 {} false)))
    (is (= [:reload] (step 2 {} true)))
    (is (= [:stop "3 turns run"] (step 4 {} true)) "a stop wins over a reload")
    (with-out-str (ask/ask s ["which?"]))
    (is (= [:wait] (step 1 {:wait true} false)))
    (is (re-find #"^ask a1 is open: which\?" (second (step 1 {} false))))))
