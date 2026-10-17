(ns lume.ntfy-test
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [lume.agent :as agent]
            [lume.agents :as agents]
            [lume.ask :as ask]
            [lume.drive :as drive]
            [lume.features :as features]
            [lume.item :as item]
            [lume.kernel :as kernel]
            [lume.ntfy :as ntfy]
            [lume.session :as session]
            [lume.store :as store]
            [org.httpkit.server :as httpkit]))

(def ^:dynamic *root* nil)

;; The fake driver's scripts, per test so tests can run in parallel.
(def ^:private ^:dynamic *driver-turns* nil)

(use-fixtures :each
  (fn [t]
    (let [home (str (fs/create-temp-dir {:prefix "lume-home"}))
          root (str (fs/create-temp-dir {:prefix "lume-repo"}))]
      (try
        (binding [store/*home* home store/*session* nil kernel/*actor* "human" kernel/*spawned-as* nil *root* root
                  kernel/*features* features/all
                  kernel/*confirm* (constantly true) kernel/*read-line* (constantly nil)
                  drive/*source* nil ntfy/*ntfy* nil drive/*reexec* (fn [& _] (throw (ex-info "no exec in tests" {})))
                  *driver-turns* (atom [])]
          (t))
        (finally (fs/delete-tree home) (fs/delete-tree root))))))

(defn- new-session [goal]
  (session/init *root* [goal])
  (store/current-session *root*))

(defn- lume-cmd [& args]
  (str/join " " (concat ["bb" "-cp" (str (fs/absolutize "src")) "-m" "lume.cli"]
                        (map #(str "'" % "'") args))))

;; A fake driver: each turn runs the next shell script, which can call lume.
(defmethod agents/command :ntfy-driver [_ _prompt _opts]
  (let [script (or (first @*driver-turns*) "true")]
    (swap! *driver-turns* #(vec (rest %)))
    ["sh" "-c" script]))

(defmethod agents/summarize :ntfy-driver [_ out] (agents/summarize :claude out))

(defn- drive! [s & turns]
  (reset! *driver-turns* (vec turns))
  (with-out-str (is (string? (drive/drive s *root* {:agent "ntfy-driver" :turns 3}))))
  (last (keep :why (store/read-events s))))

(defmethod agents/command :ntfy-asker [_ _prompt _opts]
  ["sh" "-c" (lume-cmd "ask" "from the worker?")])

(deftest hand-backs-notify-the-human
  (let [got (atom [])
        status (atom 200)
        server (httpkit/run-server (fn [r]
                                     (swap! got conj [(get-in r [:headers "title"]) (slurp (:body r))])
                                     {:status @status})
                                   {:port 0 :legacy-return-value? false})
        url (str "http://localhost:" (httpkit/server-port server) "/secret-topic")
        notices (fn [s] (filter #(#{:notify/sent :notify/failed} (:type %)) (store/read-events s)))]
    (try
      (let [s (new-session "goal")
            id (item/item-add s ["ask from a worker"])]
        (testing "unset LUME_NTFY sends and logs nothing"
          (ask/ask s ["quiet?"])
          (is (empty? @got))
          (is (empty? (notices s))))
        (binding [ntfy/*ntfy* url]
          (testing "an ask is sent and logged, without the URL"
            (is (= "a2" (ask/ask s ["which db?"])))
            (is (= [["lume: ask a2 from human" (str "which db?\nlume answer a2\n" (store/session-id s))]] @got))
            (is (= {:type :notify/sent :on :ask/opened :ask/id "a2" :text "which db?\nlume answer a2" :by "human"}
                   (select-keys (last (notices s)) [:type :on :ask/id :item/id :text :by])))
            (is (not (str/includes? (slurp (str (fs/path s "log.edn"))) "secret-topic"))))
          (testing "a worker's ask names its item: lume sends it, since LUME_NTFY doesn't reach the spawn"
            (agent/spawn s *root* [id "--agent" "ntfy-asker"])
            (is (= "lume: ask a3 on i1 from s1" (first (last @got))))
            (is (= {:ask/id "a3" :item/id "i1" :by "human"} (select-keys (last (notices s)) [:ask/id :item/id :by]))))
          (testing "a failed send is logged and never fails the command"
            (reset! status 500)
            (is (= "a4" (ask/ask s ["still there?"])))
            (is (= {:type :notify/failed :ask/id "a4" :error "HTTP 500"}
                   (select-keys (last (notices s)) [:type :ask/id :error])))
            (binding [ntfy/*ntfy* "http://localhost:1/nothing-listens"]
              (is (= "a5" (ask/ask s ["anyone?"])))
              (is (= :notify/failed (:type (last (notices s)))))))))
      (testing "a drive's stop is sent"
        (reset! status 200)
        (let [s (new-session "goal")]
          (item/item-add s ["do it" "--gate" "false"])
          (binding [ntfy/*ntfy* url]
            (drive! s (lume-cmd "note" "looked")))
          (is (re-find #"^lume: drive stopped$" (first (last @got))))
          (is (= {:type :notify/sent :on :drive/stopped :text "d2 logged no events"}
                 (select-keys (last (notices s)) [:type :on :text])))))
      (finally (httpkit/server-stop! server)))))

(defmethod agents/command :ntfy-peek [_ _prompt _opts]
  ["sh" "-c" "echo \"ntfy=[$LUME_NTFY$LUME_NTFY_REPLY$LUME_NTFY_OTHER]\""])

;; i74, i78: a stub ntfy server. POSTs are recorded and answered with an id,
;; and a POST to /topic is echoed onto the stream, as ntfy does on a topic
;; the drive both notifies and reads; GET .../json streams those echoes and
;; whatever replies the test queues, a poll at a time.
(deftest asks-are-answered-from-the-phone
  (let [got (atom [])
        gets (atom [])
        replies (java.util.concurrent.LinkedBlockingQueue.)
        server (httpkit/run-server
                (fn [r]
                  (if (= :post (:request-method r))
                    (let [id (str (random-uuid))
                          body (slurp (:body r))]
                      (swap! got conj {:title (get-in r [:headers "title"]) :actions (get-in r [:headers "actions"])
                                       :body body})
                      (when (= "/topic" (:uri r)) (.put replies {:id id :message body}))
                      {:status 200 :body (json/generate-string {:id id :event "message"})})
                    (do (swap! gets conj (str (:uri r) "?" (:query-string r)))
                        {:status 200
                         :body (str/join "\n" (cons "{\"event\":\"open\"}"
                                                    (for [m (remove nil? [(.poll replies 300 java.util.concurrent.TimeUnit/MILLISECONDS)])
                                                          :let [{:keys [id message]} (if (map? m) m {:message m})]]
                                                      (json/generate-string {:id (or id (str (random-uuid))) :event "message" :message message}))))})))
                {:port 0 :legacy-return-value? false})
        base (str "http://localhost:" (httpkit/server-port server))
        topic-url (str base "/topic?auth=tok")
        sent (fn [n] (loop [i 0] (cond (>= (count @got) n) (nth @got (dec n))
                                       (< i 200) (do (Thread/sleep 100) (recur (inc i))))))
        code-of #(second (re-find #"\ncode (\w+)\n" (:body %)))
        log #(slurp (str (fs/path % "log.edn")))]
    (try
      (binding [ntfy/*ntfy* topic-url ntfy/*ntfy-reply* nil ntfy/*reconnect-ms* 50]
        (testing "a spawn's env has no LUME_NTFY*"
          (let [s (new-session "goal")]
            (item/item-add s ["peek"])
            (agent/spawn s *root* ["i1" "--agent" "ntfy-peek"])
            (is (str/includes? (slurp (:out (first (filter #(= :spawn/started (:type %)) (store/read-events s)))))
                               "ntfy=[]"))))
        (testing "on one topic, a reply that names the ask and quotes its code answers it, and the drive carries on"
          (let [s (new-session "goal")
                _ (item/item-add s ["do it"])
                out (java.io.StringWriter.)
                drive (future (reset! *driver-turns* [(lume-cmd "ask" "ship it? (y/n)")])
                              (binding [*out* out]
                                (drive/drive s *root* {:agent "ntfy-driver" :turns 3 :wait true})))
                {:keys [title actions] :as n} (sent 1)
                code (code-of n)]
            (is (= "lume: ask a1 from d1" title) "sent once, by the drive, with a code")
            (is (re-matches #"[a-z2-9]{8}" code))
            (is (= (str "http, Yes, " topic-url ", method=POST, body=a1 " code " yes, clear=true; "
                        "http, No, " topic-url ", method=POST, body=a1 " code " no, clear=true")
                   actions))
            (doseq [m ["a1 wrong yes" (str "a2 " code " yes") "just yes" (str " A1 " (str/upper-case code) "  yes, ship it\n")]]
              (.put replies m))
            (is (= "drive stopped: d2 logged no events" (deref drive 30000 "timed out")))
            (is (= [{:ask/id "a1" :answer "yes, ship it" :by "human" :via :ntfy}]
                   (map #(select-keys % [:ask/id :answer :by :via]) (filter #(= :ask/answered (:type %)) (store/read-events s)))))
            (is (re-find #"^/topic/json\?auth=tok&since=\d+$" (first @gets)) "the token stays in the query")
            (is (= ["ignored a reply on ntfy: wrong code for a1"
                    "ignored a reply on ntfy: no ask waiting with id a2"
                    "ignored a reply on ntfy: not '<ask id> <code> <answer>'"]
                   (filter #(str/starts-with? % "ignored") (str/split-lines (str out))))
                "the drive's own post, echoed back, isn't reported")
            (testing "the code and the URL are never logged"
              (is (not-any? #(str/includes? (log s) %) [code "tok" "/topic"])))))
        (testing "with LUME_NTFY_REPLY, answers come from that topic; an answer at the terminal ends the wait, and spends the code"
          (binding [ntfy/*ntfy* (str base "/notify") ntfy/*ntfy-reply* (str base "/reply")]
            (let [s (new-session "goal")
                  _ (item/item-add s ["do it"])
                  before (count @got)
                  gets-before (count @gets)
                  drive (future (reset! *driver-turns* [(lume-cmd "ask" "which db: (a) postgres, (b) sqlite, or (c) one \"ünusual\"; I pick a")])
                                (binding [*out* (java.io.StringWriter.)]
                                  (drive/drive s *root* {:agent "ntfy-driver" :turns 3 :wait true})))
                  n (sent (inc before))
                  code (code-of n)]
              (is (= (str/join "; " (for [[label l] [["a: postgres" "a"] ["b: sqlite" "b"] ["c: one nusual" "c"]]]
                                      (str "http, " label ", " base "/reply, method=POST, body=a1 " code " " l ", clear=true")))
                     (:actions n))
                  "lettered options: a button each, posting the letter")
              (ask/answer s ["a1" "postgres"])
              (is (= "drive stopped: d2 logged no events" (deref drive 30000 "timed out")))
              (is (re-find #"^/reply/json\?since=\d+$" (nth @gets gets-before)))
              (.put replies (str "a1 " code " sqlite"))
              (Thread/sleep 500)
              (is (= ["postgres"] (keep #(when (= :ask/answered (:type %)) (:answer %)) (store/read-events s))))))))
      (testing "--wait-on-ask needs LUME_NTFY"
        (let [s (new-session "goal")]
          (binding [ntfy/*ntfy* nil ntfy/*ntfy-reply* "http://localhost:1/x"]
            (is (thrown-with-msg? Exception #"needs LUME_NTFY "
                                  (#'drive/drive-args ["--wait-on-ask"]))))))
      (finally (httpkit/server-stop! server)))))

(deftest lettered-asks-have-options
  (let [lettered #'ntfy/lettered]
    (is (= [["a" "keep going"] ["b" "try X"] ["c" "stop"]]
           (lettered "i45 plateaued: (a) keep going (b) try X (c) stop? I'd pick c; see n120")))
    (is (nil? (lettered "which db?")) "free text: no buttons")
    (is (nil? (lettered "(a) w (b) x (c) y (d) z")) "more than ntfy's three")
    (is (nil? (lettered "(a) w (c) y")) "not a, b, c in order")
    (is (nil? (lettered "see (a) above")))))

;; i96: a subscriber stopped mid-request used to send (or have recorded) a GET
;; after :stop returned, so the next case's count of GETs caught it.
(deftest a-stopped-subscriber-sends-nothing-after
  (let [gets (atom [])
        entered (promise)
        server (httpkit/run-server
                (fn [r]
                  (deliver entered true)
                  (Thread/sleep 200)
                  (swap! gets conj (:query-string r))
                  {:status 200 :body "{\"event\":\"open\"}"})
                {:port 0 :legacy-return-value? false})
        {:keys [stop]} (binding [ntfy/*reconnect-ms* 10]
                         (ntfy/subscribe! (str "http://localhost:" (httpkit/server-port server) "/reply")))]
    (try
      (is (deref entered 5000 false))
      (stop)
      (let [n (count @gets)]
        (is (= 1 n) "the request in flight finished before :stop returned")
        (Thread/sleep 400)
        (is (= n (count @gets)) "and none followed"))
      (finally (httpkit/server-stop! server)))))

(deftest a-stalled-reply-stream-reconnects
  (let [gets (atom [])
        open (atom [])
        server (httpkit/run-server
                (fn [r]
                  (swap! gets conj (:query-string r))
                  (httpkit/as-channel r {:on-open
                                         (fn [ch]
                                           (swap! open conj ch)
                                           ;; Before m1: m1, then silence with the socket held open.
                                           ;; From m1 on: m2, then silence again. Chosen by since,
                                           ;; not by arrival order, so a timed-out or retried
                                           ;; connect under load still gets the right reply.
                                           (httpkit/send! ch {:status 200
                                                              :body (str (json/generate-string
                                                                          (if (str/ends-with? (:query-string r) "since=m1")
                                                                            {:id "m2" :event "message" :message "second"}
                                                                            {:id "m1" :event "message" :message "first"}))
                                                                         "\n")}
                                                          false))}))
                {:port 0 :legacy-return-value? false})
        {:keys [queue stop]} (binding [ntfy/*idle-ms* 300 ntfy/*reconnect-ms* 10]
                               (ntfy/subscribe! (str "http://localhost:" (httpkit/server-port server) "/reply?auth=tok")))
        stall {:error "no data for 0s, reconnecting"}
        ;; The next message or stall, past transport errors: a loaded machine
        ;; can time out a connect (the 300ms request timeout), which only retries.
        take! #(loop []
                 (let [m (.poll ^java.util.concurrent.LinkedBlockingQueue queue 5 java.util.concurrent.TimeUnit/SECONDS)]
                   (if (and (:error m) (not= stall m)) (recur) m)))]
    (try
      (is (= {:id "m1" :message "first"} (take!)))
      (is (= stall (take!)) "the silent stream is dropped")
      (is (= {:id "m2" :message "second"} (take!)) "and the next connection delivers")
      (is (re-find #"^auth=tok&since=\d+$" (first @gets)))
      (is (some #{"auth=tok&since=m1"} @gets) "resuming after the last message, so none is lost")
      (finally (stop)
               (run! #(httpkit/close %) @open)
               (httpkit/server-stop! server)))))
