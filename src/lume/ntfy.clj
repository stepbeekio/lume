(ns lume.ntfy
  "Hand-backs reach the human remotely: when an ask opens or a drive stops,
  lume POSTs a short message to the ntfy topic URL in LUME_NTFY and logs
  :notify/sent or :notify/failed. Unset is a no-op, and a failed send never
  fails the command. The URL is a secret, so it's never logged. A drive
  --wait-on-ask reads answers back with wait-for-answer!."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lume.ask :as ask]
            [lume.kernel :as kernel]))

(def ^:dynamic *ntfy*
  "The ntfy topic URL hand-backs are POSTed to: LUME_NTFY, else nil (no
  notifications). Tests bind it."
  (not-empty (System/getenv "LUME_NTFY")))

(def ^:dynamic *ntfy-reply*
  "The ntfy topic URL a drive --wait-on-ask reads answers from:
  LUME_NTFY_REPLY, else nil (LUME_NTFY's topic). Tests bind it."
  (not-empty (System/getenv "LUME_NTFY_REPLY")))

(def ^:dynamic *sent*
  "nil, or an atom of the ids ntfy gave the messages this process POSTed: a
  drive reading replies on the topic it notifies skips its own posts."
  nil)

(defn reply-url
  "Where answers come from: LUME_NTFY_REPLY, else LUME_NTFY's own topic."
  []
  (or *ntfy-reply* *ntfy*))

(defn- ntfy-target
  "[url headers] for an ntfy topic URL: a token rides in it as ntfy's ?auth=
  param, or as user info (https://:<token>@host/topic), sent as basic auth."
  [url]
  (let [uri (java.net.URI. url)]
    (if-let [info (.getRawUserInfo uri)]
      [(str (java.net.URI. (.getScheme uri) nil (.getHost uri) (.getPort uri) (.getPath uri) (.getQuery uri) nil))
       {"Authorization" (str "Basic " (.encodeToString (java.util.Base64/getEncoder)
                                                       (.getBytes (java.net.URLDecoder/decode info "UTF-8"))))}]
      [url {}])))

(defn- ntfy-post
  "POST message to an ntfy topic, titled title (ASCII: it's a header), with
  any other headers. nil when it was accepted, else why not."
  [url title message & [headers]]
  (try (let [[url auth] (ntfy-target url)
             {:keys [status body]} ((requiring-resolve 'babashka.http-client/post)
                                    url {:headers (merge {"Title" title} headers auth) :body message
                                         :timeout 5000 :throw false})]
         (if (<= 200 status 299)
           (when-let [id (and *sent* (:id (try ((requiring-resolve 'cheshire.core/parse-string) (str body) true)
                                               (catch Exception _ nil))))]
             (swap! *sent* conj id)
             nil)
           (str "HTTP " status)))
       (catch Exception e (str (.getSimpleName (class e)) (some->> (ex-message e) (str ": "))))))

(defn notify!
  "Tell the human about event e (an :ask/opened or :drive/stopped) if
  LUME_NTFY is set, and log the send. The message is text, then secret (a
  reply code: sent, never logged), then the session; headers go with it.
  Never throws."
  [session e title text & {:keys [secret headers]}]
  (when-let [url *ntfy*]
    (try (let [why (ntfy-post url title (str/join "\n" (remove nil? [text secret (kernel/session-id session)])) headers)]
           (kernel/append! session (constantly (merge {:type (if why :notify/failed :notify/sent) :on (:type e)}
                                                      (select-keys e [:ask/id :item/id :drive/id])
                                                      {:text text}
                                                      (when why {:error why})))))
         (catch Exception _ nil))))

(def ^:dynamic *ask-notices*
  "False inside a drive --wait-on-ask, which sends each ask itself, with its
  reply code."
  true)

(defn- yes-no?
  "An ask whose question ends (y/n) or (yes/no) gets Yes and No buttons."
  [question]
  (boolean (re-find #"(?i)\((?:y/n|yes/no)\)\W*$" question)))

(defn- lettered
  "The options of a question that lists (a), (b) and maybe (c), as
  [[letter text] ...]: each text runs to the next option or the first ?;.!
  and loses the characters ntfy's Actions header can't carry (separators,
  quotes, non-ASCII). nil unless the letters are a, b[, c] in order (ntfy
  allows three buttons)."
  [question]
  (let [os (for [[_ l t] (re-seq #"\(([a-z])\)\s*([^()]*)" question)]
             [l (-> t (str/split #"[?;.!]" 2) first (str/replace #"\s+" " ") (str/replace #"[,=\"'\x00-\x1f\x7f-￿]" "")
                    (str/replace #"\s+or\s*$" "") str/trim)])]
    (when (and (<= 2 (count os) 3) (= (map first os) (take (count os) ["a" "b" "c"])))
      os)))

(defn- notify-ask!
  "Send an ask, unless a waiting drive will. With a reply code, the message
  says how to answer it and a yes/no or (a)/(b)/(c) ask carries buttons
  that post the answer to the reply topic; the code and that URL are never logged."
  [session {id :ask/id :keys [question] :as opened} & [code]]
  (when (or code *ask-notices*)
    (let [item (ask/ask-item (kernel/read-events session) opened)
          [reply auth] (when code (ntfy-target (reply-url)))
          button (fn [label answer]
                   (str/join ", " (concat ["http" label reply "method=POST"]
                                          (for [[k v] auth] (str "headers." k "=" v))
                                          [(str "body=" id " " code " " answer) "clear=true"])))]
      (notify! session (cond-> opened item (assoc :item/id item))
               (str "lume: ask " id (when item (str " on " item)) " from " (:by opened))
               (str question "\n" (if code (str "reply " id " <code> <answer>, or lume answer " id) (str "lume answer " id)))
               :secret (when code (str "code " code))
               :headers (when-let [buttons (and code
                                                (seq (if (yes-no? question)
                                                       [["Yes" "yes"] ["No" "no"]]
                                                       (for [[l t] (lettered question)
                                                             :let [label (str/trim (str l ": " t))]]
                                                         [(subs label 0 (min 30 (count label))) l]))))]
                          {"Actions" (str/join "; " (map #(apply button %) buttons))})))))

;; Answers from the phone: a drive --wait-on-ask doesn't hand back on an open
;; ask. It sends the ask with a one-time code, held only in its memory, and
;; reads the reply topic (LUME_NTFY_REPLY, else LUME_NTFY's) until a reply
;; names the ask and quotes its code ("a12 <code> <answer>"), so a leaked URL
;; can't answer and a stale reply can't land on a newer ask. A good reply is
;; the human's answer. On a shared topic the drive skips its own posts.

(def ^:dynamic *reconnect-ms*
  "How long the reply subscriber waits before reconnecting. Tests bind it."
  5000)

(def ^:dynamic *idle-ms*
  "How long the reply stream may go silent before the subscriber drops it and
  reconnects. ntfy sends a keepalive every ~45s, so two missed ones mean a
  half-dead connection. Tests bind it."
  100000)

(defn- reply-code
  "8 characters a phone types easily, from a secure random source."
  []
  (let [rnd (java.security.SecureRandom.)
        abc "abcdefghjkmnpqrstuvwxyz23456789"]
    (apply str (repeatedly 8 #(nth abc (.nextInt rnd (count abc)))))))

(defn- ntfy-json-url
  "The streaming JSON endpoint of the topic at url, from since (a unix time
  or message id) on, keeping its query (?auth=)."
  [url since]
  (let [[base query] (str/split url #"\?" 2)]
    (str (str/replace base #"/+$" "") "/json?" (when query (str query "&")) "since=" since)))

(defn subscribe!
  "Stream the ntfy topic at url, from now on, into :queue: each message as
  {:id id :message text}, each failure as {:error why}. It runs on a daemon thread,
  reconnecting after a dropped stream from the last message it saw, until
  (:stop) is called. A stream silent for *idle-ms* (no message, no
  keepalive) is dropped by a watchdog, so a half-dead connection can't block
  it forever."
  [url]
  (let [q (java.util.concurrent.LinkedBlockingQueue.)
        [url auth] (ntfy-target url)
        since (atom (str (quot (System/currentTimeMillis) 1000)))
        running (atom true)
        stream (atom nil)
        heard (atom (System/currentTimeMillis))
        dropped (atom false)
        idle-ms (long *idle-ms*)
        http-get (requiring-resolve 'babashka.http-client/get)
        parse (requiring-resolve 'cheshire.core/parse-string)
        run (bound-fn []
              (while @running
                (try (let [{:keys [status body]} (http-get (ntfy-json-url url @since)
                                                           {:headers auth :as :stream :throw false :timeout idle-ms})]
                       (reset! heard (System/currentTimeMillis))
                       (reset! dropped false)
                       (reset! stream body)
                       ;; Stopped while connecting: :stop found no stream to close.
                       (when-not @running (.close ^java.io.InputStream body))
                       (with-open [r (io/reader body)]
                         (if (<= 200 status 299)
                           (doseq [line (line-seq r)
                                   :let [_ (reset! heard (System/currentTimeMillis))
                                         m (try (parse line true) (catch Exception _ nil))]
                                   :when (= "message" (:event m))]
                             (reset! since (:id m))
                             (.put q {:id (:id m) :message (str (:message m))}))
                           (.put q {:error (str "HTTP " status)}))))
                     (catch Exception e
                       (when (and @running (not @dropped))
                         (.put q {:error (str (.getSimpleName (class e)) (some->> (ex-message e) (str ": ")))}))))
                (reset! stream nil)
                (when @running (Thread/sleep (long *reconnect-ms*)))))
        ;; Closing the stream ends the blocked read; run then reconnects
        ;; from the last message it saw.
        watch (fn []
                (while @running
                  (Thread/sleep (max 10 (quot idle-ms 4)))
                  (when-let [s @stream]
                    (when (> (- (System/currentTimeMillis) @heard) idle-ms)
                      (reset! heard (System/currentTimeMillis))
                      (reset! dropped true)
                      (.put q {:error (str "no data for " (quot idle-ms 1000) "s, reconnecting")})
                      (try (.close ^java.io.InputStream s) (catch Exception _ nil))))))]
    (let [t (doto (Thread. ^Runnable run) (.setDaemon true) (.start))]
      (doto (Thread. ^Runnable watch) (.setDaemon true) (.start))
      ;; :stop waits, a bounded while, for a request in flight, so none is
      ;; sent after it returns.
      {:queue q
       :stop #(do (reset! running false)
                  (some-> ^java.io.InputStream @stream (.close))
                  (.join t 2000))})))

(defn- open-ask-ids [session]
  (map :id (ask/open-asks (kernel/state session))))

(defn- reply-answer
  "[ask-id answer] if message answers an ask in codes ({id code}), else why not."
  [codes message]
  (if-let [[_ id code answer] (re-matches #"(?s)\s*([aA]\d+)\s+(\S+)\s+(.*\S)\s*" message)]
    (let [id (str/lower-case id)
          want (codes id)]
      (cond (nil? want) (str "no ask waiting with id " id)
            (not (java.security.MessageDigest/isEqual (.getBytes ^String want) (.getBytes (str/lower-case code))))
            (str "wrong code for " id)
            :else [id answer]))
    "not '<ask id> <code> <answer>'"))

(defn wait-for-answer!
  "Send every open ask that has no code yet its own, then wait until a reply
  answers one (logged :ask/answered :by human :via :ntfy) or they're answered
  some other way (lume answer at a terminal). codes is an atom, {id code};
  replies delivers the reply subscriber. Messages whose id is in *sent* are
  the drive's own, echoed on a shared topic, and skipped."
  [session codes replies say]
  (let [opened (into {} (keep #(when (= :ask/opened (:type %)) [(:ask/id %) %]) (kernel/read-events session)))]
    (doseq [id (open-ask-ids session) :when (not (@codes id))]
      (let [code (reply-code)]
        (swap! codes assoc id code)
        (notify-ask! session (opened id) code)
        (say (str "waiting on " id " for an answer on ntfy, or lume answer " id)))))
  (let [q (:queue @replies)]
    (loop [last-error nil]
      (swap! codes select-keys (open-ask-ids session))
      (when (seq @codes)
        (let [{:keys [message error] :as m} (.poll ^java.util.concurrent.LinkedBlockingQueue q 1 java.util.concurrent.TimeUnit/SECONDS)
              message (when-not (contains? @*sent* (:id m)) message)
              [id answer :as ok] (when message (reply-answer @codes message))]
          (cond
            error (do (when (not= error last-error) (say (str "ntfy replies: " error "; retrying")))
                      (recur error))
            ;; The reply carried the ask's one-time code, which only the human
            ;; was sent, so it is the human answering, whoever runs the drive.
            (vector? ok) (do (swap! codes dissoc id)
                             (when (binding [kernel/*actor* "human"]
                                     (kernel/append! session (fn [events]
                                                               (when (= :open (get-in (kernel/project events) [:asks id :state]))
                                                                 {:type :ask/answered :ask/id id :answer answer
                                                                  :via :ntfy}))))
                               (say (str id " answered from ntfy"))))
            message (do (say (str "ignored a reply on ntfy: " ok)) (recur last-error))
            :else (recur last-error)))))))

;; ntfy's :reactions send each ask opened and each drive stop. A worker's or
;; driver's events react when it exits, in the lume that started it.
(def feature
  {:name "ntfy"
   :events {:notify/sent {:req #{:on :text}}
            :notify/failed {:req #{:on :text :error}}}
   :reactions [{:on :ask/opened :run (fn [e] (notify-ask! kernel/*session* e))}
               {:on :drive/stopped :run (fn [e] (notify! kernel/*session* e "lume: drive stopped" (:why e)))}]})
