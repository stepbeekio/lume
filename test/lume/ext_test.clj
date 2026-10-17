(ns lume.ext-test
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [lume.ext :as ext]
            [lume.features :as features]
            [lume.inspect :as inspect]
            [lume.item :as item]
            [lume.kernel :as kernel]
            [lume.session :as session]
            [lume.status :as status]
            [lume.store :as store])
  (:import (java.time Instant)))

(def ^:dynamic *root* nil)

(use-fixtures :each
  (fn [t]
    (let [home (str (fs/create-temp-dir {:prefix "lume-home"}))
          root (str (fs/create-temp-dir {:prefix "lume-repo"}))]
      (try
        (binding [store/*home* home store/*session* nil kernel/*actor* "human" kernel/*spawned-as* nil
                  kernel/*depth* 0 *root* root
                  kernel/*features* features/all
                  kernel/*confirm* (constantly true) kernel/*read-line* (constantly nil)]
          (t))
        (finally (fs/delete-tree home) (fs/delete-tree root))))))

(defn- new-session [goal]
  (session/init *root* [goal])
  (store/current-session *root*))

;; The test extension, renamed: each name is its own ns, so tests running in
;; parallel never load different code into one.
(def ^:private tally-src (slurp "test/lume/ext/tally.clj"))

(defn- put! [s name & {:keys [edit] :or {edit identity}}]
  (let [f (fs/path (ext/ext-dir s) (str name ".clj"))]
    (fs/create-dirs (ext/ext-dir s))
    (spit (str f) (edit (str/replace tally-src "tally" name)))
    (str f)))

(defn- lume
  "Run a command as -main would: the built-ins and s's extensions."
  [s & args]
  (binding [store/*session* s
            kernel/*features* (ext/with-extensions features/all s)]
    (kernel/run args)))

(defn- gate! [s]
  (item/run-gate s {:id (item/item-add s ["gated" "--gate" "true"]) :gate "true"}))

(defn- render [s]
  (binding [kernel/*features* (ext/with-extensions features/all s)]
    (status/render "sess" (store/read-events s) (Instant/now))))

(deftest loading-an-extension-drops-no-repl-names
  ;; bb's require writes the namespace map back from a snapshot, so an ns form
  ;; loading in one thread could drop what a repl's set-up interned in another
  ;; (i97); both hold kernel/ns-lock.
  (let [s (new-session "goal")
        path (put! s "tallyrace")
        _ (lume s "ext" "approve" path)
        stop (atom false)
        loads (doall (for [_ (range 3)]
                       (future (while (not @stop) (ext/with-extensions features/all s)))))
        missing (doall (for [i (range 500)
                             :let [ns-sym (symbol (str "lume.ext-race-" (System/nanoTime) "-" i))]]
                         (try (inspect/repl-ns! ns-sym s)
                              (filterv #(nil? (ns-resolve ns-sym %)) '[events state session sid refresh! render measure])
                              (finally (remove-ns ns-sym)))))]
    (reset! stop true)
    (run! deref loads)
    (is (= [] (apply concat missing)))))

(deftest extensions-load-only-as-approved
  (let [s (new-session "goal")
        path (put! s "tally")]
    (testing "an unapproved extension doesn't load, and status says so in one line"
      (is (thrown-with-msg? Exception #"unknown command: tally" (lume s "tally")))
      (is (re-find #"(?m)^ext     tally unapproved — lume ext$" (render s)))
      (is (re-find #"^tally unapproved · sha256 \w{12} — lume ext approve tally$" (lume s "ext"))))
    (testing "approval is the human's, at a tty, typing the name after reading the source"
      (binding [kernel/*actor* "s1"]
        (is (thrown-with-msg? Exception #"only the human approves" (lume s "ext" "approve" "tally"))))
      (binding [kernel/*confirm* (constantly false)]
        (is (thrown-with-msg? Exception #"interactive terminal" (lume s "ext" "approve" "tally"))))
      (let [asked (atom nil)]
        (binding [kernel/*confirm* (fn [prompt expected] (reset! asked [prompt expected]) true)]
          (is (re-find #"^approved tally · sha256 \w{12}" (lume s "ext" "approve" path))))
        (is (= "tally" (second @asked)))
        (is (str/includes? (first @asked) "(ns lume.ext.tally")))
      (is (= ["tally" path] ((juxt :ext/name :path) (last (store/read-events s))))))
    (testing "approved, it loads: commands, events, folds, guards, reactions"
      (is (= "tallied 3" (lume s "tally" "add" "3")))
      (is (thrown-with-msg? Exception #"3 is already tallied" (lume s "tally" "add" "3")))
      (is (= :tally/seen (:type (last (store/read-events s)))) "its reaction ran")
      (gate! s)
      (is (= "total 3 · gates 1" (lume s "tally")))
      (is (re-find #"^tally loaded · sha256 \w{12} · state \{:counts \[3\], :gates 1\}$" (lume s "ext")))
      (is (re-find #"(?m)^tally   total 3 · gates 1\n        \+3$" (render s)))
      (is (not (re-find #"(?m)^ext " (render s))) "nothing to say about extensions that loaded"))
    (testing "a changed file doesn't load until approved again"
      (spit path (str/replace (slurp path) "the total so far" "the sum"))
      (is (thrown-with-msg? Exception #"unknown command: tally" (lume s "tally")))
      (is (re-find #"(?m)^ext     tally changed since approval — lume ext$" (render s)))
      (lume s "ext" "approve" "tally")
      (is (= "total 3 · gates 1" (lume s "tally")) "its state is the log's, so it survives"))))

(deftest extensions-keep-to-their-corner
  (let [s (new-session "goal")]
    (testing "its fold sees and returns only its own state"
      (put! s "hijack" :edit #(str/replace % "(fn [s _] (update s :gates inc))" "(fn [s _] (assoc s :goal \"hijacked\" :gates (:goal s)))"))
      (lume s "ext" "approve" "hijack")
      (gate! s)
      (let [state (binding [kernel/*features* (ext/with-extensions features/all s)]
                    (kernel/project (store/read-events s)))]
        (is (= "goal" (:goal state)))
        (is (= {:counts [] :gates nil :goal "hijacked"} (get-in state [:ext "hijack"])))))
    (testing "status: 2 lines a section, 4 for every extension"
      (put! s "few")
      (lume s "ext" "approve" "few")
      (dotimes [n 3] (lume s "hijack" "add" (str (inc n))) (lume s "few" "add" (str (inc n))))
      (put! s "late")
      (lume s "ext" "approve" "late")
      (let [out (render s)]
        (is (re-find #"(?m)^few     total 6 · gates 1\n        \+3 more — lume few\nhijack  total 6 · gates \n        \+3 more — lume hijack\n" out) out)
        (is (not (str/includes? out "late ")) "the 4 lines are taken")
        (is (<= (count (str/split-lines out)) 44))))))

(deftest extensions-are-refused-by-shape
  (let [s (new-session "goal")
        ;; Why approving it fails, or once approved, why it didn't load.
        refused (fn [n edit]
                  (put! s n :edit edit)
                  (or (try (lume s "ext" "approve" n) nil (catch Exception x (ex-message x)))
                      (binding [kernel/*features* (ext/with-extensions features/all s)]
                        (let [r (some #(when (= n (:name %)) %) (some :ext/report (kernel/features)))]
                          (str (name (:state r)) ": " (:why r))))))]
    (testing "at approval: the ns and what it may reach"
      (is (re-find #"refused: it requires babashka.process; extensions require only lume.kernel"
                   (refused "shell" #(str/replace % "[lume.kernel :as kernel]" "[lume.kernel :as kernel] [babashka.process :as p]"))))
      (is (re-find #"refused: its ns is lume.ext.other, not lume.ext.misnamed"
                   (refused "misnamed" #(str/replace % "(ns lume.ext.misnamed" "(ns lume.ext.other"))))
      (is (re-find #"refused: it names System/exit"
                   (refused "exits" #(str % "\n(defn bye [] (System/exit 1))\n"))))
      (is (re-find #"refused: it names require"
                   (refused "sneaky" #(str % "\n(require 'babashka.process)\n"))))
      (is (re-find #"refused: it names java.io.File."
                   (refused "javan" #(str % "\n(defn f [] (java.io.File. \"x\"))\n"))))
      (is (re-find #"refused: item is a built-in name" (refused "item" identity))))
    (testing "at load: the feature map"
      (is (re-find #"refused: it declares :gate/fake; its event types are :outside/…"
                   (refused "outside" #(str/replace % ":outside/seen {:req #{:of}}" ":gate/fake {:req #{:of}}"))))
      (is (re-find #"refused: it has command lume add; its commands are lume squat …"
                   (refused "squat" #(str/replace % ":path [\"squat\" \"add\"]" ":path [\"add\"]"))))
      (is (re-find #"refused: it guards :note/added; it guards only its own"
                   (refused "guard" #(str/replace % ":guards {:guard/added" ":guards {:note/added"))))
      (is (re-find #"refused: its feature has :prompt"
                   (refused "prompts" #(str/replace % ":init {" ":prompt (fn [_ _] []) :init {"))))
      (is (re-find #"refused: it failed to load"
                   (refused "broken" #(str % "\n(def oops (inc nil))\n"))))
      (is (re-find #"(?m)^outside refused: it declares :gate/fake; .* · sha256 \w{12}$" (lume s "ext")))
      (is (re-find #"(?m)^ext     broken refused: .*…$" (render s)) "one line, however many"))))
