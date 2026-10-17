(ns lume.note-test
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [lume.cli :as cli]
            [lume.kernel :as kernel]
            [lume.note :as note]
            [lume.status :as status]
            [lume.store :as store])
  (:import (java.time Instant)))

(def ^:dynamic *root* nil)

(use-fixtures :each
  (fn [t]
    (let [home (str (fs/create-temp-dir {:prefix "lume-home"}))
          root (str (fs/create-temp-dir {:prefix "lume-repo"}))]
      (try
        (binding [store/*home* home store/*session* nil kernel/*actor* "human" *root* root
                  kernel/*features* cli/features
                  kernel/*confirm* (constantly true) kernel/*read-line* (constantly nil)]
          (t))
        (finally (fs/delete-tree home) (fs/delete-tree root))))))

(defn- new-session [goal]
  (cli/init *root* [goal])
  (store/current-session *root*))

(deftest note-show-reads-a-note-in-full
  (let [s (new-session "goal")
        long-text (str "start " (str/join " " (repeat 80 "word")) " the end")]
    (binding [kernel/*actor* "s1"] (note/note s [long-text]))
    (note/note s ["--pin" "lasting"])
    (note/note s ["--pin" "was lasting"])
    (note/note s ["--unpin" "n3"])
    (let [out (note/note-show s ["n1"])]
      (is (re-find #"^n1\nnoted    \d{4}-\d\d-\d\dT\d\d:\d\d by s1\n" out))
      (is (str/ends-with? out (str "\n" long-text))))
    (is (str/starts-with? (note/note-show s ["n2"]) "n2 pinned\n"))
    (is (re-find #"^n3 unpinned\nnoted .*\nunpinned \S+ by human\nwas lasting$" (note/note-show s ["n3"])))
    (binding [store/*session* s]
      (is (str/starts-with? (kernel/run ["note" "show" "n2"]) "n2 pinned"))
      (is (= "noted n4" (kernel/run ["note" "show"])) "a note that just says show is still a note"))
    (is (thrown-with-msg? Exception #"no such note: n9" (note/note-show s ["n9"])))
    (is (thrown-with-msg? Exception #"usage: lume note show <id>" (note/note-show s [])))))

(deftest pinned-notes-outlive-the-window
  (let [s (new-session "goal")
        id (cli/item-add s ["read the notes"])]
    (is (= "pinned n1" (note/note s ["--pin" "run bb test before done"])))
    (dotimes [n 12] (note/note s [(str "chatter " n)]))
    (is (= "noted n14" (note/note s ["latest"])))
    (let [out (status/render "sess" (store/read-events s) (Instant/now))]
      (is (re-find #"(?m)^pinned  n1 run bb test before done$" out))
      (is (not (re-find #"\] run bb test" out)) "pinned notes are left out of notes")
      (is (< (str/index-of out "pinned  n1") (str/index-of out "open    i1"))))
    (testing "workers see every pinned note and the last 10 others"
      (let [state (status/project (store/read-events s))
            prompt (str (str/join "\n" (status/prompt-lines state (get-in state [:items id]))) "\n")]
        (is (str/includes? prompt "Pinned notes:\n- run bb test before done\n"))
        (is (str/includes? prompt "- chatter 3\n"))
        (is (not (str/includes? prompt "- chatter 2\n")))
        (is (str/includes? prompt "- latest\n"))))
    (testing "unpinning returns a note to the notes section"
      (is (= "unpinned n1" (note/note s ["--unpin" "n1"])))
      (is (thrown-with-msg? Exception #"no pinned note: n1" (note/note s ["--unpin" "n1"])))
      (is (thrown-with-msg? Exception #"no pinned note: n99" (note/note s ["--unpin" "n99"])))
      (let [out (status/render "sess" (store/read-events s) (Instant/now))]
        (is (not (re-find #"(?m)^pinned" out)))
        (is (str/includes? out "+9 more") "14 notes, 5 shown")))))
