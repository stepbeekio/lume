(ns lume.measure-test
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [lume.agents :as agents]
            [lume.cli :as cli]
            [lume.kernel :as kernel]
            [lume.measure :as measure]
            [lume.ntfy :as ntfy]
            [lume.policy :as policy]
            [lume.spec :as spec]
            [lume.status :as status]
            [lume.store :as store])
  (:import (java.time Instant)))

(def ^:dynamic *root* nil)

(use-fixtures :each
  (fn [t]
    (let [home (str (fs/create-temp-dir {:prefix "lume-home"}))
          root (str (fs/create-temp-dir {:prefix "lume-repo"}))]
      (try
        (binding [store/*home* home store/*session* nil kernel/*actor* "human" kernel/*spawned-as* nil *root* root
                  kernel/*features* cli/features
                  kernel/*confirm* (constantly true) kernel/*read-line* (constantly nil)
                  ntfy/*ntfy* nil]
          (t))
        (finally (fs/delete-tree home) (fs/delete-tree root))))))

(defn- new-session [goal]
  (cli/init *root* [goal])
  (store/current-session *root*))

;; Workers for spawns on measured items: one that fails, one that adds a line to made.
(defmethod agents/command :fails [_ _prompt _opts]
  ["sh" "-c" "printf 'working\\ndone by %s\\n' \"$LUME_ACTOR\"; exit 3"])

(defmethod agents/command :appends [_ _prompt _opts]
  ["sh" "-c" "echo more >> made"])

(deftest measurements-are-ledger-rows
  (testing "named numbers, else the last number"
    (is (= {"build_s" 9.5 "hits" 40 "tests" 21} (measure/measure-values "build_s=12 hits: 40\ntests=21\nbuild_s=9.5")))
    (is (= {"value" 0} (measure/measure-values "Ran 21 tests containing 197 assertions. 0 failures")))
    (is (nil? (measure/measure-values "nothing here"))))
  (testing "numbers with no digit before or after the point, as bc prints them"
    (is (= {"secs" 0.915} (measure/measure-values "secs=.915 915454175")))
    (is (= {"a" -0.5 "b" 5.0} (measure/measure-values "a=-.5 b=5.")))
    (is (= {"value" 0.25} (measure/measure-values "took .25")))
    (is (= {"value" -0.5} (measure/measure-values "delta -.5")))
    (is (= {"value" 5.0} (measure/measure-values "done in 5."))))
  (let [s (new-session "goal")
        id (cli/item-add s ["bc" "--gate" "false"])]
    (measure/item-measure s [id "echo secs=.915 915454175"])
    (testing "a bc-style row logs secs, not the trailing number as value"
      (is (re-matches #"m1 i1 secs=0\.915 · \d+\.\ds" (measure/measure s [id])))
      (is (= {"secs" 0.915}
             (:values (get-in (status/project (store/read-events s)) [:items id :last-measure]))))))
  (let [s (new-session "goal")
        id (cli/item-add s ["faster" "--gate" "false"])]
    (is (thrown-with-msg? Exception #"no measurement" (measure/measure s [id])))
    (is (thrown-with-msg? Exception #"usage" (measure/item-measure s [id])))
    (measure/item-measure s [id "pwd; echo build_s=12.5 hits=3"])
    (testing "lume measure prints the row, not the output, and runs in the repo"
      (is (re-matches #"m1 i1 build_s=12\.5 hits=3 · \d+\.\ds" (measure/measure s [id])))
      (let [{:keys [measure last-measure]} (get-in (status/project (store/read-events s)) [:items id])]
        (is (= "pwd; echo build_s=12.5 hits=3" measure))
        (is (= {"build_s" 12.5 "hits" 3} (:values last-measure)))
        (is (str/includes? (slurp (:out last-measure)) (str (fs/file-name *root*))))))
    (testing "a spawn measures what its worker left, before the gate"
      (let [out (cli/spawn s *root* [id "--agent" "fails"])]
        (is (re-find #"\nm2 i1 build_s=12\.5 hits=3 · \d+\.\ds · vs m1\ni1 left open to decide · g1 fail exit 1" out) out)))
    (testing "a failing measurement is still a row, with where its output is"
      (measure/item-measure s [id "echo oops; exit 2"])
      (let [out (measure/measure s [id])]
        (is (str/starts-with? out "m3 i1 no numbers · "))
        (is (re-find #"fail exit 2\n.*measures/m3\.out$" out))))
    (testing "status shows the latest row on the item; item show has them all"
      (let [st (status/render "sess" (store/read-events s) (Instant/now))]
        (is (re-find #"(?m)^            gate false .*\n            measure echo oops; exit 2 · m3 exit 2 · no numbers · " st) st))
      (let [shown (cli/item-show s [id])]
        (is (str/includes? shown "measure echo oops; exit 2"))
        (is (re-find #"measure/ran m1 build_s=12\.5 hits=3 · " shown))
        (is (re-find #"measure/ran m3 no numbers · \d+\.\ds · fail exit 2" shown))))
    (testing "a long command with many numbers stays within width"
      (measure/item-measure s [id (str (apply str (repeat 30 "echo x; ")) (str/join " " (map #(str "echo n" % "=" %) (range 30))) "; exit 127")])
      (measure/measure s [id])
      (let [line (some #(when (str/includes? % "measure ") %)
                       (str/split-lines (status/render "sess" (store/read-events s) (Instant/now))))]
        (is (re-find #"m4 exit 127 · n0=0 n1=1 n10=10 .*…" line) line)
        (is (<= (count line) 110) line)))
    (testing "a row shows its change against the latest passing row of the same command"
      (let [f (str (fs/create-temp-file))
            run! #(do (spit f %) (measure/measure s [id]))]
        (measure/item-measure s [id (str "sh " f)])
        (is (re-matches #"m5 i1 build_s=12\.5 hits=3 · \d+\.\ds" (run! "echo build_s=12.5 hits=3")) "a new command has no baseline")
        (is (re-matches #"m6 i1 build_s=9\.6\(-2\.9\) hits=3 new=1 · \d+\.\ds · vs m5" (run! "echo build_s=9.6 hits=3 new=1")))
        (is (re-find #"^m7 i1 build_s=10\(\+0\.4\) hits=3 · \d+\.\ds · fail exit 1 · vs m6\n" (run! "echo build_s=10 hits=3; exit 1"))
            "a failed row is compared, but is not a baseline")
        (is (re-matches #"m8 i1 build_s=9\.5\(-0\.1\) hits=3 · \d+\.\ds · vs m6" (run! "echo build_s=9.5 hits=3")))
        (let [line (some #(when (str/includes? % "measure ") %)
                         (str/split-lines (status/render "sess" (store/read-events s) (Instant/now))))]
          (is (re-find #"m8 vs m6 build_s=9\.5\(-0\.1\) hits=3 · \d+\.\ds · " line) line))
        (testing "a row is compared with the latest that shares a number's name"
          (is (re-matches #"m9 i1 value=915454175 · \d+\.\ds" (run! "echo value=915454175")) "a misparse has no baseline")
          (is (re-matches #"m10 i1 build_s=9\.12\(-0\.377\) hits=3 · \d+\.\ds · vs m8"
                          (run! "echo build_s=9.1234567 hits=3"))
              "skips the misparse; numbers round to 3 significant digits"))))
    (testing "spreads round too, but never a digit before the point"
      (is (= "n=1235 secs=0.764[0.699..0.773] · 0.0s"
             (measure/measure-text {:values {"secs" 0.7635452455 "n" 1234.567} :spread {"secs" [0.699478972 0.772594053]}
                                    :ms 0 :exit 0}))))
    (testing "a change is marked ~ within noise, ! outside it, only when both rows have spreads"
      (let [base {:measure/id "m1" :values {"a" 41.2 "b" 10 "c" 5} :spread {"a" [40.1 42.0] "b" [9 11]}}]
        (is (= "a=37.9(-3.3 !)[37..38.5] b=10.5(+0.5 ~)[10..11] c=4(-1) · 0.0s · vs m1"
               (measure/measure-text {:values {"a" 37.9 "b" 10.5 "c" 4} :spread {"a" [37.0 38.5] "b" [10 11] "c" [4 4]}
                                      :ms 0 :exit 0 :base base})))
        (is (= "a=37.9(-3.3) · 0.0s · vs m1"
               (measure/measure-text {:values {"a" 37.9} :ms 0 :exit 0 :base base}))
            "a single-run row gets no mark")))
    (testing "lume measure --help prints usage"
      (is (= "usage: lume measure <id>" (kernel/run ["measure" "--help"]))))
    (testing "a closed item can still be measured"
      (cli/item-close s [id "dropped" "x"])
      (is (str/starts-with? (measure/measure s [id]) "m11 i1 ")))
    (testing "lume ledger: every row, closed items' too, each against its base, newest last"
      (let [other (cli/item-add s ["other" "--gate" "true"])]
        (is (= "i2 has no measurement rows" (measure/ledger *root* [other])))
        (measure/item-measure s [other "echo q=4"])
        (measure/measure s [other])
        (let [lines (str/split-lines (measure/ledger *root* []))]
          (is (= 12 (count lines)))
          (is (re-matches #"m1 i1 \d{4}-\d\d-\d\dT\d\d:\d\d build_s=12\.5 hits=3 · \d+\.\ds · pwd; echo build_s=12\.5 hits=3" (first lines)))
          (is (re-matches #"m2 i1 \S+ build_s=12\.5 hits=3 · \d+\.\ds · vs m1" (nth lines 1)) "the same spec isn't repeated")
          (is (re-find #"^m3 i1 \S+ no numbers · \d+\.\ds · fail exit 2 · echo oops; exit 2$" (nth lines 2)))
          (is (re-find #"^m8 i1 \S+ build_s=9\.5\(-0\.1\) hits=3 · \d+\.\ds · vs m6$" (nth lines 7)))
          (is (re-matches #"m12 i2 \S+ q=4 · \d+\.\ds · echo q=4" (last lines))))
        (let [lines (str/split-lines (measure/ledger *root* [id "-n" "3"]))]
          (is (= "+8 earlier rows — lume ledger i1 -n 11" (first lines)))
          (is (str/starts-with? (second lines) "m9 i1 ") "a bounded view names the spec on its first row")
          (is (str/includes? (second lines) (str " · sh ")))
          (is (str/starts-with? (last lines) "m11 i1 ")))
        (testing "a past session's, by id"
          (let [old-id (store/session-id s)]
            (new-session "next")
            (is (= "no measurement rows" (measure/ledger *root* [])))
            (is (= (str "+11 earlier rows — lume ledger --session " old-id " -n 12")
                   (first (str/split-lines (measure/ledger *root* ["--session" old-id "-n" "1"])))))))
        (is (thrown-with-msg? Exception #"no such item" (measure/ledger *root* ["i9"])))
        (doseq [args [["-n" "x"] ["i1" "i2"] ["-n"] ["--session" "a" "--session" "b"]]]
          (is (thrown-with-msg? Exception #"usage" (measure/ledger *root* args)) (pr-str args)))))))

(deftest measurements-repeat
  (testing "a row of runs is each number's median and its spread"
    (is (= {:ms 20 :values {"a" 2 "b" 0.15} :spread {"a" [1 3] "b" [0.1 0.2]}}
           (measure/summarize-runs [{:ms 10 :output "a=1 b=0.1"} {:ms 30 :output "a=3 b=0.2"}
                                    {:ms 20 :output "a=2"}]))))
  (let [s (new-session "goal")
        id (cli/item-add s ["faster" "--gate" "true"])
        f (str (fs/create-temp-file))
        n (str (fs/create-temp-file))
        ;; Each run reads and bumps a counter, so runs differ: the first is a slow warm-up.
        script! #(spit f (str "i=$(cat " n "); echo $((i+1)) > " n "; " %))
        rows #(mapv (fn [_] (measure/measure s [id])) (range %))]
    (script! "if [ $i = 0 ]; then echo t=100; else echo t=$((10 + i % 3)) k=7; fi")
    (spit n "0")
    (doseq [args [["--runs" "0"] ["--runs" "2" "--drop" "2"] ["--runs" "x"] ["--drop" "-1"] ["--runs" "2" "--runs" "3"]]]
      (is (thrown-with-msg? Exception #"usage" (measure/item-measure s (into [id "sh f"] args))) (pr-str args)))
    (is (= "i1 measure sh x ×5 drop 1" (measure/item-measure s [id "sh x" "--runs" "5" "--drop" "1"])))
    (measure/item-measure s [id (str "sh " f) "--runs" "5" "--drop" "1"])
    (testing "runs N times, drops the first K, and the warm-up never counts"
      (is (re-matches #"m1 i1 k=7 t=11\[10\.\.12\] · \d+\.\ds · 4 of 5 runs" (first (rows 1))))
      (is (= "5\n" (slurp n)))
      (let [out (slurp (:out (last (store/read-events s))))]
        (is (= 5 (count (re-seq #"(?m)^--- run \d/5 · exit 0 · " out))) out)
        (is (str/includes? out "t=100"))))
    (testing "the same spec compares with its last passing row"
      (is (re-matches #"m2 i1 k=7 t=10\.5\(-0\.5 ~\)\[10\.\.12\] · \d+\.\ds · 4 of 5 runs · vs m1" (first (rows 1)))))
    (testing "changing runs or drop is a new spec, so a new baseline"
      (measure/item-measure s [id (str "sh " f) "--runs" "3"])
      (is (re-matches #"m3 i1 k=7 t=11\[10\.\.12\] · \d+\.\ds · 3 runs" (first (rows 1))))
      (measure/item-measure s [id (str "sh " f)])
      (is (re-matches #"m4 i1 k=7 t=1[012] · \d+\.\ds" (first (rows 1))) "one run: no spread, no runs")
      (measure/item-measure s [id (str "sh " f) "--runs" "3"])
      (is (str/ends-with? (first (rows 1)) " · 3 runs · vs m3")))
    (testing "a failing run ends the measurement"
      (spit n "0")
      (script! "echo t=$i; [ $i -lt 1 ]")
      (measure/item-measure s [id (str "sh " f) "--runs" "4"])
      (let [out (first (rows 1))]
        (is (re-find #"^m6 i1 t=0\.5\[0\.\.1\] · \d+\.\ds · fail exit 1 in run 2 of 4\n.*m6\.out$" out) out))
      (is (= "2\n" (slurp n))))
    (testing "status shows the spec and stays within width; item show has the environment"
      (script! "echo t=1")
      (measure/item-measure s [id (str "sh " f " " (apply str (repeat 20 "x"))) "--runs" "10" "--drop" "1"])
      (rows 1)
      (let [line (some #(when (str/includes? % "measure ") %)
                       (str/split-lines (status/render "sess" (store/read-events s) (Instant/now))))]
        (is (re-find #"measure sh .*… ×10 drop 1 · m7 t=1 · " line) line)
        (is (<= (count line) 110) line))
      (let [shown (cli/item-show s [id])
            {:keys [env]} (last (store/read-events s))]
        (is (string? (:host env)))
        (is (number? (:load env)))
        (is (re-find #"(?m)^measure sh .* ×10 drop 1$" shown) shown)
        (is (re-find #"item/measuring sh .* ×10 drop 1 \[human\]" shown) shown)
        (is (re-find #"measure/ran m7 t=1 · \d+\.\ds · 9 of 10 runs · on \S+ load [\d.]+ \[human\]" shown) shown)))
    (testing "in a repo, the row records the commit it measured"
      (p/shell {:dir *root* :out :string :err :string} "git" "init" "-q")
      (p/shell {:dir *root* :out :string :err :string}
               "git" "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-q" "--allow-empty" "-m" "x")
      (let [head (str/trim (:out (p/shell {:dir *root* :out :string} "git" "rev-parse" "--short" "HEAD")))]
        (rows 1)
        (is (= head (:commit (:env (last (store/read-events s))))))
        (is (str/includes? (cli/item-show s [id]) (str " at " head " [human]")))))))

(deftest a-spawn-has-a-before-row
  (let [s (new-session "goal")
        id (cli/item-add s ["fewer lines" "--gate" "true"])
        item #(get-in (status/project (store/read-events s)) [:items id])]
    (spit (str (fs/path *root* "made")) "start\n")
    (is (= "i1 measure echo t=$(wc -l < made) before" (measure/item-measure s [id "echo t=$(wc -l < made)" "--before"])))
    (is (re-matches #"m1 i1 t=1 · \d+\.\ds" (measure/measure s [id])))
    (is (re-matches #"m2 i1 t=1 · \d+\.\ds · vs m1" (measure/measure s [id])) "before any spawn, the row before is the base")
    (testing "--before: a spawn measures first, then after, a pair"
      (let [out (cli/spawn s *root* [id "--agent" "appends"])]
        (is (re-find #"^m3 i1 t=1 · \d+\.\ds · vs m2 · before s1\ns1 i1 exit 0" out) out)
        (is (re-find #"\nm4 i1 t=2\(\+1\) · \d+\.\ds · vs m3\ni1 left open to decide · g1 pass" out) out))
      (is (= "s1" (:before-spawn (first (filter #(= "m3" (:measure/id %)) (store/read-events s)))))))
    (testing "every later row compares with the before row, not the row before it"
      (spit (str (fs/path *root* "made")) "more\n" :append true)
      (is (re-matches #"m5 i1 t=3\(\+2\) · \d+\.\ds · vs m3" (measure/measure s [id]))))
    (testing "a before row isn't counted towards a plateau"
      (is (= "m5" (:counted (:plateau (item)))))
      (is (= 3 (:stale (:plateau (item)))) "m2, m4 and m5 after the best, m1"))
    (testing "without --before, the base is the latest row before the spawn"
      (is (= "i1 measure echo t=$(wc -l < made)" (measure/item-measure s [id "echo t=$(wc -l < made)"])))
      (let [out (cli/spawn s *root* [id "--agent" "appends"])]
        (is (re-find #"^s2 i1 exit 0" out) out)
        (is (re-find #"\nm6 i1 t=4\(\+1\) · \d+\.\ds · vs m5\n" out) out))
      (is (re-matches #"m7 i1 t=4\(\+1\) · \d+\.\ds · vs m5" (measure/measure s [id]))))
    (testing ":before in a spec: checked, folded back, and started from"
      (is (= [[[:items 0 :before] "must be true or false, got 1"]]
             (spec/problems {:goal "g" :items [{:intent "x" :measure "m" :before 1}]})))
      (is (= [[[:items 0] ":runs, :drop, :higher and :before need a :measure"]]
             (spec/problems {:goal "g" :items [{:intent "x" :before true}]})))
      (is (= {:type :item/measuring :item/id "i1" :measure "m" :before true}
             (second (filter #(= "i1" (:item/id %)) (spec/events {:goal "g" :items [{:intent "x" :measure "m" :before true}]} "/r")))))
      (measure/item-measure s [id "echo t=1" "--before"])
      (is (= [{:intent "fewer lines" :gate "true" :measure "echo t=1" :before true}]
             (:items (spec/from-state (status/project (store/read-events s)))))))))

;; i71: dropped --revert puts the working copy back at the before row.
(def ^:private vcs-env
  {"GIT_AUTHOR_NAME" "t" "GIT_AUTHOR_EMAIL" "t@t" "GIT_COMMITTER_NAME" "t" "GIT_COMMITTER_EMAIL" "t@t"
   "JJ_USER" "t" "JJ_EMAIL" "t@t"})

(defn- vcs! [& cmd]
  (apply p/shell {:dir *root* :out :string :err :string :extra-env vcs-env} cmd))

(defn- made [] (slurp (str (fs/path *root* "made"))))

(defn- closed [s id] (last (filter #(and (= :item/closed (:type %)) (= id (:item/id %))) (store/read-events s))))

(defn- measured-spawn
  "An item measured once, then a spawn that appends to made: its before row is m1."
  [s]
  (let [id (cli/item-add s ["fewer lines" "--gate" "true"])]
    (measure/item-measure s [id "echo t=$(wc -l < made)"])
    (measure/measure s [id])
    (cli/spawn s *root* [id "--agent" "appends"])
    id))

(deftest dropped-revert-restores-the-jj-commit
  (spit (str (fs/path *root* "made")) "start\n")
  (vcs! "jj" "git" "init")
  (vcs! "jj" "commit" "-m" "start")
  (let [s (new-session "goal")]
    (testing "refused without a before row, and only with dropped"
      (let [id (cli/item-add s ["unspawned" "--gate" "true"])]
        (measure/item-measure s [id "echo t=1"])
        (measure/measure s [id])
        (is (thrown-with-msg? Exception #"i1 has no before row" (cli/item-close s [id "dropped" "--revert" "no"])))
        (is (thrown-with-msg? Exception #"--revert goes with dropped" (cli/item-close s [id "done" "--revert" "no"])))
        (is (= :open (get-in (status/project (store/read-events s)) [:items id :state])))))
    (let [id (measured-spawn s)
          c (:jj-commit (:env (first (filter #(= "m2" (:measure/id %)) (store/read-events s)))))]
      (is (= "start\nmore\n" (made)))
      (is (re-matches #"[0-9a-f]{12}" (str c)))
      (is (= (str id " dropped · restored jj " c " from m2") (cli/item-close s [id "dropped" "--revert" "slower"])))
      (is (= "start\n" (made)))
      (is (= {:disposition :dropped :why "slower" :measure/id "m2" :jj-commit c}
             (select-keys (closed s id) [:disposition :why :measure/id :jj-commit]))))))

(deftest dropped-revert-restores-the-git-commit
  (spit (str (fs/path *root* "made")) "start\n")
  (vcs! "git" "init" "-q")
  (vcs! "git" "add" "made")
  (vcs! "git" "commit" "-qm" "start")
  (let [s (new-session "goal")
        head (str/trim (:out (vcs! "git" "rev-parse" "--short" "HEAD")))]
    (testing "without --revert, dropped changes nothing"
      (let [id (measured-spawn s)]
        (is (= (str id " dropped") (cli/item-close s [id "dropped" "slower"])))
        (is (= "start\nmore\n" (made)))
        (is (nil? (:commit (closed s id))))))
    (testing "--revert restores the before row's commit, a committed change too"
      (let [id (measured-spawn s)]
        (vcs! "git" "commit" "-qam" "more")
        (is (= "start\nmore\nmore\n" (made)))
        (is (= (str id " dropped · restored git " head " from m3") (cli/item-close s [id "dropped" "--revert" "slower"])))
        (is (= "start\n" (made)))
        (is (= ["m3" head] ((juxt :measure/id :commit) (closed s id))))))
    (testing "a failed restore is refused and the item stays open"
      (let [id (measured-spawn s)]
        (fs/delete-tree (fs/path *root* ".git"))
        (vcs! "git" "init" "-q")
        (is (thrown-with-msg? Exception #"not reverted: git restore .* failed" (cli/item-close s [id "dropped" "--revert" "x"])))
        (is (= :open (get-in (status/project (store/read-events s)) [:items id :state])))))))

;; Plateaus: under cadence on plateau K, K rows without a new best ask the human.
(defn- plateau-session []
  (let [s (new-session "goal")]
    (policy/cadence-cmd s ["batch 5 on plateau 2"])
    (cli/item-add s ["work" "--gate" "false"])
    s))

(deftest plateau-counts-only-evidence
  (testing "failed rows and rows without numbers don't count toward a plateau"
    (let [s (plateau-session)
          t #(spit (str (fs/path *root* "t")) %)
          row #(measure/measure s ["i1"])
          plateau #(select-keys (get-in (status/project (store/read-events s)) [:items "i1" :plateau]) [:stale :best-id])]
      (measure/item-measure s ["i1" "cat t"])
      (row)
      (is (= {} (plateau)) "a failed first row starts nothing")
      (t "secs=5") (row)
      (t "secs=6") (row)
      (is (= {:stale 1 :best-id "m2"} (plateau)))
      (fs/delete (fs/path *root* "t"))
      (is (not (str/includes? (row) "ask")) "a failed row is no evidence")
      (t "no numbers") (row)
      (is (= {:stale 1 :best-id "m2"} (plateau)))
      (testing "a row whose number names differ from the best's starts over"
        (t "value=915454175") (row)
        (is (= {:stale 0 :best-id "m6"} (plateau)))
        (t "secs=9") (row)
        (is (= {:stale 0 :best-id "m7"} (plateau)) "secs isn't compared with the misparsed value"))
      (testing "the row that makes K asks, but a failed row after it doesn't ask again"
        (row)
        (is (str/includes? (row) "\nask a1 i1 plateaued: 2 measurement rows"))
        (cli/answer s ["a1" "keep going"])
        (fs/delete (fs/path *root* "t"))
        (is (not (str/includes? (row) "ask"))))
      (testing "a closed item opens no plateau ask"
        (t "secs=9") (row)
        (cli/item-close s ["i1" "dropped" "enough"])
        (is (not (str/includes? (row) "ask")))
        (is (= {:stale 4 :best-id "m7"} (plateau)))))))

(deftest plateau-higher-is-better
  (testing "--higher: a score's new best is a higher number, and rows mark it name↑"
    (let [s (plateau-session)
          row #(measure/measure s ["i1"])
          plateau #(select-keys (get-in (status/project (store/read-events s)) [:items "i1" :plateau]) [:stale :best-id])]
      (spit (str (fs/path *root* "t")) "score=5 ms=10")
      (is (= "i1 measure cat t higher score" (measure/item-measure s ["i1" "cat t" "--higher" "score"])))
      (is (re-find #"^m1 i1 ms=10 score↑=5 · " (row)))
      (spit (str (fs/path *root* "t")) "score=7 ms=10")
      (is (re-find #"^m2 i1 ms=10 score↑=7\(\+2\) · " (row)))
      (is (= {:stale 0 :best-id "m2"} (plateau)) "higher score is a new best")
      (spit (str (fs/path *root* "t")) "score=6 ms=11")
      (row)
      (is (re-find #"\nask a1 i1 plateaued: 2 measurement rows without a new best \(best m2, higher is better for score, lower for the rest\)"
                   (row)))
      (testing "a change of direction starts the count over"
        (measure/item-measure s ["i1" "cat t"])
        (row)
        (is (= {:stale 0 :best-id "m5"} (plateau)))))))
