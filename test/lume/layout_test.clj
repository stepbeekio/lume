(ns lume.layout-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]))

(defn- ns-form
  "The ns form at the top of a source file."
  [f]
  (with-open [r (java.io.PushbackReader. (clojure.java.io/reader (fs/file f)))]
    (read r)))

(defn- requires
  "The namespaces an ns form's :require names."
  [form]
  (set (for [clause (drop 2 form)
             :when (and (seq? clause) (= :require (first clause)))
             spec (rest clause)]
         (if (sequential? spec) (first spec) spec))))

;; Features are built on the kernel's surface. lume.cli is only -main, so a
;; feature that required it would have nothing to gain but a cycle.
(deftest features-require-the-kernel-not-the-cli
  (let [forms (into {} (for [f (fs/glob "src/lume" "*.clj")
                             :let [form (ns-form f)]]
                         [(second form) form]))]
    (is (contains? forms 'lume.cli))
    (doseq [[ns form] forms
            :when (not= 'lume.cli ns)]
      (testing (str ns)
        (is (not (contains? (requires form) 'lume.cli)) "requires lume.cli")
        (when-not ('#{lume.kernel lume.store lume.features} ns)
          (is (contains? (requires form) 'lume.kernel) "doesn't require lume.kernel"))))))
