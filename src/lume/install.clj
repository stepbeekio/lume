(ns lume.install
  "Install: set up the agent side of this machine. Safe to re-run; writes only
  what differs and nothing inside any repo."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [lume.kernel :refer [*default-agent*]]))

(defn install [[a b]]
  (let [agent (keyword (if (= "--agent" a) b *default-agent*))
        {:keys [files manual]} ((requiring-resolve 'lume.agents/setup) agent)]
    (str/join "\n"
              (concat
               (for [[path content] files]
                 (let [old (when (fs/exists? path) (slurp path))]
                   (if (= old content)
                     (str "unchanged " path)
                     (do (fs/create-dirs (fs/parent path))
                         (spit path content)
                         (str (if old "updated   " "wrote     ") path)))))
               manual))))

(def feature
  {:name "install"
   :commands [{:path ["install"] :needs :none :usage "usage: lume install [--agent claude|claude-acp|devin]"
               :run (fn [_ args] (install args))
               :help "lume install [--agent A]                    set up agents on this machine (a lume skill); safe to re-run.
                                            Agents: claude, claude-acp, devin; LUME_AGENT sets the default (claude)"}]})
