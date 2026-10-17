(ns lume.features
  "The built-in features, in order. Their folds run in this order, and lume
  help lists their commands in it. lume.cli binds kernel/*features* to them."
  (:require [lume.agent :as agent]
            [lume.ask :as ask]
            [lume.clean :as clean]
            [lume.drive :as drive]
            [lume.inspect :as inspect]
            [lume.install :as install]
            [lume.item :as item]
            [lume.measure :as measure]
            [lume.note :as note]
            [lume.ntfy :as ntfy]
            [lume.policy :as policy]
            [lume.rule :as rule]
            [lume.session :as session]
            [lume.spec :as spec]))

(def all
  [install/feature session/feature item/feature ask/feature note/feature measure/feature agent/feature
   policy/budget-feature policy/cadence-feature drive/feature ntfy/feature rule/feature spec/feature inspect/feature
   clean/feature])
