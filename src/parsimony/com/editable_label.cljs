(ns parsimony.com.editable-label
  (:require [parsimony.com.input :as input :refer [input-text]]
            [re-com.core :refer [label]]
            [re-com.util :refer [deref-or-value]]
            [re-com.validate :refer-macros [validate-args-macro]]
            [reagent.core :as reagent]))

(def editable-label-args-desc
  (into input/args-desc
        [{:name :editing? :required true :default false :type "boolean | atom"}]))

(defn editable-label
  "A label that becomes an in-place editor when its editing? field is true"
  [args]
  {:pre [(validate-args-macro editable-label-args-desc args "editable-label")]}
  (fn [{:keys [model height width editing? auto-focus auto-select on-blur on-change on-key-up style] :as args}]
    {:pre [(validate-args-macro editable-label-args-desc args "editable-label")]}
    (if-not (deref-or-value editing?)
      [label :label (deref-or-value model)]
      [input-text
       :model model
       :on-change (when on-change on-change)
       :change-on-blur? true
       :auto-focus true
       :auto-select true
       :on-blur (when on-blur on-blur)
       :on-key-up (when on-key-up on-key-up)
       :height (if height height "inherit")
       :width (if width width "inherit")
       :style (when style style)])))
