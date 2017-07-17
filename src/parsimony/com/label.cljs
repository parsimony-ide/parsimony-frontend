(ns parsimony.com.label
  (:require [re-com.box :refer [box flex-child-style]]
            [re-com.validate :refer [css-style? html-attr?] :refer-macros [validate-args-macro]])
  (:require-macros [re-com.core :refer [handler-fn]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A version of the re-com label with the :size option
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def label-args-desc
    [{:name :label    :required true                  :type "anything"                              :description "text or hiccup or whatever to display"}
     {:name :on-click :required false                 :type "-> nil"        :validate-fn fn?        :description "a function which takes no params and returns nothing. Called when the label is clicked"}
     {:name :size     :required false :default "none" :type "string"        :validate-fn string?    :description [:span "equivalent to CSS style " [:span.bold "flex"] "." [:br]  "Examples: " [:code "initial"] ", " [:code "auto"] ", " [:code "none"]", " [:code "100px"] ", " [:code "2"] " or a generic triple of " [:code "grow shrink basis"]]}
     {:name :width    :required false                 :type "string"        :validate-fn string?    :description "a CSS width"}
     {:name :class    :required false                 :type "string"        :validate-fn string?    :description "CSS class names, space separated"}
     {:name :style    :required false                 :type "CSS style map" :validate-fn css-style? :description "additional CSS styles"}
     {:name :attr     :required false                 :type "HTML attr map" :validate-fn html-attr? :description [:span "HTML attributes, like " [:code ":on-mouse-move"] [:br] "No " [:code ":class"] " or " [:code ":style"] "allowed"]}])

(defn label
  "Returns markup for a basic label"
  [& {:keys [label on-click size width class style attr]
      :or   {size "none"}
      :as   args}]
  {:pre [(validate-args-macro label-args-desc args "label")]}
  [box
   :size size
   :width width
   :align :start
   :class "display-inline-flex"
   :child [:span
           (merge
             {:class (str "rc-label " class)
              :style (merge (flex-child-style size)
                            style)}
             (when on-click
               {:on-click (handler-fn (on-click))})
             attr)
           label]])

