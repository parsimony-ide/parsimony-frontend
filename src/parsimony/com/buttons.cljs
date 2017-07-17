(ns parsimony.com.buttons
  (:require-macros [re-com.core :refer [handler-fn]])
  (:require [re-com.util     :refer [deref-or-value px]]
            [re-com.validate :refer [position? position-options-list button-size? button-sizes-list
                                     string-or-hiccup? css-style? html-attr? string-or-atom?] :refer-macros [validate-args-macro]]
            [re-com.popover  :refer [popover-tooltip]]
            [re-com.box      :refer [h-box v-box box gap line flex-child-style]]
            [re-com.buttons  :refer [row-button-args-desc button-args-desc]]
            [reagent.core    :as    reagent]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A version of the re-com row-button that stops propagation on click
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn row-button
  "a small button containing a material design icon"
  []
  (let [showing? (reagent/atom false)]
    (fn
      [& {:keys [md-icon-name on-click mouse-over-row? tooltip tooltip-position disabled? class style attr]
          :or   {md-icon-name "zmdi-plus"}
          :as   args}]
      {:pre [(validate-args-macro row-button-args-desc args "row-button")]}
      (let [the-button [:div
                        (merge
                          {:class    (str
                                       "rc-row-button noselect "
                                       (when mouse-over-row? "rc-row-mouse-over-row ")
                                       (when disabled? "rc-row-disabled ")
                                       class)
                           :style    style
                           :on-click (handler-fn
                                       (when (and on-click (not disabled?))
                                         (on-click)
                                         (.stopPropagation event)))}
                          (when tooltip
                            {:on-mouse-over (handler-fn (reset! showing? true))
                             :on-mouse-out  (handler-fn (reset! showing? false))}) ;; Need to return true to ALLOW default events to be performed
                          attr)
                        [:i {:class (str "zmdi zmdi-hc-fw-rc " md-icon-name)}]]]
        (if tooltip
          [popover-tooltip
           :label    tooltip
           :position (if tooltip-position tooltip-position :below-center)
           :showing? showing?
           :anchor   the-button]
          the-button)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A version of the re-com button that stops propagation on click
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn button
  "Returns the markup for a basic button"
  []
  (let [showing? (reagent/atom false)]
    (fn
      [& {:keys [label on-click tooltip tooltip-position disabled? class style attr]
          :or   {class "btn-default"}
          :as   args}]
      {:pre [(validate-args-macro button-args-desc args "button")]}
      (let [disabled? (deref-or-value disabled?)
            the-button [:button
                        (merge
                          {:class    (str "rc-button btn " class)
                           :style    (merge
                                       (flex-child-style "none")
                                       style)
                           :disabled disabled?
                           :on-click (handler-fn
                                       (when (and on-click (not disabled?))
                                         (on-click)
                                         (.stopPropagation event)))}
                          (when tooltip
                            {:on-mouse-over (handler-fn (reset! showing? true))
                             :on-mouse-out  (handler-fn (reset! showing? false))})
                          attr)
                        label]]
        [box
         :class "display-inline-flex"
         :align :start
         :child (if tooltip
                  [popover-tooltip
                   :label    tooltip
                   :position (if tooltip-position tooltip-position :below-center)
                   :showing? showing?
                   :anchor   the-button]
                  the-button)]))))
