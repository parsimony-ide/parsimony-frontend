(ns parsimony.com.input
  (:require-macros [re-com.core :refer [handler-fn]])
  (:require [dommy.core      :refer-macros [sel1]]
            [re-com.util     :refer [deref-or-value px]]
            [re-com.popover  :refer [popover-tooltip]]
            [re-com.box      :refer [h-box v-box box gap line flex-child-style align-style]]
            [re-com.validate :refer [input-status-type? input-status-types-list regex?
                                     string-or-hiccup? css-style? html-attr? number-or-string?
                                     string-or-atom? throbber-size? throbber-sizes-list] :refer-macros [validate-args-macro]]
            [re-com.misc     :refer [input-text-args-desc]]
            [reagent.core    :as    reagent]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A version of re-com input-text with customized blur and keypress behavior
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def args-desc
  (into input-text-args-desc
        [{:name :auto-focus :required false :default false :type "boolean"}
         {:name :auto-select :required false :default false :type "boolean"}
         {:name :on-blur :required false :type "string -> nil" :validate-fn fn?}
         {:name :on-key-up :required false :type "int -> nil" :validate-fn fn?}
         {:name :on-submit :required false :type "string -> nil" :validate-fn fn?}]))

(def ^:private autoselect
  (with-meta identity
    {:component-did-mount #(.select (reagent/dom-node %))}))

(defn- input-text-base
  "Returns markup for a basic text input label"
  [& {:keys [model input-type] :as args}]
  {:pre [(validate-args-macro args-desc args "input-text")]}
  (let [external-model (reagent/atom (deref-or-value model))  ;; Holds the last known external value of model, to detect external model changes
        internal-model (reagent/atom (if (nil? @external-model) "" @external-model))] ;; Create a new atom from the model to be used internally (avoid nil)
    (fn
      [& {:keys [model status status-icon? status-tooltip placeholder width height rows on-change change-on-blur? validation-regex disabled? class style attr
                 auto-focus auto-select on-blur on-key-up on-submit]
          :or   {change-on-blur? true
                 auto-focus false
                 auto-select false}
          :as   args}]
      {:pre [(validate-args-macro args-desc args "input-text")]}
      (let [latest-ext-model (deref-or-value model)
            disabled?        (deref-or-value disabled?)
            change-on-blur?  (deref-or-value change-on-blur?)
            showing?         (reagent/atom false)]
        (when (not= @external-model latest-ext-model) ;; Has model changed externally?
          (reset! external-model latest-ext-model)
          (reset! internal-model latest-ext-model))
        [h-box
         :align    :start
         :width    (if width width "250px")
         :class    "rc-input-text "
         :children [[:div
                     {:class (str "rc-input-text-inner "          ;; form-group
                                  (case status
                                    :warning "has-warning "
                                    :error "has-error "
                                    "")
                                  (when (and status status-icon?) "has-feedback"))
                      :style (flex-child-style "auto")}
                     [(if auto-select autoselect identity)
                      [input-type
                       (merge
                        {:class       (str "form-control " class)
                         :type        (when (= input-type :input) "text")
                         :rows        (when (= input-type :textarea) (if rows rows 3))
                         :style       (merge
                                       (flex-child-style "none")
                                       {:height        height
                                        :padding-right "12px"} ;; override for when icon exists
                                       style)
                         :placeholder placeholder
                         :value       @internal-model
                         :disabled    disabled?
                         :on-change   (handler-fn
                                       (let [new-val (-> event .-target .-value)]
                                         (when (and
                                                on-change
                                                (not disabled?)
                                                (if validation-regex (re-find validation-regex new-val) true))
                                           (reset! internal-model new-val)
                                           (when-not change-on-blur?
                                             (on-change @internal-model)))))
                         :on-blur     (handler-fn
                                       (when on-blur (on-blur @internal-model))
                                       (when (and
                                              on-change
                                              change-on-blur?
                                              (not= @internal-model @external-model))
                                         (on-change @internal-model)))
                         :on-key-up   (handler-fn
                                       (if disabled?
                                         (.preventDefault event)
                                         (do
                                           (when on-key-up (on-key-up (.-which event)))
                                           (case (.-which event)
                                             13 (do (when on-change (on-change @internal-model))
                                                    (when on-submit (on-submit @internal-model)))
                                             27 (reset! internal-model @external-model)
                                             true))))
                         :auto-focus auto-focus}
                        attr)]]]
                    (when (and status-icon? status)
                      (if status-tooltip
                        [popover-tooltip
                         :label status-tooltip
                         :position :right-center
                         :status status
                         ;:width    "200px"
                         :showing? showing?
                         :anchor [:i {:class         (str "zmdi " (if (= status :warning) "zmdi-alert-triangle" "zmdi-alert-circle") " form-control-feedback")
                                      :style         {:position "static"
                                                      :width    "auto"
                                                      :height   "auto"
                                                      :opacity  (if (and status-icon? status) "1" "0")}
                                      :on-mouse-over (handler-fn (when (and status-icon? status) (reset! showing? true)))
                                      :on-mouse-out  (handler-fn (reset! showing? false))}]
                         :style (merge (flex-child-style "none")
                                       (align-style :align-self :center)
                                       {:font-size   "130%"
                                        :margin-left "4px"})]
                        [:i {:class (str "zmdi " (if (= status :warning) "zmdi-alert-triangle" "zmdi-alert-circle") " form-control-feedback")
                             :style (merge (flex-child-style "none")
                                           (align-style :align-self :center)
                                           {:position    "static"
                                            :font-size   "130%"
                                            :margin-left "4px"
                                            :opacity     (if (and status-icon? status) "1" "0")
                                            :width       "auto"
                                            :height      "auto"})
                             :title status-tooltip}]))]]))))


(defn input-text
  [& args]
  (apply input-text-base :input-type :input args))


(defn input-textarea
  [& args]
  (apply input-text-base :input-type :textarea args))
