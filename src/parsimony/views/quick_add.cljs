(ns parsimony.views.quick-add
  (:require [parsimony.com.input :refer [input-text]]
            [parsimony.models.parse-dashboard :as parse-dashboard]
            [parsimony.views.common :refer [icon]]
            [re-com.core :refer [gap h-box v-box label md-icon-button popover-tooltip] :refer-macros [handler-fn]]
            [re-frame.core :refer [dispatch subscribe]]
            [reagent.core :as reagent]
            [reagent.ratom :refer [reaction]]))

(defn- solve-button [args]
  (let [command-enabled? (subscribe [:command-enabled? :run-solver nil])
        hover (reagent/atom nil)]
    (fn [{:keys [on-click tooltip complete-input?]}]
      (let [enabled? (or @command-enabled? complete-input?)
            the-button
            [h-box
             :class (str "solve-button "
                         (when (not enabled?)
                           "disableme "))
             :attr {:on-click (handler-fn (on-click))
                    :on-mouse-over (handler-fn (reset! hover true))
                    :on-mouse-out (handler-fn (reset! hover nil))}
             :justify :center
             :align :center
             :gap "5px"
             :height "24px"
             :children
             [[:span {:style {:position "relative"
                              :width "20px"
                              :height "20px"}}
               [:span
                {:style
                 (merge {:width "20px"
                         :height "20px"
                         :background-image "url(assets/persimmon.svg)"
                         :background-size "100% 100%"
                         :position "absolute"}
                        (when (not enabled?)
                          {:filter "grayscale(70%) brightness(130%)"}))}]
               (when complete-input?
                 [icon {:md-icon-name "zmdi-plus"
                        :style {:color "black"
                                :font-size "120%"
                                :font-weight "bold"
                                :position "absolute"
                                :left "-5px"
                                :bottom "-3px"}}])]
              [label :label "Solve!"]]]]
        [popover-tooltip
         :label tooltip
         :position :below-center
         :showing? hover
         :anchor the-button]))))

(defn quick-add-bar []
  (let [current-editor-id (subscribe [:current-editor])
        current-editor-selection (subscribe [:current-editor-selection])
        nt-str (reagent/atom "")]
    (fn []
      (let [{:keys [char-range string] :as selection} @current-editor-selection
            complete-input? (and (seq @nt-str) (some? selection))
            add-sample-fn
            (fn [nt-str]
              (let [[char-from char-to] (:char-range @current-editor-selection)]
                (dispatch [:parse-dashboard/add-sample
                           nt-str
                           @current-editor-id
                           char-from
                           char-to
                           false])))
            run-solver-fn #(dispatch [:exec-command-from-menu :run-solver])]
        [h-box
         :class "quick-add-bar"
         :align :start
         :gap "5px"
         :children [(when (some? selection)
                      [input-text
                       :model nt-str
                       :width "150px"
                       :height "24px"
                       :change-on-blur? false
                       :on-submit
                       (fn [nt-str]
                         (when complete-input?
                           (add-sample-fn nt-str)
                           (run-solver-fn)))
                       :on-change #(reset! nt-str %)])
                    [solve-button
                     {:complete-input? complete-input?
                      :on-click
                      (fn []
                        (when complete-input?
                          (add-sample-fn @nt-str))
                        (run-solver-fn))
                      :tooltip
                      (if complete-input?
                        [v-box
                         :children
                         [[h-box
                              :gap "3px"
                              :children [[:span "1. Add parse label"]
                                         [:code @nt-str]
                                         [:span "for selected text:"]]]
                          [:pre.sample-text string]
                          [gap :size "3px"]
                          [label :label "2. Then run the solver."]]]
                        [label :label "Run the solver"])}]]]))))
