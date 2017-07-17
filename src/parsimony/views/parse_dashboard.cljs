(ns parsimony.views.parse-dashboard
  (:require [clojure.set :as set]
            [parsimony.models.parse-dashboard :refer [ positive-labels-for schema Sample Heading]]
            [parsimony.com.input :refer [input-text]]
            [parsimony.util :refer [pprint-str matches-schema?]]
            [parsimony.views.common :refer [icon]]
            [parsimony.views.editor :refer [editor]]
            [parsimony.views.parse-forest :as parse-forest]
            [parsimony.views.ribbon :refer [ribbon] :as ribbon]
            [parsimony.views.tree-view :refer [ITreeLabel] :as tree-view]
            [parsimony.views.tree-view.common :refer [label-and-icon delete-button]]
            [reagent.core :as reagent]
            [reagent.ratom :refer-macros [reaction]]
            [re-com.core :refer [alert-box box gap h-box h-split label line md-icon-button scroller throbber v-box v-split] :refer-macros [handler-fn]]
            [re-com.validate :refer-macros [validate-args-macro]]
            [re-frame.core :refer [dispatch subscribe]]
            [parsimony.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selection Preview
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_(defn- selection-preview
  [args]
  {:pre [(validate-args-macro selection-preview-args-desc args "selection-preview")]}
  (let [current-editor-id (subscribe [:current-editor])
        current-editor-selection (subscribe [:current-editor-selection])
        tokens (subscribe [:tokens] [current-editor-id])
        nt-str (reagent/atom "")]
    (fn [args]
      (if-let [{:keys [char-range string]} @current-editor-selection]
        (let [disabled? (empty? @nt-str)]
          [v-box
           :class "parse-selection-preview"
           :gap "5px"
           :children
           [[h-box
             :align :start
             :gap "5px"
             :children
             [[md-icon-button
               :md-icon-name "zmdi-plus"
               :class (str "positive-label-button " (when disabled? "disableme "))
               :size :regular
               :tooltip "Create a positive label"
               :tooltip-position :right-center
               :emphasise? true
               :disabled? disabled?
               :on-click (fn []
                           (let [[char-from char-to] (:char-range @current-editor-selection)]
                             (dispatch [:parse-dashboard/add-sample @nt-str @current-editor-id char-from char-to false])))]
              #_[md-icon-button
                 :md-icon-name "zmdi-minus"
                 :class (str "negative-label-button " (when disabled? "disableme "))
                 :size :regular
                 :tooltip "Create a negative label"
                 :tooltip-position :right-center
                 :emphasise? true
                 :disabled? disabled?
                 :on-click (fn []
                             (let [[char-from char-to] (:char-range @current-editor-selection)]
                               (dispatch [:parse-dashboard/add-sample @nt-str @current-editor-id char-from char-to true])))]
              [input-text
               :model nt-str
               :height "24px"
               :change-on-blur? false
               :on-submit (fn [nt-str]
                            (when-not disabled?
                              (let [[char-from char-to] (:char-range @current-editor-selection)]
                                (dispatch [:parse-dashboard/add-sample nt-str @current-editor-id char-from char-to false]))))
               :on-change #(reset! nt-str %)]]]
            [:pre
             {:style {:flex "auto"}}
             string]
            [line :size "1px" :color "#efefef"]]])
        [h-box
         :class "parse-selection-preview empty"
         :children []]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hover Buttons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- heading-hover-buttons [id hover editing?]
  [h-box
   :margin "0 0 0 15px"
   :children
   [[h-box
     :children
     [[delete-button hover editing? #(dispatch [:parse-dashboard/delete-all-samples])
       "Are you sure you want to remove all samples?"
       "Remove all samples"
       "320px"]]]]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree Element Labels
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: :pointer-events "none" prevents icon from interfering with parent
;; sample-icon-div's mouse-enter and mouse-leave events

(defn- check-icon []
  [icon {:md-icon-name "zmdi-check"
         :style {:color "green"
                 :flex "none"
                 :pointer-events "none"}}])

(defn- close-icon []
  [icon {:md-icon-name "zmdi-close"
         :style {:color "red"
                 :flex "none"
                 :pointer-events "none"}}])

(defn- delete-icon [on-click]
  [icon {:md-icon-name "zmdi-delete"
         :style {:color "red"
                 :flex "none"}
         :on-click on-click}])

(defn- code-icon []
  [icon {:md-icon-name "zmdi-code"
         :style {:flex "none"
                 :pointer-events "none"}}])

(defn- sample-icon-div [sample-id]
  (let [hover (reagent/atom nil)
        state (subscribe [:parse-dashboard])
        sample-status (reaction (get-in @state [:sample-status sample-id]))]
    (fn [sample-id]
      (let [positive-label-ids (map :label-id (positive-labels-for @state sample-id))
            all-positive-passing?
            (every? #(= :pass (get-in @sample-status [:label-status % :passing?]))
                    positive-label-ids)
            some-positive-failing?
            (some #(= :fail (get-in @sample-status [:label-status % :passing?]))
                    positive-label-ids)]
        #_(console/debug :sample-icon-div
                         {:sample-status @sample-status
                          :all-positive-passing? all-positive-passing?
                          :some-positive-failing? some-positive-failing?})
        [:div
         {:on-mouse-enter (handler-fn
                            (reset! hover true))
          :on-mouse-leave (handler-fn
                            (reset! hover nil))}
         (if @hover
           [delete-icon #(dispatch [:parse-dashboard-delete-sample sample-id])]
           (cond
             all-positive-passing?  [check-icon]
             some-positive-failing? [close-icon]
             :else [code-icon]))]))))

(defn sample-label-and-icon [sample-id string]
  (let [hover (reagent/atom nil)]
    (fn [sample-id string]
      [h-box
       :class "parse-sample-label"
       :size "auto"
       :align :baseline
       :style (when @hover
                {:background-color "#efefef"})
       :attr {:on-mouse-up-capture (handler-fn
                                     (reset! hover nil))
              :on-mouse-over (handler-fn
                               (reset! hover true))
              :on-mouse-out (handler-fn
                              (reset! hover nil))}
       :children [[sample-icon-div sample-id]
                  [gap :size "5px"]
                  [:pre
                   {:style {:flex "1 0 auto"}
                    :on-click (handler-fn
                                (dispatch [:parse-dashboard-select-sample sample-id]))}
                   string]]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ITreeLabel Instances
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IParseDashboardElement)

(extend-type Heading
  IParseDashboardElement
  ITreeLabel
  (tree-label [this]
    [box
     :child [label-and-icon {:id 0
                             :label-str "Samples"
                             :md-icon-name "zmdi-format-list-bulleted"
                             :hover-buttons heading-hover-buttons}]]))

(extend-type Sample
  IParseDashboardElement
  ITreeLabel
  (tree-label [this]
    [box :child [sample-label-and-icon (:sample-id this) (:source-path this)]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Validation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn valid-drop-fn
  "Validation function that only allows drops between elements satisfying the IParseDashboardElement protocol."
  [data parent _]
  (when-let [tree-elem (get data ::element)]
    (let [res (vector (matches-schema? tree-view/element-schema tree-elem)
                      (matches-schema? tree-view/element-schema parent)
                      (satisfies? IParseDashboardElement (:data tree-elem))
                      (satisfies? IParseDashboardElement (:data parent)))]
      (if (every? identity res)
        true
        (do (console/warn :invalid-drop res)
            false)))))

(defn valid-drag-type-fn
  [drag-types _ _]
  (seq (set/intersection drag-types #{::element})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sample Picker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sample-picker
  [args]
  ;; XXX: basically identical to token-dashboard/sample-picker
  (let [parse-dashboard-state (subscribe [:parse-dashboard])]
    (fn [args]
      [v-box
       :size "auto"
       :class "parse-sample-picker"
       :children [[scroller
                   :h-scroll :off
                   :child [tree-view/tree-parent
                           {:model (:samples @parse-dashboard-state)
                            :drag-type ::element
                            :valid-drop-fn valid-drop-fn
                            :valid-drag-type-fn valid-drag-type-fn
                            :on-move #(dispatch [:parse-dashboard-move (:id %1) (:id %2) %3])}]]]
       ])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Detail Pane
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sample-view
  [{:keys [sample] :as args}]
  (let [editor-id (subscribe [:editor-for-sample (:sample-id sample)])]
    (fn [args]
      [editor editor-id])))

(defn- minus-icon []
  [icon {:md-icon-name "zmdi-minus"
         :style {:flex "none"
                 :pointer-events "none"}}])

(defn- plus-icon []
  [icon {:md-icon-name "zmdi-plus"
         :style {:flex "none"
                 :pointer-events "none"}}])

(defn- label-icon-div [sample-id label-id type hover]
  (let [state (subscribe [:parse-dashboard])]
    (fn [sample-id label-id type hover]
      (let [passing? (get-in @state [:sample-status sample-id :label-status label-id :passing?])]
        [:div
         {:style {:flex "none"}
          :on-mouse-enter (handler-fn
                            (reset! hover label-id))
          :on-mouse-leave (handler-fn
                            (reset! hover nil))}
         (if (= @hover label-id)
           [delete-icon (fn []
                          (reset! hover nil)
                          (dispatch [:parse-dashboard-delete-label sample-id label-id]))]
           (cond
             (and (= :positive type)
                  (= :pass passing?)) [check-icon]
             (and (= :positive type)
                  (= :fail passing?)) [close-icon]
             :else
             (case type
               :positive [plus-icon]
               :negative [minus-icon])))]))))

(defn- one-label [sample -label icon-hover]
  (let [state (subscribe [:parse-dashboard])]
    (fn [{:keys [sample-id] :as sample}
         {:keys [nt label-id type] :as -label}]
      (let [selected-sample-id (get-in @state [:selected :sample-id])
            selected-label-id (get-in @state [:selected :label-id])
            active? (and (= sample-id selected-sample-id)
                         (= label-id selected-label-id))]
        [h-box
         :class (str "parse-label "
                     (if active? "active" ""))
         :align :baseline
         :children [[label-icon-div sample-id label-id type icon-hover]
                    [gap :size "5px"]
                    [:div
                     {:style {:flex "auto"}
                      :on-mouse-leave (handler-fn
                                        (dispatch [:parse-dashboard-unpeek-all-labels sample-id]))
                      :on-mouse-enter (handler-fn
                                        (dispatch [:parse-dashboard-peek-label sample-id label-id]))
                      ; :on-click (handler-fn
                      ;             (dispatch [:parse-dashboard-select-label sample-id label-id]))
                      }
                    [label :label (name nt)]]]]))))

(defn label-picker
  "All labels for the given sample in a list"
  [sample]
  (let [icon-hover (reagent/atom nil)]
    (fn [sample]
      [v-box
       :size "auto"
       :class "parse-label-picker"
       :children [[scroller
                   :h-scroll :off
                   :child [v-box
                           :size "auto"
                           :children (into []
                                           (for [l (sort-by :label-id (:labels sample))]
                                             ^{:key (:label-id l)}
                                             [one-label sample l icon-hover]))]]]])))

(defn forest-view
  [args]
  (let [decoration-map (subscribe [:affinity-decoration-map])]
    (fn forest-view [{:keys [forest tokens string] :as args}]
      [v-box
       :class "forest-view"
       :size "auto"
       :children
       [[parse-forest/parse-forest-view
         {:forest forest
          :tokens tokens
          :string string
          :decoration-map @decoration-map
          :disabled-overlays #{[:tokens "ws"]}
          :peek nil}]]])))

(defn custom-alert-box [{:keys [alert-type md-icon-name style heading body]}]
  (into [alert-box
         :style (merge {:width "100%"
                        :margin "0px"
                        :border-radius "0px"
                        :border-width "0px"}
                       style)
         :alert-type alert-type
         :heading [h-box
                   :margin "0px"
                   :gap "5px"
                   :children [[icon {:md-icon-name md-icon-name}]
                              (if (string? heading)
                                [:span heading]
                                heading)]]]
        (when body [:body body])))

(defn pass-alert-box [{:keys [heading body]}]
  [custom-alert-box {:alert-type :info
                     :md-icon-name "zmdi-check"
                     :heading heading
                     :body body}])

(defn fail-alert-box [{:keys [heading body]}]
  [custom-alert-box {:alert-type :danger
                     :md-icon-name "zmdi-close"
                     :heading heading
                     :body body}])

(defn unknown-alert-box [{:keys [heading body]}]
  [custom-alert-box {:alert-type :none
                     :md-icon-name "zmdi-more-horiz"
                     :style {:color "#bbb"
                             :background-color "#eee"}
                     :heading heading
                     :body body}])

(defn failure-explanation [{:keys [positive-failures negative-failures forest] :as args}]
  (cond
    (not (seq forest))
    [fail-alert-box {:heading [:span "Fail"]
                     :body [:span "The current grammar does not produce any
                                   parse tree for the labelled string."]}]

    (seq positive-failures)
    [fail-alert-box {:heading [:span "Fail"]
                     :body [:span "Although the current grammar produces the following parse tree,
                                   the parse tree is missing expected nodes."]}]

    (seq negative-failures)
    [fail-alert-box {:heading [:span "Fail"]
                     :body [:span "Although the current grammar produces the following parse tree,
                                   the parse tree contains disallowed nodes."]}]

    :else
    [fail-alert-box {:heading [:span "Fail"]
                     :body [:pre (pprint-str args)]}]))

(defn element-detail-status-view
  [{:keys [passing? positive-failures negative-failures forest] :as args}]
  (cond
    (= :pass passing?)
    [pass-alert-box {:heading [:span "Pass"]}]

    (= :fail passing?)
    [failure-explanation (select-keys args [:positive-failures :negative-failures :forest])]

    :else
    [unknown-alert-box {:heading [:span "Unchecked"]}]))

(defn element-detail-view
  [{:keys [forest passing? tokens string] :as args}]
  [v-box
   :size "auto"
   :children
   [[element-detail-status-view args]
    #_[:pre (pprint-str (select-keys args [:forest :positive-failures :negative-failures :tokens :string]))]
    [forest-view {:forest forest :tokens tokens :string string}]]])

(defn element-view
  [args]
  (let [state (subscribe [:parse-dashboard])]
    (fn [{:keys [element visible?] :as args}]
      (let [{selected-sample-id :sample-id selected-label-id :label-id} (:selected @state)
            sample (:data element)
            split-val (if selected-label-id
                        (get-in @state [:layout :top])
                        100)]
        [scroller
         :style (when-not visible?
                  {:display "none"})
         :child [v-box
                 :size "auto"
                 :children [[h-split
                             :margin "0"
                             :initial-split (get-in @state [:layout :center])
                             :on-split-change #(dispatch [:parse-dashboard-change-layout {:center %}])
                             :panel-1 [label-picker (:data element)]
                             :panel-2
                             ^{:key split-val} ;; XXX: hack to force h-split to redraw on split-val change
                             [h-split
                              :margin "0"
                              :initial-split split-val
                              :on-split-change #(dispatch [:parse-dashboard-change-layout {:top %}])
                              :panel-1 [box
                                        :size "auto"
                                        :style {:border-right "1px solid #efefef"}
                                        :child [sample-view {:sample sample}]]
                              :panel-2
                              (if (and selected-sample-id selected-label-id)
                                (let [tokens (get-in @state [:sample-status selected-sample-id :tokens])
                                      string (:string sample)
                                      label-status (get-in @state [:sample-status selected-sample-id
                                                                   :label-status selected-label-id])]
                                  (if (and (seq tokens)
                                           (seq string))
                                    [scroller
                                     :style {:border-left "1px solid #efefef"}
                                     :child [element-detail-view (merge label-status {:tokens tokens
                                                                                      :string (:string sample)})]]
                                    [:div]))
                                [:div])]]]]]))))

(defn detail-pane
  [args]
  ;; XXX: basically identical to token-dashboard/detail-pane
  (let [dashboard-state (subscribe [:parse-dashboard])]
    (fn [args]
      (let [{:keys [samples]} @dashboard-state
            selected-id (first (tree-view/get-selected-element-ids samples))
            selected-element (when selected-id (tree-view/get-element samples selected-id))]
        [v-box
         :size "auto"
         :class "parse-detail-pane"
         :children (into []
                         (for [s samples]
                           ^{:key (:id s)}
                           [element-view
                            {:element s
                             :visible? (= s selected-element)}]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Component
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def parse-dashboard-args-desc [])

(defn parse-dashboard [args]
  {:pre [(validate-args-macro parse-dashboard-args-desc args "parse-dashboard")]}
  (let [parse-dashboard-state (subscribe [:parse-dashboard])]
    [v-box
     :class "parse-dashboard"
     :size "auto"
     :children [#_[ribbon {:model
                         [(ribbon/command-item :run-solver {:image-url "url(assets/persimmon.svg)"})
                          (ribbon/command-item :run-check {:md-icon-name "zmdi-assignment-check"})
                          (ribbon/button-item {:text "Clear Checks"
                                               :md-icon-name "zmdi-format-color-reset"
                                               :on-click #(dispatch [:parse-dashboard-clear])})]}]
                [v-box
                 :size "auto"
                 :children [[h-split
                             :margin "0"
                             :initial-split (get-in @parse-dashboard-state [:layout :left])
                             :on-split-change #(dispatch [:parse-dashboard-change-layout {:left %}])
                             :panel-1 [v-box
                                       :size "auto"
                                       :children [#_[selection-preview]
                                                  [sample-picker]]]
                             :panel-2 [detail-pane]]]]]]))
