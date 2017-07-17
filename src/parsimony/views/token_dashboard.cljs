(ns parsimony.views.token-dashboard
  "The visible component for interacting with the token learner"
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [parsimony.com.buttons :refer [button]]
            [parsimony.com.editable-label :refer [editable-label]]
            [parsimony.models.token-dashboard :refer [schema Sample Heading ITokenDashboardElement ITokenCategory] :as token-dashboard]
            [parsimony.util :refer [matches-schema?]]
            [parsimony.views.common :refer [icon]]
            [parsimony.views.tree-view.common :refer [label-and-icon add-heading-button delete-button rename-button]]
            [parsimony.views.tree-view :refer [ITreeLabel] :as tree-view]
            [reagent.core :as reagent]
            [reagent.ratom :as ratom :refer-macros [reaction run!]]
            [re-com.core :refer [box scroller h-box h-split label md-icon-button v-box p title line gap throbber row-button] :refer-macros [handler-fn]]
            [re-com.validate :refer-macros [validate-args-macro]]
            [re-frame.core :refer [dispatch subscribe]]
            [schema.core :as s :include-macros true]
            [parsimony.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selection Preview
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def selection-preview-args-desc [])

(defn selection-preview
  [args]
  (let [current-editor-selection (subscribe [:current-editor-selection])]
    (fn [{:keys [samples] :as args}]
      (if-let [{:keys [string]} @current-editor-selection]
        (let [already-exists (contains? (set (map :string samples)) string)]
          [v-box
           :class "token-selection-preview"
           :gap "5px"
           :children
           [[h-box
             :align :start
             :gap "5px"
             :children [[md-icon-button
                         :md-icon-name "zmdi-plus"
                         :size :regular
                         :tooltip (if already-exists
                                    "This string already exists in the sample set"
                                    "Add this string to the sample set")
                         :tooltip-position :right-center
                         :emphasise? (not already-exists)
                         :disabled? already-exists
                         :on-click #(dispatch [:add-token-sample string])]
                        [label :label [:pre (str string)]]]]
            [line :size "1px" :color "#efefef"]]])
        [h-box
         :class "token-selection-preview empty"
         :children []]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hover Buttons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- heading-hover-buttons [id hover editing?]
  [h-box
   :margin "0 0 0 15px"
   :attr {:on-click (handler-fn (.stopPropagation event))}
   :children
   [[h-box
     :children
     [#_[add-heading-button hover editing? #(dispatch [:add-token-category id])]
      [rename-button hover editing? nil]
      [gap :size "5px"]
      [delete-button hover editing? #(dispatch [:delete-token-category id])]]]]])

(defn static-heading-hover-buttons [id hover _]
  (let [editing? (reagent/atom false)]
    (fn [id hover]
      [h-box
       :margin "0 0 0 15px"
       :attr {:on-click (handler-fn (.stopPropagation event))}
       :children
       [[h-box
         :children
         [[add-heading-button hover editing? #(dispatch [:add-token-category id])]
          [gap :size "5px"]
          [delete-button hover editing? #(dispatch [:token-dashboard/delete-all-samples])
           "Are you sure you want to remove all samples?"
           "Remove all samples"
           "320px"]]]]])))

(defn sample-hover-buttons [id hover _]
  (let [editing? (reagent/atom false)]
    (fn [id hover]
      [h-box
       :margin "0 0 0 15px"
       :attr {:on-click (handler-fn (.stopPropagation event))}
       :children
       [[h-box
         :children
         [[delete-button hover editing? #(dispatch [:delete-token-sample id])]]]]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree Element Labels
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn heading-label-and-icon [id label-str md-icon-name]
  [label-and-icon {:id id
                   :label-str label-str
                   :md-icon-name md-icon-name
                   :hover-buttons heading-hover-buttons
                   :on-click #(dispatch [:token-dashboard-select id])
                   :on-edit-in-place #(dispatch [:rename-token-category %1 %2])}])

(defn static-heading-label-and-icon [id label-str md-icon-name]
  [label-and-icon {:id id
                   :label-str label-str
                   :md-icon-name md-icon-name
                   :hover-buttons static-heading-hover-buttons
                   ;; :on-click #(dispatch [:token-dashboard-select id])
                   }])

(defn sample-label-and-icon [id string horizon]
  (let [delete-hover (reagent/atom nil)
        hover (reagent/atom nil)]
    (fn [id string horizon]
      [h-box
       :class "token-sample-label"
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
       :children [[box
                   :attr {:on-mouse-enter (handler-fn
                                            (reset! delete-hover true))
                          :on-mouse-leave (handler-fn
                                            (reset! delete-hover nil))
                          :on-click
                          (handler-fn
                            (when @delete-hover
                              (.stopPropagation event)
                              (dispatch [:delete-token-sample id])))}
                   :child
                   (if @delete-hover
                     [icon {:md-icon-name "zmdi-delete"
                            :style {:color "red"
                                    :flex "none"}}]
                     (if horizon
                       [icon {:md-icon-name "zmdi-check"
                              :style {:color "green"
                                      :flex "none"}}]
                       [throbber
                        :size :small
                        :style {:margin "0px"}]))]
                  [gap :size "5px"]
                  [:pre
                   {:style {:flex "1 0 auto"}
                    :on-click (handler-fn
                                (dispatch [:token-dashboard-select id]))}
                   string]]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ITreeLabel Instances
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(extend-type Heading
  ITokenDashboardElement
  ITokenCategory
  ITreeLabel
  (tree-label [{:keys [id label-str md-icon-name editable?] :as this}]
    [box
     :class "token-sample-heading"
     :child (if editable?
              [heading-label-and-icon id label-str md-icon-name]
              [static-heading-label-and-icon id label-str md-icon-name])]))

(extend-type Sample
  ITokenDashboardElement
  ITreeLabel
  (tree-label [{:keys [id string horizon] :as this}]
    [h-box
     :size "auto"
     :class "token-sample"
     :children [[sample-label-and-icon id string horizon]]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Validation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn valid-drop-fn
  "Validation function that only allows drops between elements satisfying the ITokenDashboardElement protocol."
  [data parent _]
  (when-let [tree-elem (get data ::element)]
    (let [res (vector (matches-schema? tree-view/element-schema tree-elem)
                      (matches-schema? tree-view/element-schema parent)
                      (satisfies? ITokenDashboardElement (:data tree-elem))
                      (satisfies? ITokenDashboardElement (:data parent)))]
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
  "The left side of the dashboard"
  [args]
  (let [token-dashboard-state (subscribe [:token-dashboard])]
    (fn [args]
      [v-box
       :size "auto"
       :class "token-sample-picker"
       :attr {:on-click
              (handler-fn
                (dispatch [:token-dashboard-deselect-all]))}
       :children [[scroller
                   :h-scroll :off
                   :child [tree-view/tree-parent
                           {:model (:samples @token-dashboard-state)
                            :drag-type ::element
                            :valid-drop-fn valid-drop-fn
                            :valid-drag-type-fn valid-drag-type-fn
                            :on-toggle #(dispatch [:token-dashboard-toggle (:id %)])
                            :on-move   #(dispatch [:token-dashboard-move (:id %1) (:id %2) %3])
                            :attr {:on-click (handler-fn (.stopPropagation event))}}]]]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Detail Pane
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn examples-view
  [id path]
  (let [example-strings (subscribe [:example-strings id path])]
   (fn [id path]
     [v-box
      :class "token-examples-view"
      :gap "2px"
      :children
      [[h-box
        :align :center
        :gap "5px"
        :children [[label :label "Example Strings"]
                   [icon {:md-icon-name "zmdi-refresh"
                          :style {:cursor "pointer"}
                          :tooltip "Click to generate new example strings"
                          :tooltip-position :right-center
                          :on-click #(dispatch [:fetch-token-example-strings id path])}]]]
       (into [:div {:style {:display "flex"
                            :flex-flow "row wrap"}}]
             (interpose [gap :size "4px"])
             (for [string @example-strings]
               [:pre.example-string string]))]])))

(defn- accept-button [id path rule-name-str]
  (let [hover (reagent/atom nil)]
    (fn [id path rule-name-str]
      [h-box
       :class (str "accept-button "
                   (when @hover "hoverme "))
       :align :baseline
       :align-self :start
       :gap "4px"
       :attr {:on-mouse-over (handler-fn
                               (reset! hover true))
              :on-mouse-out (handler-fn
                              (reset! hover nil))
              :on-click (handler-fn
                          (dispatch [:token-dashboard/accept path rule-name-str]))}
       :children [[icon {:md-icon-name "zmdi-format-valign-bottom"}]
                  [label :label "Add to token definitions"]]])))

(defn path-view
  [id path]
  ;; note that this subscription does NOT change on changes to the path prop after this component has been mounted
  (let [token-package (subscribe [:token-package-at path])
        dashboard-state (subscribe [:token-dashboard])
        [_ token-kw] path
        rule-name-str (reagent/atom (name token-kw))
        rule-name-changed? (reaction (not= @rule-name-str (name token-kw)))
        editing? (reagent/atom false)]
    (fn [id [lang token-kw]]
      (let [category-label (token-dashboard/get-category-label @dashboard-state id)
            model (cond
                    @rule-name-changed? @rule-name-str
                    (seq category-label) category-label
                    :else @rule-name-str)
            {:keys [source-str]} @token-package
            rhs (token-dashboard/source-str-rhs source-str)
            editable-lhs
            [box
             :style {:cursor "pointer"}
             :attr {:on-double-click #(reset! editing? true)}
             :child
             [editable-label
              {:model model
               :editing? editing?
               :on-change (fn [s]
                            (let [s (str/trim s)]
                              (if (seq s)
                                (reset! rule-name-str s)
                                (reset! rule-name-str (name token-kw))))
                            (reset! editing? false))
               :on-blur #(reset! editing? false)
               :on-key-up (fn [keycode]
                            (when (= keycode 27) ;; keycode for <Esc>
                              (reset! editing? false)))
               :height "inherit"
               :width "inherit"
               :style {:padding "0"
                       :padding-right "0"}}]]]
        [v-box
         :class "token-path-view"
         :gap "2px"
         :children
         [[h-box
           :align :center
           :class "token-definition"
           :children [editable-lhs
                      [gap :size "5px"]
                      [:span "="]
                      [:pre rhs]]]
          [:div {:class "token-definition-lang"} lang]
          [examples-view id path]
          [accept-button id path model]]]))))

(defn horizon-view
  [id horizon]
  [v-box
   :class "token-horizon-view"
   :gap "5px"
   :children
   (into []
         (map (fn [p]
                ^{:key (str p)}
                [path-view id p]))
         horizon)])

(defn sample-view [{:keys [id string horizon]}]
  [v-box
   :gap "5px"
   :children [[title :label "Sample" :level :level3]
              [:pre string]
              [line :size "4px" :color "#efefef"]
              [title :label "Candidates" :level :level3]
              [horizon-view id horizon]]])

(defn- -heading-view
  "This component exists to expose child-horizons as a prop. Then, the
   will-receive-props lifecycle handler can check for prop change as a
   mechanism for detecting whether a new :fetch-category-horizon event must be
   fired to update the heading horizon. Should only be used in conjunction with
   heading-view."
  [child-horizons heading args]
  (let [last-child-horizons (atom ::uninitialized)]
    (reagent/create-class
      {:component-will-receive-props
       (fn [c [_ child-horizons heading _]]
         #_(console/debug ::will-receive-props {:child-horizons child-horizons
                                                :last-child-horizons @last-child-horizons})
         (when (not= child-horizons @last-child-horizons)
           (reset! last-child-horizons child-horizons)
           (dispatch [:fetch-category-horizon (:id heading)])))
       :reagent-render
       (fn [child-horizons
            {:keys [id horizon]}
            {:keys [dashboard-state children]}]
         (let [child-ids (map :id @children)]
           [v-box
            :gap "5px"
            :children (-> []
                          (conj [title :label "Samples" :level :level3])
                          (into (for [id child-ids]
                                  [:pre (tree-view/get-element-attr (:samples @dashboard-state) id [:data :string])]))
                          (conj [line :size "4px" :color "#efefef"])
                          (conj [title :label "Candidates" :level :level3])
                          (conj
                            ^{:key (str "horizon-view-" id)}
                            [horizon-view id horizon]))]))})))

(defn heading-view [heading]
  (let [dashboard-state (subscribe [:token-dashboard])
        children (reaction
                   (into []
                         (remove #(satisfies? ITokenCategory (:data %)))
                         (tree-view/get-children (:samples @dashboard-state) (:id heading))))
        child-horizons (reaction
                         (into #{} ;; set ensures that reordering doesn't cause another round-trip
                               (comp
                                 (map #(get-in % [:data :horizon]))
                                 (keep identity)) ;; remove the nils
                               @children))]
    (reagent/create-class
      {:reagent-render
       (fn [heading]
         [-heading-view
          @child-horizons
          heading
          {:dashboard-state dashboard-state
           :children children}])})))

(defn element-view
  "Show the details for a single element"
  [element visible?]
  [scroller
   :style (when-not visible?
            {:display "none"})
   :child
   (if (satisfies? ITokenCategory (:data element))
     [heading-view (:data element)]
     [sample-view (:data element)])])

(defn detail-pane
  "The right side of the dashboard"
  [args]
  (let [dashboard-state (subscribe [:token-dashboard])]
    (fn [args]
      (let [{:keys [samples]} @dashboard-state
            selected-id (first (tree-view/get-selected-element-ids samples))
            selected-element (when selected-id (tree-view/get-element samples selected-id))]
        [v-box
         :size "auto"
         :class "token-detail-pane"
         :children (into []
                         (for [s samples]
                           ^{:key (:id s)}
                           [element-view s (= s selected-element)]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Component
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def token-dashboard-args-desc [])

(defn token-dashboard
  [args]
  {:pre [(validate-args-macro token-dashboard-args-desc args "token-dashboard")]}
  (let [token-dashboard-state (subscribe [:token-dashboard])]
    [scroller
     :class "token-dashboard"
     :child
     [v-box
      :size "auto"
      :children [[selection-preview (select-keys @token-dashboard-state [:samples])]
                 [h-split
                  :margin "0"
                  :initial-split (get-in @token-dashboard-state [:layout :left])
                  :panel-1 [sample-picker]
                  :panel-2 [detail-pane]]]]]))
