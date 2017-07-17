(ns parsimony.views.project-picker
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [parsimony.com.buttons :refer [row-button]]
            [parsimony.models.project-picker :refer [Heading Project]]
            [parsimony.models.source :as source]
            [parsimony.util :refer [matches-schema?]]
            [parsimony.views.common :refer [icon]]
            [parsimony.views.tree-view :refer [ITreeLabel] :as tree-view]
            [parsimony.views.tree-view.common :refer [label-and-icon delete-button rename-button]]
            [re-com.core
             :refer [md-icon-button box scroller h-box v-box label p gap button
                     popover-anchor-wrapper popover-content-wrapper
                     input-textarea]
             :refer-macros [handler-fn]]
            [reagent.core :as reagent]
            [re-frame.core :refer [dispatch subscribe]]
            [schema.core :as s :include-macros true]
            [parsimony.console :as console]))

(defn add-project-button [hover]
  [row-button
   :md-icon-name "zmd zmdi-plus"
   :style {:color "black"}
   :mouse-over-row? @hover
   :tooltip "Create New Project"
   :on-click #(dispatch [:create-new-project])])

(defn- heading-hover-buttons [_ hover _]
  (fn [_ hover]
    [h-box
     :margin "0 0 0 15px"
     :children
     [[h-box
       :children
       [[add-project-button hover]]]]]))

(defn source-view [source]
  [:span (source/source-path source)])

(defn source-list-view [sources]
  [scroller
   :class "source-list-view"
   :max-height "50px"
   :child [v-box
           :children (into []
                           (map (partial vector source-view))
                           (sort-by source/source-path sources))]])

(defn open-project-button [project-id]
  (let [popover-showing? (reagent/atom nil)
        current-project (subscribe [:current-project])
        db-modified? (subscribe [:db-modified?])]
    (fn [project-id]
      [box
       :align :start
       :child
       [popover-anchor-wrapper
        :showing? popover-showing?
        :position :below-center
        :anchor
        [h-box
         :class (str "open-project-button")
         :size "auto"
         :align :baseline
         :gap "5px"
         :attr {:on-click (handler-fn
                            (if (and (some? @current-project)
                                     @db-modified?)
                              (reset! popover-showing? true)
                              (dispatch [:open-project project-id])))}
         :children [[icon {:style {:flex "none"}
                           :md-icon-name "zmdi-open-in-new"}]
                    [label :label "Open this Project"]]]
        :popover
        [popover-content-wrapper
         :showing? popover-showing?
         :position :right-center
         :on-cancel #(reset! popover-showing? false)
         :no-clip? true
         :width "300px"
         :body
         [v-box
          :size "auto"
          :gap "5px"
          :children [[:div
                      [:p "Are you sure you want to open this project?"]
                      [:p "Any unsaved changes will be lost."]]
                     [h-box
                      :gap "5px"
                      :children [[button
                                  :label "Open"
                                  :class (str "btn open")
                                  :on-click (fn []
                                              (reset! popover-showing? false)
                                              (dispatch [:open-project project-id]))]
                                 [button
                                  :label "Cancel"
                                  :on-click #(reset! popover-showing? false)]]]]]]]])))

(defn loaded-icon []
  [box
   :align :start
   :child
   [h-box
    :class "loaded-icon"
    :align :baseline
    :gap "5px"
    :children [[icon {:style {:flex "none"}
                      :md-icon-name "zmdi-badge-check"}]
               [label :label "This project is open"]]]])

(defn- project-hover-buttons [project-id hover editing?]
  (let [current-project-id (subscribe [:current-project])]
    (fn [project-id hover editing?]
      [h-box
       :margin "0 0 0 15px"
       :children
       [[h-box
         :gap "5px"
         :children
         [[rename-button hover editing? nil]
          (when-not (= project-id @current-project-id)
            [delete-button hover editing? #(dispatch [:delete-project project-id])])]]]])))

(defn project-tree-label [project-id]
  (let [project (subscribe [:project-with-id project-id])
        current-project-id (subscribe [:current-project])]
    (fn [project-id]
      [v-box
       :class "project-tree-label"
       :size "auto"
       :align :stretch
       :gap "3px"
       :children [[label-and-icon {:id project-id
                                   :label-str (if-let [description (:description @project)]
                                                description
                                                (str (.format (js/moment (:last-saved @project)) "ll")
                                                     " "
                                                     (.format (js/moment (:last-saved @project)) "LTS")))
                                   :md-icon-name "zmdi-collection-text"
                                   :tooltip-position :right-center
                                   :tooltip [:pre {:class "detail-tooltip"}
                                             ;; FIXME: tooltip doesn't update with time
                                             (str/join "\n"
                                                       [(str "Saved " (.fromNow (js/moment (:last-saved @project))))
                                                        (str (count (:sources @project)) " files")])]
                                   :hover-buttons project-hover-buttons
                                   :on-edit-in-place #(dispatch [:set-project-description %1 %2])}]
                  [v-box
                   :gap "3px"
                   :class (str "project-tree-label-details "
                               (if (= project-id @current-project-id)
                                 "active"
                                 "inactive"))
                   :children [[source-list-view (vals (:sources @project)) 3]
                              (if (= project-id @current-project-id)
                                [loaded-icon]
                                [open-project-button project-id])]]]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ITreeLabel Instances
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IProjectPickerElement)

(extend-type Project
  IProjectPickerElement
  ITreeLabel
  (tree-label [{:keys [project-id] :as this}]
    [project-tree-label project-id]))

(extend-type Heading
  IProjectPickerElement
  ITreeLabel
  (tree-label [this]
    [box
     :child [label-and-icon {:id 0
                             :label-str "All Projects"
                             :md-icon-name "zmdi-format-list-bulleted"
                             :hover-buttons heading-hover-buttons}]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Validation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn valid-drop-fn
  "Validation function that only allows drops between elements satisfying the IProjectPickerElement protocol."
  [data parent _]
  (when-let [tree-elem (get data ::element)]
    (let [res (vector (matches-schema? tree-view/element-schema tree-elem)
                      (matches-schema? tree-view/element-schema parent)
                      (satisfies? IProjectPickerElement (:data tree-elem))
                      (satisfies? IProjectPickerElement (:data parent)))]
      (if (every? identity res)
        true
        (do (console/warn :invalid-drop res)
            false)))))

(defn valid-drag-type-fn
  [drag-types _ _]
  (seq (set/intersection drag-types #{::element})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Component
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn project-picker [args]
  (let [state (subscribe [:project-picker])]
    [v-box
     :size "auto"
     :class "project-picker"
     :children [[scroller
                 :h-scroll :off
                 :child [tree-view/tree-parent
                         {:model (:tree @state)
                          :drag-type ::element
                          :valid-drop-fn valid-drop-fn
                          :valid-drag-type-fn valid-drag-type-fn
                          :on-toggle #(dispatch [:project-picker-toggle (:id %)])
                          :on-move #(dispatch [:project-picker-move (:id %1) (:id %2) %3])}]]]]))

