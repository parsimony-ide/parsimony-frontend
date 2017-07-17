(ns parsimony.views.tree-view.common
  (:require [parsimony.com.editable-label :refer [editable-label]]
            [parsimony.views.common :refer [icon]]
            [parsimony.views.tree-view :refer [ITreeLabel] :as tree-view]
            [reagent.core :as reagent]
            [re-com.core :refer [h-box label row-button popover-anchor-wrapper popover-content-wrapper v-box p gap button] :refer-macros [handler-fn]]
            [re-com.validate :refer [string-or-hiccup? position?] :refer-macros [validate-args-macro]]))

(defn- edit-in-place [id label-str editing? on-submit]
  [editable-label
   {:model label-str
    :editing? editing?
    :on-change (fn [s]
                 (when on-submit (on-submit s))
                 (reset! editing? false))
    :change-on-blur? true
    :auto-focus true
    :auto-select true
    :on-blur #(reset! editing? false)
    :on-key-up (fn [keycode]
                 (when (= keycode 27) ;; keycode for <Esc>
                   (reset! editing? false)))
    :height "inherit"
    :width "inherit"
    :style {:padding "0"
            :padding-right "0"}}])

(defn rename-button [hover editing? on-click]
  [row-button
   :md-icon-name "zmdi zmdi-edit"
   :style {:color "black"}
   :mouse-over-row? (and @hover (not @editing?))
   :tooltip "Rename"
   :on-click (fn []
               (reset! editing? true)
               (when on-click
                 (on-click)))])

(defn delete-button
  ([hover editing? on-click]
   (delete-button hover editing? on-click "Are you sure you want to delete?" "Delete" "250px"))
  ([hover editing? on-click msg tooltip width]
   (let [popover-showing? (reagent/atom nil)]
     (fn [hover editing? on-click]
       [popover-anchor-wrapper
        :showing? popover-showing?
        :position :below-center
        :anchor
        [row-button
         :md-icon-name "zmdi zmdi-delete"
         :style {:color "red"}
         :mouse-over-row? (and @hover (not @editing?))
         :tooltip tooltip
         :on-click #(reset! popover-showing? true)]
        :popover
        [popover-content-wrapper
         :showing? popover-showing?
         :position :below-center
         :on-cancel #(reset! popover-showing? false)
         :no-clip? true
         :width width
         :body
         [v-box
          :size "auto"
          :children [[p msg]
                     [gap :size "5px"]
                     [h-box
                      :children [[button
                                  :label "Yes"
                                  :class "btn btn-danger"
                                  :on-click (fn []
                                              (reset! popover-showing? false)
                                              (on-click))]
                                 [gap :size "5px"]
                                 [button
                                  :label "No"
                                  :on-click #(reset! popover-showing? false)]]]]]]
        ]))))

(defn add-heading-button [hover editing? on-click]
  [row-button
   :md-icon-name "zmdi zmdi-collection-plus"
   :style {:color "black"}
   :mouse-over-row? (and @hover (not @editing?))
   :tooltip "Add Folder"
   :on-click on-click])

(def label-and-icon-args-desc
  [{:name :id :required true :type "integer | uuid"}
   {:name :label-str :required true :type "string" :validate-fn string?}
   {:name :md-icon-name :required false :type "zmdi icon name string" :validate-fn string?}
   {:name :hover-buttons :required false :type "Reagent component"}
   {:name :on-click :required false :type "id -> nil" :validate-fn fn?}
   {:name :on-edit-in-place :required false :type "id -> string -> nil" :validate-fn fn?}
   {:name :tooltip :required false :type "string | hiccup" :validate-fn string-or-hiccup?}
   {:name :tooltip-position :required false :type "keyword" :validate-fn position?}])

(defn label-and-icon
  [args]
  {:pre [(validate-args-macro label-and-icon-args-desc args "label-and-icon")]}
  (let [hover (reagent/atom nil)
        editing? (reagent/atom nil)]
    (fn [{:keys [id label-str md-icon-name hover-buttons on-click on-edit-in-place tooltip tooltip-position] :as args}]
      [h-box
       :class "tree-view-label"
       :align :baseline
       :style (when @hover
                {:background-color "#efefef"})
       :attr {
              ;; deal with issue where row button would remain visible after clicking a row button
              :on-mouse-up-capture (handler-fn
                                    (reset! hover nil))
              :on-mouse-enter (handler-fn
                               (reset! hover true))
              :on-mouse-leave (handler-fn
                               (reset! hover nil))
              :on-click (when on-click
                          (handler-fn
                            (on-click id)))}
       :children [[h-box
                   :align :baseline
                   :gap "5px"
                   :children
                   [[icon (merge {:md-icon-name md-icon-name}
                                 (when tooltip
                                   {:tooltip tooltip
                                    :tooltip-position tooltip-position}))]
                    (if on-edit-in-place
                      [edit-in-place id label-str editing? #(on-edit-in-place id %)]
                      [label :label label-str])]]
                  (if hover-buttons
                    [hover-buttons id hover editing?])]])))
