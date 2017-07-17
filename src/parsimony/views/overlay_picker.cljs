(ns parsimony.views.overlay-picker
  (:require [parsimony.com.label :refer [label]]
            [parsimony.util :refer [debug overlay-seq]]
            [parsimony.models.colors :as colors :refer [decoration->html-css]]
            [parsimony.models.overlay-state :as overlay-state]
            [re-com.util :refer [deref-or-value]]
            [re-com.validate :refer-macros [validate-args-macro]]
            [re-com.core :refer [h-box v-box scroller checkbox button] :refer-macros [handler-fn]]
            [re-frame.core :refer [dispatch subscribe]]
            [reagent.core :as reagent]
            [parsimony.console :as console]
            [clojure.string :as str])
  (:require-macros [reagent.ratom :refer [reaction]]))

(defn- disabled? [disabled-overlays {:keys [type tag] :as overlay}]
  (contains? disabled-overlays [type tag]))

(defn show-hide-all [{:keys [overlay-state] :as args}]
  (let [disabled-overlays (reaction (overlay-state/disabled-overlays @overlay-state))]
    (fn [{:keys [type
                 overlays
                 on-disable-overlay
                 on-enable-overlay
                 on-disable-overlays-by-type
                 on-enable-overlays-by-type] :as args}]
      [checkbox
       :label [label
               :label (name type)
               :style {:font-weight :bold}
               :size "auto"]
       :model (reduce #(and %1 %2) true
                      (map (comp not #(disabled? @disabled-overlays %)) overlays))
       :on-change (fn [state]
                    (console/debug ::show-hide-all {:state state})
                    (if (and (some? on-disable-overlays-by-type)
                             (some? on-enable-overlays-by-type))
                      ;; if *-by-type callbacks are provided, use those
                      (if state
                        (on-enable-overlays-by-type type)
                        (on-disable-overlays-by-type type))
                      ;; otherwise fall back to iterating over individual on-* callbacks
                      (doseq [{:keys [type tag]} overlays]
                        (if state
                          (on-enable-overlay type tag)
                          (on-disable-overlay type tag)))))])))

(defn overlay-tag-item
  [{:keys [overlay-state] :as args}]
  (let [peek (reaction (overlay-state/all-peek @overlay-state))
        disabled-overlays (reaction (overlay-state/disabled-overlays @overlay-state))
        decorations (subscribe [:decorations])]
    (fn [{:keys [overlay-state type tag on-click-overlay on-toggle-overlay on-unpeek] :as args}]
      (let [{:keys [decoration-index decoration-mod peek-item] :as overlay} (overlay-state/get-overlay @overlay-state type tag)
            decoration (colors/lookup-decoration @decorations decoration-index)]
        [h-box
         :size "auto"
         :align :baseline
         :gap "5px"
         :style (when (and (seq @peek)
                           (not (contains? @peek [type tag])))
                  {:opacity "0.3"})
         :children [[checkbox
                     :model (not (disabled? @disabled-overlays overlay))
                     :on-change #(on-toggle-overlay type tag)]
                    [label
                     :class "overlay-tag-item-label"
                     :label tag
                     :size "auto"
                     :attr {:on-mouse-leave (handler-fn
                                              (when (seq @peek)
                                                (on-unpeek type tag)))}
                     :on-click #(on-click-overlay type tag)
                     :style (decoration->html-css (colors/mix decoration decoration-mod))]]]))))

(defn overlay-type-item [{:keys [overlay-state type] :as args}]
  (let [non-ws-overlays (for [[-type m] (overlay-state/all-overlays @overlay-state)
                              [tag overlay] (sort-by first m)
                              :when (and (= type -type)
                                         (not (and (= tag "ws")
                                                   (= type :tokens))))]
                          overlay)]
    [v-box
     :gap "3px"
     :children [[show-hide-all (assoc args :overlays non-ws-overlays)]
                [v-box
                 :gap "3px"
                 :padding "0 0 0 15px"
                 :children
                 (vec (for [tag (map :tag non-ws-overlays)]
                        ^{:key tag}
                        [overlay-tag-item (assoc args :tag tag)]))]]]))

(defn whitespace-item [{:keys [overlay-state on-toggle-overlay] :as args}]
  (if-let [ws-overlay (overlay-state/get-overlay @overlay-state :tokens "ws")]
    [h-box
     :size "auto"
     :align :baseline
     :gap "5px"
     :children
     [[label :label "Show whitespace?"
       :style {:cursor :pointer}
       :on-click #(on-toggle-overlay :tokens "ws")]
      [overlay-tag-item (assoc args :type :tokens :tag "ws")]]]
    [:div]))

(def -overlay-picker-args-desc
  [{:name :overlay-state               :required true  :type "atom"}
   {:name :on-click-overlay            :required true  :type "overlay-type -> overlay-tag -> nil"}
   {:name :on-disable-overlay          :required true  :type "overlay-type -> overlay-tag -> nil"}
   {:name :on-enable-overlay           :required true  :type "overlay-type -> overlay-tag -> nil"}
   {:name :on-toggle-overlay           :required true  :type "overlay-type -> overlay-tag -> nil"}
   {:name :on-unpeek                   :required true  :type "overlay-type -> overlay-tag -> nil"}
   {:name :on-disable-overlays-by-type :required false :type "overlay-type -> nil"}
   {:name :on-enable-overlays-by-type  :required false :type "overlay-type -> nil"}])

(defn -overlay-picker [{:keys [overlay-state on-toggle-overlay] :as args}]
  {:pre [(validate-args-macro -overlay-picker-args-desc args "-overlay-picker")]}
  [scroller
   :class "overlay-picker"
   :v-scroll :auto
   :h-scroll :off
   :child
   [v-box
    :gap "3px"
    :children
    (-> []
        (conj [whitespace-item args])
        (into (for [type (overlay-state/all-overlay-types @overlay-state)]
                [overlay-type-item (assoc args :type type)])))]])

(defn editor-overlay-picker
  "Returns markup for a pretty visualization of the overlays available on the given editor"
  [editor-id]
  (let [editor (subscribe [:editor-with-id editor-id])
        overlay-state (reaction (:overlay-state @editor))
        peek (reaction (overlay-state/all-peek @overlay-state))]
    (fn [editor-id]
      (if (overlay-state/has-overlays? @overlay-state)
        (let [editor-id (deref-or-value editor-id)]
          [-overlay-picker {:overlay-state overlay-state
                            :on-click-overlay (fn [type tag] (dispatch [:peek-overlay editor-id type tag]))
                            :on-disable-overlay (fn [type tag] (dispatch [:disable-overlay editor-id type tag]))
                            :on-enable-overlay (fn [type tag] (dispatch [:enable-overlay editor-id type tag]))
                            :on-toggle-overlay (fn [type tag] (dispatch [:toggle-overlay editor-id type tag]))
                            :on-unpeek (fn [type tag] (dispatch [:unpeek-overlay editor-id type tag]))
                            :on-disable-overlays-by-type (fn [type] (dispatch [:disable-overlays-by-type editor-id type]))
                            :on-enable-overlays-by-type (fn [type] (dispatch [:enable-overlays-by-type editor-id type]))}])
        [:div.overlay-picker.empty]))))

(defn solver-overlay-picker
  []
  (let [overlay-state (subscribe [:solver/overlay-state])]
    (fn []
      (if (overlay-state/has-overlays? @overlay-state)
        [-overlay-picker {:overlay-state overlay-state
                          :on-click-overlay (fn [type tag] (dispatch [:solver/peek-overlay type tag]))
                          :on-disable-overlay (fn [type tag] (dispatch [:solver/disable-overlay type tag]))
                          :on-enable-overlay (fn [type tag] (dispatch [:solver/enable-overlay type tag]))
                          :on-toggle-overlay (fn [type tag] (dispatch [:solver/toggle-overlay type tag]))
                          :on-unpeek (fn [type tag] (dispatch [:solver/unpeek-overlay type tag]))
                          :on-disable-overlays-by-type (fn [type] (dispatch [:solver/disable-overlays-by-type type]))
                          :on-enable-overlays-by-type (fn [type] (dispatch [:solver/enable-overlays-by-type type]))}]
        [:div.overlay-picker.empty]))))

(defn overlay-picker
  []
  (let [editor-id (subscribe [:current-editor])
        card-key (subscribe [:current-card])]
    (cond
      (= "solver-card" @card-key)
      [solver-overlay-picker]

      (some? @editor-id)
      [editor-overlay-picker editor-id]

      :else
      [:div.overlay-picker.empty])))
