(ns parsimony.views.modebar
  "The modebar for selecting between major editing modes"
  (:require [re-com.core :refer [h-box v-box label gap] :refer-macros [handler-fn]]
            [re-frame.core :refer [dispatch subscribe]]
            [reagent.core :as reagent]
            [parsimony.views.common :refer [icon]]
            [schema.core :as s :include-macros true]))

(def modes [[:token {:text "Token"
                     :icon "zmdi-view-dashboard"
                     :color "#0072bb"}]
            [:parse {:text "Parse"
                     :icon "zmdi-arrow-merge"
                     :color "purple"}]
            [:manual {:text "Freehand"
                      :icon "zmdi-keyboard"
                      :color "gray"}]])

(defn mode-button [{:keys [mode] :as args}]
  (let [hover (reagent/atom nil)
        current-mode (subscribe [:current-mode])
        on-click (fn [] (dispatch [:modebar-click mode]))]
    (fn [{:keys [text md-icon-name color] :as args}]
      [h-box
       :class (str "mode-button "
                   (if @hover "hover " " ")
                   (if (= @current-mode mode) "active " " "))
       :justify :center
       :align :center
       :style {:background-color color}
       :attr {:on-click (handler-fn (on-click))
              :on-mouse-enter (handler-fn
                               (reset! hover true))
              :on-mouse-leave (handler-fn
                               (reset! hover nil))}
       :gap "5px"
       :children [[label :label text]
                  [icon {:md-icon-name md-icon-name}]]])))

(defn modebar
  "Top-level modebar component"
  [args]
  [v-box
   :class "modebar"
   :size "none"
   :height "100%"
   :width "30px"
   :gap "90px"
   :children
   (into [[gap :size "50px"]]
         (for [[mode {:keys [text icon color]}] modes]
           ^{:key (str mode)}
           [mode-button {:text text :md-icon-name icon :color color :mode mode
                         :on-click #(dispatch [:modebar-click mode])}]))])
