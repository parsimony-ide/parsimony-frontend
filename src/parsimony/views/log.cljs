(ns parsimony.views.log
  (:require [parsimony.util :refer [pprint-str]]
            [parsimony.views.common :refer [icon]]
            [parsimony.views.ribbon :refer [ribbon] :as ribbon]
            [reagent.core :as reagent]
            [re-com.core :refer [h-box label scroller v-box]]
            [re-frame.core :refer [dispatch subscribe]]))

(defn- timestamp-view [timestamp]
  [:div.timestamp-view
   (.format (js/moment timestamp) "LTS")])

(defn- glyph [type]
  [:span
   {:class (str "glyph " (name type))}
   [icon {:md-icon-name (case type
                          :info "zmdi-help"
                          :warn "zmdi-alert-triangle"
                          :error "zmdi-alert-octagon"
                          :success "zmdi-check-square")}]])

(defn- entry-view [{:keys [type timestamp value] :as entry}]
  [h-box
   :class (str "entry-view " (name type))
   :children [[glyph type]
              value
              [timestamp-view timestamp]]])

(defn- entries-view []
  (let [entries (subscribe [:log/entries])]
    (reagent/create-class
      {:component-did-update
       (fn [c _]
         (when-let [dom-node (reagent/dom-node c)]
           (set! (.-scrollTop dom-node) (.-scrollHeight dom-node))))
       :reagent-render
       (fn []
         [scroller
          :class "entries-view"
          :child
          [v-box :children (into []
                                 (map (partial vector entry-view))
                                 @entries)]])})))

(defn log-view []
  [v-box
   :class "log-view"
   :size "auto"
   :children
   [[ribbon {:model [(ribbon/button-item
                       {:text "Clear Log"
                        :md-icon-name "zmdi-format-color-reset"
                        :on-click #(dispatch [:log/reset])})]}]
    [entries-view]]])


