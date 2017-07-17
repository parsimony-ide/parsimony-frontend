(ns parsimony.views.info
  (:require [re-com.core :refer [h-box v-box box popover-anchor-wrapper popover-content-wrapper]]
            [re-com.util :refer [deref-or-value]]
            [reagent.core :as reagent]
            [parsimony.console :as console]))

(defprotocol IDetail
  (detail-view [this] "Return detail hiccup for this record"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Details View
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn lower-left [coords]
  (let [x-min (apply min (map first coords))
        y-min (apply max (map second coords))]
    (vector x-min y-min)))

;; a detail maps 1-to-1 to a polygon
(defn detail [decoration on-click id {:keys [coords] :as polygon} info]
  ;; create a popover-anchor-wrapper around an invisible div
  (let [content-hiccup [v-box
                        :class "detail-content"
                        :align-self :start
                        :children [(detail-view info)]]
        [x y] (lower-left coords)
        anchor-hiccup [box
                       :style {:width "1px"
                               :height "1px"
                               :background "green"}
                       :child [:span]]]
    [popover-anchor-wrapper
     :style {:position :absolute
             :left (str (+ 5 x) "px")
             :top (str y "px")}
     :showing? (reagent/atom true)
     :position :below-right
     :anchor anchor-hiccup
     :popover [popover-content-wrapper
               :showing? (reagent/atom true)
               :popover-color "#f5f5f5"
               :padding "3px 15px"
               :position :below-right
               :body content-hiccup]]))

(defn details-view [{:keys [polygons infos active-infos decoration width height on-click] :as args}]
  (let [polygons (deref-or-value polygons)
        infos (deref-or-value infos)]
    (console/debug "polygons" (str polygons))
    (console/debug "infos" (str infos))
    (console/debug "active-infos" (str active-infos))
    [h-box
     :class "detail-parent"
     :width (if width width "100%")
     :height (if height height "100%")
     :children
     (for [[id polygon info] (map vector (range) polygons infos) :when (contains? active-infos id)]
       ^{:key id}
       [detail decoration on-click id polygon info])]))
