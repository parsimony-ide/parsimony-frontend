(ns parsimony.com.svg
  (:require [clojure.string :as str]
            [parsimony.models.colors :as colors :refer [color->str decoration->svg-css]]
            [parsimony.util :refer [pprint-str]]
            [re-com.core :refer [h-box] :refer-macros [handler-fn]]
            [re-com.util :refer [deref-or-value]]
            [reagent.core :as reagent]))

(def svg-args-desc
  [{:name :model :required true}
   {:name :decoration :required true}
   {:name :emphasis-mods :required false}
   {:name :width :required false}
   {:name :height :required false}
   {:name :on-click :required false :description "Called with the id of the polygon that was clicked"}
   ])

(defn- coord->str [[x y]]
  (str x "," y))

(defn- polygon [decoration on-click id {:keys [coords]}]
  (let [points-str (str/join " " (map coord->str coords))]
    [:polygon
     (merge
      (decoration->svg-css decoration)
      {:key id
       :points points-str
       :on-click (when on-click
                   (handler-fn
                    (on-click id)))})]))

(defn hatch-pattern
  [id decoration rotation]
  ;; Note: we use a Type-3 Reagent component here because React does not support the patternTransform attribute on SVG
  ;; patterns. Thus, we have to set the attribute on the mounted DOM node instead of passing directly via the
  ;; :patternTransform key
  (reagent/create-class
   {:component-did-mount
    (fn [c]
      (let [dom-node (reagent/dom-node c)]
        (.setAttribute dom-node "patternTransform" (str "rotate(" rotation ")"))))
    :reagent-render
    (fn [id decoration rotation]
      (let [color (get-in decoration [:fill :color])]
        [:pattern
         {:id (str id "-" (str/join "-" color))
          :width "3"
          :height "3"
          ;; :patternTransform "rotate(45 0 0)" ;; commented out because React does not support this yet
          :patternUnits "userSpaceOnUse"}
         [:line
          {:x1 "0"
           :y1 "0"
           :x2 "0"
           :y2 "10"
           :stroke (color->str color)
           :stroke-width "1"}]]))}))

(defn hatch-def
  [id decoration rotation]
  [:defs
   [hatch-pattern id decoration rotation]])

(defn glow-def
  "A mod that makes a polygon glow in its own color.  Unfortunately, this seems
   a bit glitchy as of Chrome version 53.0.2785.116: the glow effect
   unpredictably disappears on UI update"
  []
  ;; See http://stackoverflow.com/a/19704735 for explanation
  [:defs
   [:filter {:id "glow"
             :x "-5000%"
             :y "-5000%"
             :width "10000%"
             :height "10000%"}
    [:feMorphology
     {:in "SourceGraphic"
      :operator "dilate"
      :radius "2"
      :result "dilated"}]
    [:feGaussianBlur
     {:in "dilated"
      :stdDeviation "2"
      :result "blurred"}]
    [:feComposite
     {:in "blurred"
      :in2 "SourceGraphic"
      :operator "arithmetic"
      :k2 "1"
      :k3 "-1"
      :result "nocombine"}]
    [:feMerge
     [:feMergeNode {:in "nocombine"}]
     [:feMergeNode {:in "SourceGraphic"}]]]])

(defn- apply-emphasis
  [index decoration emphasis-mods]
  (let [applicable-mods (for [[_ emphasis-map] emphasis-mods
                              :let [mod (get emphasis-map index)]
                              :when (some? mod)]
                          mod)]
    (reduce #(colors/mix %1 %2)
            decoration
            applicable-mods)))

;; model is a vector of polygons
(defn- svg-base [{:keys [model decoration emphasis-mods on-click] :as args}]
  (let [polygon-vec (deref-or-value model)]
    (into
      [:svg {:style {:overflow "visible"}}
       [hatch-def "hatch-0" decoration 0]
       [hatch-def "hatch-45" decoration 45]
       [hatch-def "hatch-90" decoration 90]
       [hatch-def "hatch-135" decoration 135]
       [glow-def]]
      (for [[id poly] (map-indexed vector polygon-vec)]
        ^{:key id}
        [polygon (apply-emphasis id decoration emphasis-mods) on-click id poly]))))

(defn svg [{:keys [width height] :as args}]
  [h-box
   :class "svg-parent"
   :width (if width width "100%")
   :height (if height height "100%")
   :children [[svg-base args]]])
