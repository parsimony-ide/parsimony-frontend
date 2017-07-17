(ns parsimony.com.box
  "Enhanced version of boxes in which gaps can be drop targets"
  (:require [parsimony.util :refer [get-all-drag-data get-drag-types set-drop-effect show-drag-data]]
            [re-com.box :refer [box gap v-box v-box-args-desc h-box h-box-args-desc]]
            [re-com.core :refer-macros [handler-fn]]
            [re-com.util :refer [deref-or-value]]
            [re-com.validate :refer-macros [validate-args-macro]]
            [reagent.core :as reagent]
            [parsimony.console :as console]))

(defn- indexed-interpose
  "Similar to interpose, but each separator is the result of calling sep-fn on
   idx, where idx is the 0-origin position in the resulting collection.
   Additionally, the beginning and end of the result are separators by default.
   If add-final? is false, then do not add the end separator."
  ([sep-fn coll]
   (indexed-interpose sep-fn true coll))
  ([sep-fn add-final? coll]
   (concat
     (mapcat #(list (sep-fn %1) %2) (range) coll)
     (when add-final?
       (list (sep-fn (count coll)))))))

(defn- create-gap-form-fn
  "Draggy h-box and v-box are almost the same, with only slight differences. This function extracts the commonality
  between the two and generates a function appropriate as a sep-fn for indexed-interpose"
  [gap draggy hover on-gap-drop valid-drop-fn valid-drag-type-fn halo-before halo-after halo-grow? gap-color halo-color orientation]
  {:pre [(#{:v :h} orientation)]}
  (let [[halo-style static-gap] (case orientation
                                  :v [{:position :absolute
                                       :width "100%"
                                       :top (str "-" halo-before)
                                       :bottom (str "-" halo-after)}
                                      [re-com.box/gap :size gap :height gap]]
                                  :h [{:position :absolute
                                       :height "100%"
                                       :left (str "-" halo-before)
                                       :right (str "-" halo-after)}
                                      [re-com.box/gap :size gap :width gap]])]
    (fn [i]
      (when gap
        (if draggy
          [box ;; this box represents the visible gap that actually separates
           :class (str "draggy-gap " (if (= @hover i) "hover" ""))
           :style (merge {:position :relative} ;; position relative so absolutely-positioned child halo is relative to this origin
                         ;; make room for the halo if halo-grow?
                         (when (and (= @hover i)
                                    halo-grow?)
                           (if (= :h orientation)
                             {:margin-left halo-before
                              :margin-right halo-after}
                             {:margin-top halo-before
                              :margin-bottom halo-after}))
                         ;; set gap color during hover if gap-color specified
                         (when (and (= @hover i)
                                    gap-color)
                             {:background-color gap-color}))
           :size gap
           ;; :height gap ;; don't need this since size does what we want
           :child [:div.halo
                   ;; this represents an invisible area around the gap that still triggers drop
                   ;; behavior this is so the user doesn't need insane accuracy to hit a 2px gap
                   {:style (if-not halo-grow?
                             halo-style ;; if not halo-grow?, then always have a static halo
                             (if (= @hover i)
                               ;; otherwise, when halo-grow? and hover, expand the halo
                               (merge
                                halo-style
                                (when halo-color
                                  {:background-color halo-color}))
                               ;; make the halo same size as gap when not hovering
                               {:position :absolute
                                :top 0
                                :bottom 0
                                :left 0
                                :right 0}))
                    :on-drag-over
                    (handler-fn
                     (.preventDefault event)
                     (.stopPropagation event)
                     (set-drop-effect event :move))
                    :on-drop
                    (handler-fn
                     (.preventDefault event)
                     (.stopPropagation event)
                     (reset! hover nil)
                     (let [data (get-all-drag-data event)]
                       (console/debug ::on-drop {:all-drag-data data})
                       (when (and on-gap-drop
                                  (valid-drop-fn data i)
                                  (valid-drag-type-fn (get-drag-types event) i))
                         (on-gap-drop data i))))
                    :on-drag-enter
                    (handler-fn
                      (.preventDefault event)
                      (.stopPropagation event)
                      (when (valid-drag-type-fn (get-drag-types event) i)
                        (set-drop-effect event :move)
                        (reset! hover i)))
                    :on-drag-leave
                    (handler-fn
                      (reset! hover nil))}]]
          static-gap)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draggy v-box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def draggy-args-desc
  [{:name :draggy             :required false :default true    :type "boolean | atom"                      :description "Whether or not the gaps can be drop targets"}
   {:name :valid-drop-fn      :required false                  :type "drag-data -> integer -> nil"         :description "Compute whether this is an acceptable drop target for the given element"}
   {:name :valid-drag-type-fn :required false                  :type "set of drag-types -> integer -> nil" :description "Compute whether to show drop indicator based on the drag-types of the element being dragged."}
   {:name :on-gap-drop        :required false                  :type "drag-data -> idx -> nil"             :description "called whenever a gap is a drop target"}
   {:name :halo-before        :required false :default "5px"   :type "CSS size string"                     :description "Slop before the gap that still allows drop"}
   {:name :halo-after         :required false :default "5px"   :type "CSS size string"                     :description "Slop after the gap that still allows drop"}
   {:name :halo-grow?         :required false :default "false" :type "boolean"                             :description "Instead of a static halo, grow the halo according to :halo-before and :halo-after during drag-over"}
   {:name :final-gap?         :required false :default "true"  :type "boolean"                             :description "When true, draw the gap that occurs after the last element."}
   {:name :gap-color          :required false                  :type "color string"                        :description "Color of gap during drag-over"}
   {:name :halo-color         :required false                  :type "color string"                        :description "Color of halo during drag-over"}])

(def draggy-v-box-args-desc
  (into v-box-args-desc draggy-args-desc))

(defn draggy-v-box [args]
  {:pre [(validate-args-macro draggy-v-box-args-desc args "draggy-v-box")]}
  (let [hover (reagent/atom nil)]
    (fn [{:keys [children draggy valid-drop-fn valid-drag-type-fn on-gap-drop gap halo-before halo-after halo-grow? final-gap? gap-color halo-color]
          :or {draggy true
               halo-before "5px"
               halo-after "5px"
               halo-grow? false
               final-gap? true}
          :as args}]
      (let [draggy (deref-or-value draggy)
            valid-drop-fn (if valid-drop-fn valid-drop-fn
                            (constantly true))
            valid-drag-type-fn (if valid-drag-type-fn valid-drag-type-fn
                                 (constantly true))
            gap-form-fn (create-gap-form-fn gap draggy hover on-gap-drop valid-drop-fn valid-drag-type-fn halo-before halo-after halo-grow? gap-color halo-color :v)
            children (if gap
                       (->> children
                            (filter identity) ;; filter to remove possible nils so we don't add unwanted gaps
                            (indexed-interpose gap-form-fn final-gap?))
                       children)]
        (into [v-box :children children]
              (-> args
                  (dissoc :gap :draggy :valid-drop-fn :valid-drag-type-fn :halo-before :halo-after :halo-grow? :final-gap? :gap-color :halo-color :on-gap-drag-over :on-gap-drop :children)
                  (seq)
                  (flatten)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draggy h-box
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def draggy-h-box-args-desc
  (into h-box-args-desc draggy-args-desc))

;; XXX: this is still kind of copy-paste from draggy-v-box. Can we extract the commonality more than we have?
(defn draggy-h-box [args]
  {:pre [(validate-args-macro draggy-h-box-args-desc args "draggy-h-box")]}
  (let [hover (reagent/atom nil)]
    (fn [{:keys [children draggy valid-drop-fn valid-drag-type-fn on-gap-drop gap halo-before halo-after halo-grow? final-gap? gap-color halo-color]
          :or {draggy true
               halo-before "5px"
               halo-after "5px"
               halo-grow? false
               final-gap? true}
          :as args}]
      (let [draggy (deref-or-value draggy)
            valid-drop-fn (if valid-drop-fn valid-drop-fn
                              (constantly true))
            valid-drag-type-fn (if valid-drag-type-fn valid-drag-type-fn
                                 (constantly true))
            gap-form-fn (create-gap-form-fn gap draggy hover on-gap-drop valid-drop-fn valid-drag-type-fn halo-before halo-after halo-grow? gap-color halo-color :h)
            children (if gap
                       (->> children
                            (filter identity) ;; filter is to remove possible nils so we don't add unwanted gaps
                            (indexed-interpose gap-form-fn final-gap?))
                       children)]
        (into [h-box :children children]
              (-> args
                  (dissoc :gap :draggy :valid-drop-fn :valid-drag-type-fn :halo-before :halo-after :halo-grow? :final-gap? :gap-color :halo-color :on-gap-drag-over :on-gap-drop :children)
                  (seq)
                  (flatten)))))))
