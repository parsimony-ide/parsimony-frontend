(ns parsimony.views.color-preview
  (:require [parsimony.util :refer [pprint-str]]
            [parsimony.models.colors :as colors]
            [re-com.core :refer [h-box v-box scroller]]))

(defn one-color [color]
  (let [css (-> color
                colors/color->decoration
                colors/decoration->html-css)]
    [h-box
     :height "30px"
     :style css
     :children [[:span (pprint-str color)]]]))

(defn color-preview [colors]
  [scroller
   :child
   [v-box
    :size "auto"
    :gap "5px"
    :children (into [[h-box :children [[:span (str (count colors) " colors")]]]]
                    (map (partial vector one-color))
                    colors)]])
