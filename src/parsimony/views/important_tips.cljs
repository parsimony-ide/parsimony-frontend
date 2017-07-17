(ns parsimony.views.important-tips
  (:require [parsimony.views.modal :refer [IModal]]
            [re-com.core :refer [v-box scroller line gap]]))

(defn pre [s]
  [:pre {:style {:font-size "14px"}}
   s])

(defn page []
  [pre
"1. Use 'Lex File' and 'Parse File' frequently to review your progress.

2. Although you can write lexer rules and productions manually, STRONGLY
   PREFER using the automated features because they can save you a lot of
   headaches from things like typos.

3. When using the solver:
   a. Make your examples as small as possible.
   b. Solve as few examples simultaneously as necessary. In particular,
      start with one example, and only add more if the result doesn't match
      your expectations.
   c. Accept the solution as soon as you find it acceptable: don't add more
      examples until you have done so."])

(defn important-tips-pane []
  [scroller
   :size "auto"
   :child
   [v-box
    :children [[:div {:style {:font-size "25px"
                              :font-weight "lighter"}}
                "Important Tips"]
               [line]
               [gap :size "10px"]
               [page]]]])

(defrecord ImportantTipsModal []
  IModal
  (show-modal [this]
    [v-box
     :size "auto"
     :class "important-tips-modal"
     :children [[important-tips-pane]]])

  (backdrop-closeable? [this]
    true))

(cljs.reader/register-tag-parser! "parsimony.views.important-tips.ImportantTipsModal" map->ImportantTipsModal)

(defn important-tips-modal []
  (ImportantTipsModal.))


