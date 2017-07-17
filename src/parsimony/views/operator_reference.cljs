(ns parsimony.views.operator-reference
  (:require [parsimony.views.modal :refer [IModal]]
            [re-com.core :refer [v-box scroller line gap]]))

(defn pre [s]
  [:pre {:style {:font-size "14px"}}
   s])

(defn page []
  [pre
"As a reference, the following is a subset of the operator preference hierarchy
for the C language.
   
All operators shown are left-associative.

Highest precedence
                        * / %
                          + -
                        >> <<
                    < > <= >=
                        == !=
                            &
                            |
                           &&
                           ||
Lowest precedence"])

(defn operator-reference-pane []
  [scroller
   :size "auto"
   :child
   [v-box
    :children [[:div {:style {:font-size "25px"
                              :font-weight "lighter"}}
                "Operator Reference"]
               [line]
               [gap :size "10px"]
               [page]]]])

(defrecord OperatorReferenceModal []
  IModal
  (show-modal [this]
    [v-box
     :size "auto"
     :class "operator-reference-modal"
     :children [[operator-reference-pane]]])

  (backdrop-closeable? [this]
    true))

(cljs.reader/register-tag-parser! "parsimony.views.operator-reference.OperatorReferenceModal" map->OperatorReferenceModal)

(defn operator-reference-modal []
  (OperatorReferenceModal.))

