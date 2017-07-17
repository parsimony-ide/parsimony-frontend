(ns parsimony.com.menu
  "A menubar"
  (:require [parsimony.views.tree-view :as tree-view]
            [re-com.core :refer [h-box v-box box popover-anchor-wrapper popover-content-wrapper] :refer-macros [handler-fn]]
            [re-com.util :refer [deref-or-value]]
            [re-com.validate :refer-macros [validate-args-macro]]
            [reagent.core :as reagent]
            [reagent.ratom :refer-macros [reaction]]
            [schema.core :as s :include-macros true]))

;; Conceptually, a menu is a lot like a tree-view, except instead of showing a nested view, we show nested popups
;; Additionally, we do not support drag and drop, which simplifies matters greatly

(defprotocol IMenuLabel
  (menu-label [this] "Convert this into a string or hiccup for rendering a menu label"))

(extend-type nil
  IMenuLabel
  (menu-label [_] [:span]))

(extend-type string
  IMenuLabel
  (menu-label [s] [:span s]))

(extend-type Keyword
  IMenuLabel
  (menu-label [kw] [:code (str kw)]))

(extend-type number
  IMenuLabel
  (menu-label [n] [:span (str n)]))

(def element-schema {:id s/Num
                     :parent (s/maybe s/Num)
                     :data (s/protocol IMenuLabel)})

(def flat-model-schema
  [element-schema])

;; Each top-level menu item is represented by a single nested-model-schema. Thus, a menubar with sub-menus "Project",
;; "File", "Help" will be represented by 3 separate nested-models
(def nested-model-schema
  {:model element-schema
   :children [s/recursive #'nested-model-schema]})

(defn menu-view
  "Component representing a submenu. A full view consists of multiple nested menu-view components."
  [args]
  (let [showing? (reagent/atom nil)
        hover? (reagent/atom nil)]
    (fn [{:keys [model children] :as args}]
      (let [{:keys [data] :as model} (deref-or-value model)
            children (deref-or-value children)
            has-children? (not-empty children)]
        [v-box
         :class "menu-view"
         :attr {:on-mouse-enter (handler-fn
                                 (reset! hover? true)
                                 (when has-children?
                                   (reset! showing? true)))
                :on-mouse-leave (handler-fn
                                 (reset! hover? false)
                                 (when has-children?
                                   (reset! showing? false)))}
         :children [[popover-anchor-wrapper
                     :showing? showing?
                     :position :below-right
                     :anchor
                     [h-box
                      :class "menu-view-header"
                      :style (when @hover?
                               {:border-bottom "1px solid black"
                                :margin-bottom "-1px"})
                      :children [[v-box
                                  :class "menu-view-element"
                                  :children [[menu-label data]]]]]
                     :popover
                     [popover-content-wrapper
                      :showing? showing?
                      :position :below-right
                      ;; important, do not define an on-cancel handler, otherwise a backdrop gets created that covers
                      ;; the whole viewport and messes up the hover handling
                      :arrow-length 0
                      :body [v-box
                             :children children]]]]]))))

;; notice the striking similarity to parsimony.views.tree-view/tree-parent
(defn menubar
  "Top-level menu component"
  [args]
  (letfn [(root-menu-view [{:keys [element children] :as nested-model}]
            [menu-view
             {:model element
              :children (for [{:keys [element] :as c} children]
                            ^{:key (:id element)}
                            [root-menu-view c])}])]
    (fn [{:keys [model] :as args}]
      (let [model (deref-or-value model)]
        [h-box
         :align :center
         :justify :start
         :gap "15px"
         :children (for [[idx menu-model] (map-indexed vector (remove nil? model))]
                       ^{:key idx}
                     [box :child [root-menu-view (tree-view/nested-model (into [] (remove nil?) menu-model))]])]))))
