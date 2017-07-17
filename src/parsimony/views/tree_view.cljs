(ns parsimony.views.tree-view
  (:require [clojure.set :as set]
            [parsimony.com.box :refer [draggy-v-box]]
            [parsimony.util :refer [matches-schema? get-all-drag-data set-drag-data get-drag-types set-drop-effect filter-not pprint-str find-first find-first-index]]
            [parsimony.views.common :refer [icon]]
            [re-com.core :refer [box gap h-box label title v-box] :refer-macros [handler-fn]]
            [re-com.util :refer [deref-or-value]]
            [re-com.validate :refer [css-style? html-attr?] :refer-macros [validate-args-macro]]
            [reagent.core :as reagent]
            [schema.core :as s :include-macros true]
            [parsimony.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree Label Protocol
;; - data that should be viewable in a tree view must implement ITreeLabel
;; - additionally, since tree labels ultimately become drag and drop data,
;;   any user-defined implementations of ITreeLabel must have a corresponding
;;   tag parser registered with the reader.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ITreeLabel
  (tree-label [this] "Convert this into a string or hiccup for rendering a tree node label"))

;; default implementations for various simple types

(extend-type nil
  ITreeLabel
  (tree-label [_] [:span]))

(extend-type string
  ITreeLabel
  (tree-label [s] [:span s]))

(extend-type Keyword
  ITreeLabel
  (tree-label [kw] [:code (str kw)]))

(extend-type number
  ITreeLabel
  (tree-label [n] [:span (str n)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree View
;; - a tree view is essentially agnostic to the data presented within
;; - the subwidgets that implement each tree element should handle
;;   domain-specific stuff like click/drag/blur events and presentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def element-schema {:id s/Any                           ;; element id, not the same as a source id
                     :parent (s/maybe s/Num)             ;; only root has nil parent
                     (s/optional-key :selected) s/Bool   ;; is this element selected?, default: false
                     (s/optional-key :collapsed) s/Bool  ;; is this element collapsed?, default: false
                     (s/optional-key :draggable) s/Bool  ;; can be dragged?, default: true
                     (s/optional-key :targetable) s/Bool ;; can be the target of a drop?, default: true
                     :data (s/protocol ITreeLabel)})     ;; arbitrary data describing the element, must implement ITreeLabel so it can be visualized

(def tree-view-args-desc
  [{:name :model :required true :type "element-schema instance | atom"
    :validate-fn (fn [model] (matches-schema? element-schema (deref-or-value model)))}
   {:name :children :required true :type "vector" :validate-fn sequential?
    :description "a vector of components"}
   {:name :drag-type :required true :type "keyword" :validate-fn keyword?}
   {:name :valid-drop-fn :required false :type "drag-data -> element -> integer -> boolean"
    :validate-fn fn?
    :description "Compute whether this is an acceptable drop target. First
                  argument is the drop data. Second argument is the model for
                  this tree-view.  Third argument is the index into the child
                  vector."}
   {:name :valid-drag-type-fn :required false :type "set of drag-types -> element -> integer -> boolean"
    :validate-fn fn?
    :description "Compute whether to show drop indicator based on the
                  drag-types of the element being dragged.  First argument is
                  the set of drag-types. Second argument is the model for this
                  tree-view.  Third argument is the index into the child
                  vector."}
   {:name :on-toggle :required false :type "element -> nil" :validate-fn fn?
    :description "On toggle callback"}
   {:name :on-move :required false :type "element -> element -> id -> nil"
    :validate-fn fn?
    :description "Called whenever a tree element is moved (e.g., drag and
                  drop).  Arguments are source, target, and location in
                  list."}])

(defn tree-view
  "Component representing a subtree. A full view consists of multiple nested tree-view components."
  [args]
  {:pre [(validate-args-macro tree-view-args-desc args "tree-view")]}
  (let [hover (reagent/atom nil)
        drag-hover (reagent/atom nil)]
    (fn [{:keys [model children drag-type draggable valid-drop-fn valid-drag-type-fn on-toggle on-move]
          :as args}]
      (let [{:keys [data selected collapsed draggable targetable]
             :or {selected false
                  collapsed false
                  draggable true
                  targetable true}
             :as model} (deref-or-value model)
            valid-drop-fn (if valid-drop-fn valid-drop-fn (constantly true))
            valid-drag-type-fn (if valid-drag-type-fn valid-drag-type-fn (constantly true))]
        [v-box
         :gap "2px"
         :padding "0 0 0 0px"
         ;; :style (when @hover
         ;;          {:background-color "#eaeaea"
         ;;           :border-right "4px solid #d0d0d0"})
         :children
         [[h-box
           :size "auto"
           :align :center
           :class "tree-view-header"
           :attr {:on-mouse-enter (handler-fn
                                   (reset! hover true))
                  :on-mouse-leave (handler-fn
                                   (reset! hover nil))}
           :children [(if-not (seq children)
                        [:div {:style {:width "19px"
                                       :height "1px"
                                       :border-top "1px dotted #bbb"
                                       :flex "none"}}]
                        #_[gap :size "14px"]
                        [box
                         :class (str "tree-view-arrow " (if collapsed "collapsed" "open"))
                         :style {:margin-left "5px"}
                         :child "▾"
                         :attr {:on-click
                                (when on-toggle
                                  (handler-fn (on-toggle model)))}])
                      [v-box
                       :size "auto"
                       :class (str "tree-view-element "
                                   (when selected "selected ")
                                   (when @drag-hover "drag-hovered "))
                       :attr {:draggable draggable
                              :on-drag-over (when targetable
                                              (handler-fn
                                                (.preventDefault event)
                                                (.stopPropagation event)
                                                (if (valid-drag-type-fn (get-drag-types event) model 0)
                                                  (do (set-drop-effect event :copy)
                                                      (reset! drag-hover true))
                                                  (set-drop-effect event :none))))
                              :on-drag-leave (when targetable
                                              (handler-fn
                                                (reset! drag-hover nil)))
                              ;; dropping onto a tree-view-element is the same as moving to the 0th child position with
                              ;; one exception: do not perform a move if the dragged element is already a child of the
                              ;; drop target
                              :on-drop (when targetable
                                         (handler-fn
                                           (let [data (get-all-drag-data event)]
                                             (console/debug ::on-drop {:all-drag-data data})
                                             (reset! drag-hover nil)
                                             (when (valid-drop-fn data model 0)
                                               (when-let [tree-elem (get data drag-type)]
                                                 (when (and on-move
                                                            (matches-schema? element-schema tree-elem)
                                                            (not (= (:parent tree-elem) (:id model))))
                                                   (on-move tree-elem model 0)))))))
                              :on-drag-start (handler-fn
                                               (console/debug ::on-drag-start {:drag-type drag-type})
                                               (doto event
                                                 (set-drag-data drag-type model)))}
                       :children [[tree-label data]]]]]
          (when (or targetable
                    (not-empty children))
            [draggy-v-box
             {:class (str "tree-view-children " (if collapsed "collapsed" "open"))
              :draggy targetable
              :gap "3px"
              :valid-drop-fn (when valid-drop-fn
                               (fn [data i]
                                 (valid-drop-fn data model i)))
              :valid-drag-type-fn (when valid-drag-type-fn
                                    (fn [drag-types i]
                                      (valid-drag-type-fn drag-types model i)))
              :margin "0 0 0 30px"
              :padding "0 0 10px 0" ;; add some space on the bottom so drop targets don't overlap
              :on-gap-drop (fn [data i]
                             (when on-move
                               (when-let [tree-elem (get data drag-type)]
                                 (on-move tree-elem model i))))
              :children children}])]]))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree Parent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def flat-model-schema
  [element-schema])

(def nested-model-schema
  {:element element-schema
   :children [s/recursive #'nested-model-schema]})

(defn- parents-map [model]
  "Return map from parent id to sets of child elements"
  (let [f (fn [acc {:keys [parent] :as element}]
            (update-in acc [parent] (fnil conj []) element))] ;; use a vector, not a set, to preserve ordering
    (reduce f {} model)))

(defn- roots [pm]
  (get pm nil))

(defn- children-of [pm element]
  (get pm (:id element)))

(defn- check-not-forest
  "Exactly 1 root"
  [pm]
  (when (> (count (roots pm)) 1)
    (throw (ex-info "Input has more than one root" {:parents-map pm})))
  (when (< (count (roots pm)) 1)
    (throw (ex-info "Input has not roots" {:parents-map pm})))
  true)

(defn- distinct-elements
  "No repeated element ids"
  [model]
  (apply distinct? (map :id model)))

(defn- pm->nested-model [pm]
  (check-not-forest pm)
  (letfn [(build-node [element children]
            {:element element :children (vec children)})
          (build-tree [root]
            (build-node root
                        (map build-tree (get pm (:id root)))))]
    (build-tree (first (get pm nil)))))

(defn- nested-model
  "Convert a flat vector of elements into a tree structure based on :id and :parent keys in each element."
  [model]
  {:pre [(seq model)
         (distinct-elements model)]}
  (let [pm (parents-map model)]
    (pm->nested-model pm)))

(def tree-parent-args-desc
  [{:name :model :require true :type "vector of elements | atom"
    :validate-fn (fn [model] (matches-schema? flat-model-schema (deref-or-value model)))
    :description "A vector or atom containing a vector of element-schema instances"}
   {:name :drag-type :require false :default ::element :type "keyword"
    :validate-fn keyword?
    :description "Type to associate with data when an element is dragged"}
   {:name :valid-drop-fn :require false :type "drag-data -> element -> integer -> boolean"
    :validate-fn fn?
    :description "Compute whether this is an acceptable drop target. First
                  argument is the drop data. Second argument is the model for
                  this tree-view.  Third argument is the index into the child
                  vector."}
   {:name :valid-drag-type-fn :require false :type "set of drag-types -> element -> integer -> boolean"
    :validate-fn fn?
    :description "Compute whether to show drop indicator based on the
                  drag-types of the element being dragged.  First argument is
                  the set of drag-types. Second argument is the model for this
                  tree-view.  Third argument is the index into the child
                  vector."}
   {:name :on-toggle :require false :type "element -> nil"
    :validate-fn fn?
    :description "Called whenever a tree view arrow is clicked"}
   {:name :on-move :require false :type "element -> element -> id -> nil"
    :validate-fn fn?
    :description "Called whenever a tree element is moved (e.g., drag and drop)"}
   {:name :style :require false :type "CSS style map"
    :validate-fn css-style?
    :description "CSS styles"}
   {:name :attr :require false :type "HTML attr map"
    :validate-fn html-attr?
    :description "HTML attributes"}])

(defn tree-parent
  "Component representing a full tree view. Maintains the state associated with a nested tree-view hierarchy."
  [{:keys [model drag-type valid-drop-fn valid-drag-type-fn on-toggle on-move]
    :or {drag-type ::element}
    :as args}]
  {:pre [(validate-args-macro tree-parent-args-desc args "tree-parent")]}
  #_(console/debug ::tree-parent :remount)
  (letfn [(root-tree-view [{:keys [element children] :as nested-model}]
            [tree-view
             {:model element
              :drag-type drag-type
              :valid-drop-fn valid-drop-fn
              :valid-drag-type-fn valid-drag-type-fn
              :on-toggle on-toggle
              :on-move on-move
              :children (for [{:keys [element] :as c} children]
                          ^{:key (:id element)}
                          [root-tree-view c])}])]
    ;; Need this to be a fn, otherwise the entire tree-parent and its children get remounted every time! This is because
    ;; otherwise the root-tree-view helper function gets redefined on every re-render, thus causing all component
    ;; identity checks to come up false.
    (fn [{:keys [model style attr] :as args}]
      (let [model (deref-or-value model)]
        [v-box
         :class "tree-parent"
         :style style
         :attr attr
         :children [[root-tree-view (nested-model model)]]]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree Manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- in-order-seq [nm]
  (tree-seq
   (fn [node] (seq (:children node)))
   :children
   nm))

(defn nested-model->flat-model
  "Convert a nested-model as created by the nested-model function into a flat model accepted by tree-parent"
  [nm]
  (vec (map :element (in-order-seq nm))))

(defn x2 [x]
  (* 2 x))

(defn- pm-ancestors
  "Return set of ancestor ids of element with id element-id, given parents-map pm"
  [pm element-id]
  (let [cm (->> (for [[pid children] pm {cid :id} children :when (not= pid nil)]
                  [cid pid])
                (into {}))
        ancestors (loop [xid element-id seen #{}]
                    (if-let [pid (get cm xid)]
                      (recur pid (conj seen pid))
                      seen))]
    #_(console/debug ::pm-ancestors {:pm pm
                                     :cm cm
                                     :ancestors ancestors
                                     :element-id element-id})
    ancestors))

(defn- valid-move? [pm source new-parent idx]
  ;; - if source is an ancestor of target, then reject
  ;; - if source and new-parent are the same, then reject
  (let [invalid (or (contains? (pm-ancestors pm (:id new-parent)) (:id source))
                    (= (:id source) (:id new-parent)))]
    (when invalid
      (console/warn ::valid-move?
                    :invalid
                    {:pm pm
                     :source source
                     :new-parent new-parent
                     :idx idx}))
    (not invalid)))

(defn change-position
  "Move source element to new-parent at child position idx. new-parent can be
  the same as source's old parent, in which case it simply changes child
  position."
  [model source new-parent idx]
  #_(console/debug ::change-position
                   {:source source
                    :new-parent new-parent
                    :idx idx})
  (let [pm (parents-map model)]
    (if-not (valid-move? pm source new-parent idx)
            model
            (let [sid (:parent source)
                  tid (:id new-parent)
                  source-with-new-parent (assoc source :parent tid)
                  ;; changing the target parent
                  indexed-target-children (->> (map vector (map (comp inc x2) (range)) (get pm tid)) ;; indexed starting at 1, steps of 2
                                               (filter-not #(= source (second %)))) ;; remove self so the list doesn't have a duplicate
                  new-target-children (->> (conj indexed-target-children [(x2 idx) source-with-new-parent]) ;; add source to indexed list
                                           (sort-by first) ;; sort by indices
                                           (map last)) ;; remove indices
                  new-pm (if-not (= sid tid)
                           (assoc pm
                                  sid (filter-not #(= source %) (get pm sid))
                                  tid new-target-children)
                           (assoc pm
                                  tid new-target-children))]
              (-> new-pm (pm->nested-model) (nested-model->flat-model))))))

(defn contains-element? [model element-id]
  (some? (find-first-index #(= (:id %) element-id) model)))

(defn get-element-idx [model element-id]
  (if-let [idx (find-first-index #(= (:id %) element-id) model)]
    idx
    (do
      (console/error ::get-element-idx :no-element-with-id {:element-id element-id})
      nil)))

(defn get-element [model element-id]
  (when-let [idx (get-element-idx model element-id)]
    (nth model idx)))

(defn get-ancestor-ids [model element-id]
  (pm-ancestors (parents-map model) element-id))

(defn get-descendant-ids [model element-id]
  (let [nm (nested-model model)
        subtree (find-first #(= element-id (get-in % [:element :id])) (in-order-seq nm))
        desc-nodes (mapcat in-order-seq (:children subtree))]
    (when (seq desc-nodes)
      (into []
            (map #(get-in % [:element :id]))
            desc-nodes))))

(defn get-children [model element-id]
  (when-let [element (get-element model element-id)]
    (into []
          (children-of (parents-map model) element))))

(defn get-child-ids [model element-id]
  (when-let [element (get-element model element-id)]
    (into []
          (map :id)
          (children-of (parents-map model) element))))

(defn set-element-attr [model element-id path v]
  (if-let [idx (get-element-idx model element-id)]
    (assoc-in model (into (vector idx) path) v)
    model))

(defn update-element-attr [model element-id path f]
  (if-let [idx (get-element-idx model element-id)]
    (update-in model (into (vector idx) path) f)
    model))

(defn get-element-attr [model element-id path]
  (when-let [element (get-element model element-id)]
    (get-in element path)))

(defn add-element
  [model {parent-id :parent :as element}]
  (if-let [parent-element (get-element-idx model parent-id)]
    (conj model element)
    model))

(defn- remove-one-element
  "Removes the given element.  Does not ensure that orphans get new parents, so use with caution."
  [model element-id]
  (into []
        (filter #(not= element-id (:id %)))
        model))

(defn remove-element
  "Remove an element from the model. If this would result in an orphan element, then make orphan's new parent the parent
  of the element being removed (i.e., make orphan's parent its old grandparent)"
  [model element-id]
  (if-let [{parent-id :parent :as element} (get-element model element-id)]
    (if-let [child-ids (get-child-ids model element-id)]
      ;; has children, update all children with grandparent
      (-> (reduce (fn [acc x] (set-element-attr acc x [:parent] parent-id)) model child-ids)
          (remove-one-element element-id))
      ;; no children, just remove me
      (remove-one-element model element-id))
    ;; element does not exist, do nothing
    model))

(defn remove-elements
  "Remove one or more elements by id"
  [model element-ids]
  (reduce remove-element model element-ids))

(defn collapse-element
  "Collapse the given element."
  [model element-id]
  (set-element-attr model element-id [:collapsed] true))

(defn uncollapse-element
  "Uncollapse the given element."
  [model element-id]
  (set-element-attr model element-id [:collapsed] false))

(defn toggle-collapse-element
  "Toggle the collapse state of the given element."
  [model element-id]
  (update-element-attr model element-id [:collapsed] (fnil not false)))

(defn toggle-select-element
  "Toggle the select state of the given element."
  [model element-id]
  (update-element-attr model element-id [:selected] (fnil not false)))

(defn select-element
  "Set the :selected state of the given element to true."
  [model element-id]
  (set-element-attr model element-id [:selected] true))

(defn deselect-element
  "Set the :selected state of the given element to false."
  [model element-id]
  (set-element-attr model element-id [:selected] false))

(defn deselect-elements
  [model element-ids]
  (reduce deselect-element model element-ids))

(defn all-element-ids
  [model]
  (map :id model))

(defn deselect-all
  [model]
  (deselect-elements model (all-element-ids model)))

(defn get-selected-element-ids
  [model]
  (into []
        (comp
         (filter :selected)
         (map :id))
        model))

(defn exclusively-toggle-select-element
  "Toggle the select state of the given element. If any other elements are select, deselect them first."
  [model element-id]
  (let [model (toggle-select-element model element-id)
        elements-to-deselect (disj (set (get-selected-element-ids model)) element-id)]
    (deselect-elements model elements-to-deselect)))

(defn exclusively-select-element
  "Select the given element. If any other elements are select, deselect them first."
  [model element-id]
  (let [model (select-element model element-id)
        elements-to-deselect (disj (set (get-selected-element-ids model)) element-id)]
    (deselect-elements model elements-to-deselect)))

(defn change-position-by-id
  "Change the position of an element by its id"
  [model source-element-id target-element-id idx]
  {:pre [(number? idx)]}
  (let [source-element (find-first #(= (:id %) source-element-id) model)
        target-element (find-first #(= (:id %) target-element-id) model)]
    (change-position model source-element target-element idx)))
