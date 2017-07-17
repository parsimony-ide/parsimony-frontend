(ns parsimony.models.token-dashboard
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [parsimony.views.tree-view :as tree-view]
            [schema.core :as s :include-macros true]
            [parsimony.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def schema
  {:samples tree-view/flat-model-schema
   :package-cache s/Any ;; cache for storing server-side package data
   :layout {:left s/Num}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ITokenDashboardElement)

(defprotocol ITokenCategory)

(defrecord Heading [id label-str md-icon-name editable? horizon])
(cljs.reader/register-tag-parser! "parsimony.models.token-dashboard.Heading" map->Heading)

(defn heading [id label-str md-icon-name editable? horizon]
  (Heading. id label-str md-icon-name editable? horizon))

(defrecord Sample [id string horizon example-strings])
(cljs.reader/register-tag-parser! "parsimony.models.token-dashboard.Sample" map->Sample)

(defn sample
  ([id string]
   (sample id string nil))
  ([id string horizon]
   (sample id string horizon {}))
  ([id string horizon example-strings]
   (Sample. id string horizon example-strings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defaults
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the default-model must have a root since a nested-model cannot be constructed from an empty list of nodes
(def default-samples
  [{:id 0 :parent nil :draggable false :data (heading 0 "Samples" "zmdi-format-list-bulleted" false nil)}])

(def default-layout
  {:left 30})

(def default-model
  {:samples default-samples
   :package-cache {}
   :layout default-layout})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defn all-samples [{:keys [samples] :as state}]
  (for [element samples :when (not (satisfies? ITokenCategory (:data element)))]
    (:data element)))

(defn all-categories [{:keys [samples] :as state}]
  (for [element samples :when (satisfies? ITokenCategory (:data element))]
    (:data element)))

(defn all-sample-ids [state]
  (map :id (all-samples state)))

(defn all-category-ids [state]
  (map :id (all-categories state)))

(defn is-sample-id? [state element-id]
  (contains? (set (all-sample-ids state)) element-id))

(defn is-category-id? [state element-id]
  (contains? (set (all-category-ids state)) element-id))

(defn all-ids [{:keys [samples] :as state}]
  (map :id samples))

(defn id-exists? [state id]
  (contains? (set (all-ids state)) id))

(defn get-sample [state sample-id]
  (tree-view/get-element-attr (:samples state) sample-id [:data]))

(defn get-sample-string [state sample-id]
  (:string (get-sample state sample-id)))

(defn get-sample-horizon [state sample-id]
  (:horizon (get-sample state sample-id)))

(defn collapse-category [state element-id]
  (update state :samples tree-view/collapse-element element-id))

(defn uncollapse-category [state element-id]
  (update state :samples tree-view/uncollapse-element element-id))

(defn toggle-category [state element-id]
  (update state :samples tree-view/toggle-collapse-element element-id))

(defn move [state source-element-id target-element-id idx]
  (update state :samples tree-view/change-position-by-id source-element-id target-element-id idx))

(defn deselect-all [state]
  (update state :samples tree-view/deselect-all))

(defn select [state element-id]
  (update state :samples tree-view/exclusively-select-element element-id))

(defn get-selected-id [state]
  (first (tree-view/get-selected-element-ids (:samples state))))

(defn get-nearest-selected-category-id
  "If the selected element is a category, then return its id.  If the selected
   element is a sample, then return the id of its closest ancestor category.
   Otherwise, return 0."
  [state]
  (if-let [selected-id (get-selected-id state)]
    (if (satisfies? ITokenCategory (get-sample state selected-id))
      selected-id
      (if-let [tree-elem (tree-view/get-element (:samples state) selected-id)]
        (:parent tree-elem)
        0))
    0))

(defn add-sample [state sample-id string]
  (if (seq string)
    (if-not (id-exists? state sample-id)
      (update state :samples
              tree-view/add-element
              {:id sample-id
               :parent (get-nearest-selected-category-id state)
               :targetable false
               :data (sample sample-id string)})
      (do (console/warn ::add-sample :already-contains-sample-id {:sample-id sample-id})
          state))
    (do (console/warn ::add-sample :string-is-empty)
        state)))

(defn set-horizon [state sample-id horizon]
  (update state :samples tree-view/set-element-attr sample-id [:data :horizon] horizon))

(defn delete-sample [state element-id]
  (update state :samples tree-view/remove-element element-id))

(defn delete-all-samples [state]
  (assoc state :samples default-samples))

(defn add-category [state parent-id heading-id]
  (if-not (id-exists? state heading-id)
    (update state :samples
            tree-view/add-element
            {:id heading-id
             :parent parent-id
             :targetable true
             :collapsed true
             :data (heading heading-id "<New Category>" "zmdi zmdi-folder" true nil)})
    (do (console/warn ::add-category :already-contains-sample-id {:heading-id heading-id})
        state)))

(defn delete-category [state element-id]
  (let [dead-elements (-> (:samples state)
                          (tree-view/get-descendant-ids element-id)
                          (conj element-id))]
    (update state :samples tree-view/remove-elements dead-elements)))

(defn rename-category [state element-id new-name]
  (update state :samples tree-view/set-element-attr element-id [:data :label-str] new-name))

(defn get-package-cache [state]
  (:package-cache state))

(defn set-package-cache [state packages]
  (assoc state :package-cache packages))

(defn set-example-strings [state element-id path strings]
  (update state :samples tree-view/set-element-attr element-id [:data :example-strings path] strings))

(defn clear-all-example-strings [state]
  (reduce
    (fn [state id]
      (update state :samples tree-view/set-element-attr id [:data :example-strings] {}))
    state
    (all-ids state)))

(defn source-str-rhs [source-str]
  (second (str/split (str source-str) #" = " 2)))

(defn get-category-label
  [state element-id]
  (when (not= 0 element-id)
    (when-let [sample (get-sample state element-id)]
      (when (satisfies? ITokenCategory sample)
        (:label-str sample)))))

(defn get-nested-sample-strings
  [state element-id]
  (when (is-category-id? state element-id)
    (let [sample-ids
          (set/intersection (set (tree-view/get-descendant-ids (:samples state) element-id))
                            (set (all-sample-ids state)))
          strings (map (partial get-sample-string state) sample-ids)]
      (set strings))))

(defn pristine
  "Return pristine version of state appropriate for serialization"
  [state]
  (-> state
      (update :samples tree-view/deselect-all)
      (clear-all-example-strings)))
