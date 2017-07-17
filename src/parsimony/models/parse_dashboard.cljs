(ns parsimony.models.parse-dashboard
  (:require [schema.core :as s :include-macros true]
            [parsimony.lexer :as lexer]
            [parsimony.util :refer [matches-schema? pprint-str dissoc-in-keep-empty]]
            [parsimony.views.tree-view :as tree-view]
            [parsimony.models.editor :refer [->char-range]]
            [parsimony.models.lexer :refer [token-schema]]
            [parsimony.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schemas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def label-schema {:nt s/Keyword
                   :char-from s/Num
                   :char-to s/Num
                   :type (s/enum :positive :negative)
                   :label-id s/Num})

(def status-schema {:label-status {s/Num {:passing? (s/enum :pass :fail)
                                          (s/optional-key :positive-failures) s/Any
                                          (s/optional-key :negative-failures) s/Any
                                          (s/optional-key :forest) s/Any}}
                    :tokens (s/pred sequential?)})

(def sample-schema {:sample-id s/Num         ;; a unique identifier for this sample
                    :source-id s/Num         ;; the source from which this sample was derived
                    :source-path s/Str       ;; snapshot of the source-path at the time the sample was taken
                    :string s/Str            ;; string inside char-range at the time of sample
                    :labels [label-schema]})

(def schema {:samples tree-view/flat-model-schema ;; holds tree of samples
             :sample-status {s/Num status-schema} ;; map from sample-id to status of sample
             :sample-editor-ids {s/Num s/Num}     ;; map from sample-id to backing editor-id
             :layout {:left s/Num                 ;; size of left splitter
                      :center s/Num               ;; size of center splitter
                      :top s/Num                  ;; size of the top splitter
                      }
             :selected {:label-id (s/maybe s/Num)
                        :sample-id (s/maybe s/Num)} ;; the selected sample and/or label, if any
             })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Sample [sample-id source-id source-path string labels])
(cljs.reader/register-tag-parser! "parsimony.models.parse-dashboard.Sample" map->Sample)

(defn sample [sample-id source-id source-path string labels]
  (Sample. sample-id source-id source-path string labels))

(defrecord Heading [])
(cljs.reader/register-tag-parser! "parsimony.models.parse-dashboard.Heading" map->Heading)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defaults
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def default-model
  {:samples [{:id 0 :parent nil :draggable false :data (Heading.)}]
   :sample-status {}
   :sample-editor-ids {}
   :layout {:left 15 :center 15 :top 50}
   :selected {:label-id nil :sample-id nil}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn existing-sample
  "If a sample with the same source-id and string already exists, then return it, otherwise nil"
  [{:keys [samples] :as state} {:keys [source-id string] :as sample}]
  (:data (first (filter #(and (= (get-in % [:data :source-id]) source-id)
                              (= (get-in % [:data :string]) string))
                        samples))))

(defn merge-samples [s1 s2]
  #_(console/debug :merge-samples (pprint-str {:s1 s1 :s2 s2}))
  (letfn [(ensure-distinct [labels]
            ;; ensure there are no duplicate labels, where two labels are duplicates
            ;; if they share equivalent values for all keys except :label-id
            (vals
              (reduce
                (fn [m label]
                  (let [label' (dissoc label :label-id)]
                    (if (contains? m label')
                      (do (console/debug :merge-samples :duplicate-label label)
                          m)
                      (assoc m label' label))))
                {}
                (sort-by :label-id labels))))]
    (assoc s2 :labels (vec (sort-by :char-from (ensure-distinct (concat (:labels s1) (:labels s2))))))))

(defn add-sample
  [state sample]
  (console/debug :add-sample (pprint-str sample))
  (if-let [s (existing-sample state sample)]
    (do (console/error :add-sample :sample-already-exists (pprint-str s))
        state)
    (update state :samples tree-view/add-element {:id (:sample-id sample)
                                                  :parent 0
                                                  :targetable false
                                                  :data sample})))

(defn update-sample [state sample]
  (console/debug :update-with-sample (pprint-str sample))
  (if-let [s (existing-sample state sample)]
    (update state :samples tree-view/update-element-attr (:sample-id s) [:data] (partial merge-samples sample))
    (do (console/error :update-with-sample :sample-does-not-exist)
        state)))

(defn get-sample [state sample-id]
  (tree-view/get-element-attr (:samples state) sample-id [:data]))

(defn get-label [state sample-id label-id]
  (when-let [sample (get-sample state sample-id)]
    (first (filter #(= label-id (:label-id %)) (:labels sample)))))

(defn get-underlying-sample-editor-id [state sample-id]
  (get-in state [:sample-editor-ids sample-id]))

(defn sample->overlays [{:keys [labels] :as sample}]
  (let [positive (filter #(= :positive (:type %)) labels)
        negative (filter #(= :negative (:type %)) labels)]
    (letfn [(f [labels]
              (reduce
               (fn [acc {:keys [nt char-from char-to]}]
                 (update acc (name nt) (fnil conj []) (->char-range char-from char-to)))
               {}
               (sort-by :char-from labels)))]
      {:positive (f positive)
       :negative (f negative)})))

(defn add-label [state sample-id label]
  {:pre [(matches-schema? label-schema label)]}
  (if-let [sample (get-sample state sample-id)]
    (update-sample state (assoc sample :labels [label]))
    (do (console/error :add-label :sample-does-not-exist {:sample-id sample-id})
        state)))

(defn delete-label [state sample-id label-id]
  (update state :samples tree-view/update-element-attr sample-id [:data :labels]
          (fn [labels]
            (into []
                  (remove #(= label-id (:label-id %)))
                  labels))))

(defn all-samples
  "Return lazy seq of all samples"
  [state]
  (for [element (:samples state)
        :let [data (:data element)]
        :when (some? (:sample-id data))]
    (:data element)))

(defn all-sample-ids
  "Return lazy seq of all sample ids"
  [state]
  (map :sample-id (all-samples state)))

(defn all-labels [state]
  (into []
        (mapcat :labels)
        (all-samples state)))

(defn negative-labels-for [state sample-id]
  (when-let [sample (get-sample state sample-id)]
    (filter #(= :negative (:type %)) (:labels sample))))

(defn positive-labels-for [state sample-id]
  (when-let [sample (get-sample state sample-id)]
    (filter #(= :positive (:type %)) (:labels sample))))

(defn all-labels-for [state sample-id]
  (when-let [sample (get-sample state sample-id)]
    (:labels sample)))

(defn sample-label->token-indexed-label
  "Convert a sample label whose indices are char-indices to a label of form [nt i l] where i is a token-index, and l is a token
  length"
  [tokens {:keys [nt char-from char-to] :as label}]
  (let [[i l] (lexer/inexact-char-range->token-range tokens char-from char-to)]
    [nt i l]))

(defn nested-sample-labels [sample label]
  "Return sequence of labels that are nested under the given label"
  (let [{:keys [char-from char-to]} label]
    (into []
          (comp (remove (partial = label))
                (filter (fn [{from :char-from to :char-to}]
                          (and (<= char-from from to char-to)
                               ;; must be strictly nested, not identical range
                               (or (not= char-from from)
                                   (not= char-to to))))))
          (:labels sample))))

(defn select-sample [state sample-id]
  (console/debug :parse-dashboard :select-sample sample-id)
  (assoc state :selected {:sample-id sample-id :label-id nil}))

(defn select-label [state sample-id label-id]
  (console/debug :parse-dashboard :select-label sample-id label-id)
  (assoc state :selected {:sample-id sample-id :label-id label-id}))

(defn clear-sample-status [state sample-id]
  (update state :sample-status dissoc sample-id))

(defn clear-label-status [state sample-id label-id]
  (if (some? (get-in state [:sample-status sample-id]))
    (update-in state
               [:sample-status sample-id]
               dissoc-in-keep-empty
               [:label-status label-id])
    state))

(defn clear-all-status [state]
  (assoc state :sample-status (:sample-status default-model)))

(defn pristine
  "Return pristine version of state appropriate for serialization"
  [state]
  (-> state
      (update :samples tree-view/deselect-all)
      (clear-all-status)))
