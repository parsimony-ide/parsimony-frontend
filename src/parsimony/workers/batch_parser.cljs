(ns parsimony.workers.batch-parser
  (:refer-clojure :exclude [-reset])
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [parsimony.asm.parser :as asm.parser]
            [parsimony.asm.parser-recovery :as asm.parser-recovery]
            [parsimony.dag :as dag]
            [parsimony.parser :as parser]
            [parsimony.parser-recovery :as parser-recovery]
            [parsimony.query :as q]
            [parsimony.util :refer [matches-schema? pprint-str]]
            [parsimony.worker :refer [IWorker ISyncWorker step reset status render cache-key current-progress max-progress progress-description]]
            [parsimony.workers.common :refer [source-hyperlink] :as workers.common]
            [parsimony.workers.compile-lexer :as compile-lexer]
            [parsimony.workers.compile-parser :as compile-parser]
            [parsimony.workers.batch-lexer :as workers.batch-lexer]
            [re-frame.core :refer [dispatch]]
            [schema.core :as s :include-macros true]
            [parsimony.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def schema
  (merge workers.common/schema
         {:token-editor-id s/Num
          :cfg-editor-id s/Num
          :target-editor-id s/Num
          :target-tokens (s/maybe s/Any)
          :target-source-string (s/maybe s/Str)
          :compiled-parser (s/maybe s/Any)
          :stage (s/maybe (s/pred :name))
          :result (s/maybe {(s/optional-key :success) s/Any
                            (s/optional-key :error) s/Any})}))

(defn verbose-matches-schema?
  [a-schema this]
  (matches-schema? a-schema this {:worker :batch-parser
                                  :status (status this)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ->cache-key [token-editor-id cfg-editor-id target-editor-id]
  {:algo :batch-parser
   :token-editor-id token-editor-id
   :cfg-editor-id cfg-editor-id
   :target-editor-id target-editor-id})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti -step (fn [this db]
                  {:pre [(verbose-matches-schema? schema this)]}
                  (let [s (status this)]
                    (console/debug ::-step s)
                    s)))

(defmethod -step :default [this _]
  (console/error ::-step :unrecognized-status this)
  (assoc this :-status :failure))

(defmethod -step :idle [{:keys [token-editor-id cfg-editor-id target-editor-id] :as this} db]
  (let [l-cache-key (workers.batch-lexer/->cache-key token-editor-id target-editor-id)]
    (if-let [l-worker (get-in db [:workers l-cache-key])]
      (let [cp-cache-key (compile-parser/->cache-key token-editor-id cfg-editor-id)]
        (if-let [cp-worker (get-in db [:workers cp-cache-key])]
          (do
            ;; free previously allocated heap space used by asm.parser
            (when-let [cyk (get-in this [:result :success :cyk])]
              (asm.parser/cpp-free cyk))
            (when-let [cyk (get-in this [:result :error :cyk])]
              (asm.parser/cpp-free cyk))

            (-> this
                (assoc :result nil) ;; discard old result
                (assoc :compiled-parser (get-in cp-worker [:result :success])
                       :target-tokens (get-in l-worker [:result :success])
                       :target-source-string (:target-source-string l-worker)
                       :-status :running
                       :stage {:name :init-cyk})))
          (do (console/error ::-step :idle (str "No compile-parser worker with cache key " cp-cache-key " found"))
              this)))
      (do (console/error ::-step :idle (str "No lexer worker with cache key " l-cache-key " found"))
          this))))

(declare run-stage)

(defmethod -step :running [this db]
  (run-stage this db))

(defmethod -step :success [this db]
  (step (assoc this :-status :idle) db))

(defmethod -step :failure [this db]
  (step (assoc this :-status :idle) db))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run Stage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti run-stage (fn [this _]
                      #_{:pre [(verbose-matches-schema? schema this)]}
                      (let [s (get-in this [:stage :name])]
                        (console/debug ::run-stage s)
                        s)))

(defmethod run-stage :default [this _]
  (console/error ::run-stage :unrecognized-stage this)
  (assoc this
         :-status :failure
         :stage nil))

;; -----------------------------------------------------------------------------
;; Init CYK
;; -----------------------------------------------------------------------------

(defmethod run-stage :init-cyk
  [{:keys [compiled-parser target-tokens] :as this} db]
  (let [result
        (try
          (let [{:keys [cyk codec exec-time]} (asm.parser/cpp-init-cyk (into [] (map :label) target-tokens) compiled-parser)]
            (console/debug ::cyk-runtime {:init exec-time})
            {:success {:cyk cyk :codec codec}})
          (catch js/Error e
            (console/error ::run-stage :init-cyk {:error e})
            {:error (ex-data e)}))]
    (assoc this
           :result result
           :-status (if (:error result) :failure :running)
           :stage (if (:error result) nil {:name :run-cyk}))))

(defmethod run-stage :run-cyk
  [{:keys [compiled-parser target-tokens result] :as this} db]
  (let [result
        (try
          (let [{:keys [cyk codec]} (:success result)
                cyk-time (asm.parser/cpp-run-cyk cyk)]
            (console/debug ::cyk-runtime {:cyk cyk-time})
            (if-let [start-nt (parser/start-symbol compiled-parser)]
              (let [token-vec (into [] (map :label) target-tokens)]
                (when-not (asm.parser/applicable? codec cyk token-vec start-nt 0 (count target-tokens))
                  (throw (ex-info "Parse failure" {:causes #{:parse-failure}
                                                   :cyk cyk
                                                   :codec codec}))))
              (throw (ex-info "No start symbol" {:causes #{:no-start-symbol}})))
            result)
          (catch js/Error e
            (console/error ::run-stage :run-cyk {:error e})
            {:error (ex-data e)}))]
    (assoc this
           :result result
           :-status (if (:error result) :failure :success)
           :stage nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare batch-parser-worker)

(defn- -reset [{:keys [token-editor-id cfg-editor-id target-editor-id] :as this}]
  (let [success-cyk (get-in this [:result :success :cyk])
        error-cyk (get-in this [:result :error :cyk])]
    (doseq [cyk [success-cyk error-cyk]]
      (when cyk
        (try
          (asm.parser/cpp-free cyk)
          (catch js/Error _
            (console/warn ::-reset "Free failed"))))))
  (batch-parser-worker token-editor-id cfg-editor-id target-editor-id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti -render (fn [this _]
                    (let [s (status this)]
                      (console/debug ::-render s)
                      s)))

(defmethod -render :default [this db]
  (console/error ::-render :unrecognized-status this)
  db)

(defn- string-index-map
  "Return map from string index to {:line :col}"
  [string]
  (let [[_ result]
        (reduce
          (fn [[curr v] c]
            [(if (= \newline c)
               (-> curr
                   (update :line inc)
                   (assoc :col 1))
               (update curr :col inc))
             (conj v curr)])
          [{:line 1 :col 1} []]
          (seq string))]
    (conj result :end-of-file)))

(defn- ->nested [forest node-fn]
  "Given a forest as output by parser/reconstruct, return its nested-vector
   representation.  Note that this can create very large output when the input
   forest is ambiguous"
  (let [g (parser/forest->dag forest)]
    (letfn [(build-tree [root]
              (if-let [children (seq (sort-by second (dag/successors g root)))]
                (into [(node-fn root)]
                      (map build-tree)
                      children)
                (node-fn root)))]
      (build-tree (first (dag/roots g))))))

(defn- format-forest
  "Return a formatted string representation of the given forest"
  [index-map tokens ambig-nodes forest]
  (let [tokens (vec tokens)
        ->line-col (fn [x]
                     (if (keyword? x)
                       x
                       (let [{:keys [line col]} x]
                         (symbol (str line ":" col)))))
        ->node-label (fn [[sym i l :as node]]
                       (let [inner [(symbol (name (parser/->nonterminal sym)))
                                    (->line-col (get index-map (:start (get tokens i))))
                                    (->line-col (get index-map (:end (get tokens (dec (+ i l))))))]
                             inner (if (parser/terminal? sym)
                                     (conj inner (:string (get tokens i)))
                                     inner)]
                         (if (contains? ambig-nodes node)
                           (apply list inner)
                           (vec inner))))
        nested (->nested forest ->node-label)]
    (pprint-str nested)))

(defn- split-forest
  "Split a forest at dummy nodes. Each returned subforest is guaranteed not to
   extend beyond ambiguity boundaries."
  [forest]
  (let [g (parser/forest->dag forest)
        ambig-nodes (parser/ambiguous-nodes forest)
        dummy-nodes (parser/dummy-nodes (parser/add-dummy-nodes forest))]
    ;; remove ambig-nodes from roots, since they'll be covered by a descendant
    ;; dummy node anyway
    (for [root (into (vec (remove ambig-nodes (dag/roots g)))
                     (filter (partial contains? dummy-nodes))
                     (dag/topological-sort g))]
      (-> g
          (dag/successor-subgraph [root] ambig-nodes)
          (parser/dag->forest)))))

(defmethod -render :success
  [{:keys [target-tokens target-source-string compiled-parser result] :as this} db]
  (let [{:keys [cyk codec]} (:success result)
        start-nt (parser/start-symbol compiled-parser)
        token-vec (into [] (map :label) target-tokens)
        forest (-> (asm.parser/reconstruct codec
                                           cyk
                                           token-vec
                                           compiled-parser
                                           start-nt 0 (count target-tokens))
                   (parser/disambiguate compiled-parser))
        index-map (string-index-map target-source-string)
        ambig-nodes (parser/ambiguous-nodes forest)]
    (dispatch [:log/info
               (into
                 [:div
                  [:span (str "Parser success. Computed " (count (parser/all-nodes forest)) " nodes with "
                              (count (parser/dummy-nodes (parser/add-dummy-nodes forest))) " ambiguities:")]]
                 (map (fn [forest]
                        [:pre.parse-forest
                         (format-forest index-map
                                        target-tokens
                                        ambig-nodes
                                        forest)])
                      (split-forest forest)))])
    db))

(defmethod -render :failure
  [{:keys [target-tokens target-source-string compiled-parser result] :as this} db]
  (let [{:keys [cyk codec causes]} (:error result)]
    (when (contains? causes :empty-token-stream)
      (dispatch [:log/error "Parser failure: no tokens"]))
    (when (contains? causes :parse-failure)
      (let [token-vec (into [] (map :label) target-tokens)
            prefixes (asm.parser-recovery/compute-longest-correct-prefixes compiled-parser codec cyk token-vec)
            hiccup (parser-recovery/prefixes->hiccup-msg target-source-string target-tokens prefixes)]
        (dispatch [:log/error hiccup])))
    (when (contains? causes :no-start-symbol)
      (dispatch [:log/error "Parser failure: no start symbol has been defined"]))
    db))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Worker Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord BatchParserWorker [token-editor-id
                              cfg-editor-id
                              target-editor-id
                              compiled-parser
                              target-tokens
                              target-source-string
                              -status
                              stage
                              result]

  IWorker

  (status [this]
    (:-status this))

  (render [this db]
    (-render this db))

  (cache-key [this]
    (->cache-key (:token-editor-id this) (:cfg-editor-id this) (:target-editor-id this)))

  (dependencies [{:keys [token-editor-id cfg-editor-id target-editor-id] :as this} _]
    [(compile-parser/->cache-key token-editor-id cfg-editor-id)
     (workers.batch-lexer/->cache-key token-editor-id target-editor-id)])

  (current-progress [{:keys [stage] :as this} db]
    (if (#{:failure :success} (status this))
      (max-progress this db)
      0))

  (max-progress [this db]
    1)

  (progress-description [{:keys [stage result] :as this} db]
    (case (status this)
      :idle ""
      :running
      (case (:name stage)
        :init-cyk "Parsing"
        :run-cyk "Parsing"
        ;; default
        (str (:name stage)))
      :success "Finished parsing"
      :failure
      (if-let [{:keys [causes]} (:error result)]
        (cond
          (:empty-token-stream causes)
          [source-hyperlink {:source-id (first (q/editors->sources db [target-editor-id]))
                             :label "Sample file"
                             :suffix "is empty"}]
          (:empty-grammar causes)
          [source-hyperlink {:source-id (first (q/editors->sources db [cfg-editor-id]))
                             :label "Grammar definition"
                             :suffix "is empty"}]
          (:parse-failure causes)
          [source-hyperlink {:source-id (first (q/editors->sources db [target-editor-id]))
                             :label "Sample file"
                             :suffix "failed to parse"}]
          :else
          (str causes))
        "")
      ""))

  ISyncWorker

  (step [this db]
    (-step this db))

  (reset [this]
    (-reset this)))

(cljs.reader/register-tag-parser! "parsimony.workers.batch-parser.BatchParserWorker" map->BatchParserWorker)

(defn batch-parser-worker [token-editor-id cfg-editor-id target-editor-id]
  (map->BatchParserWorker
   {:token-editor-id token-editor-id
    :cfg-editor-id cfg-editor-id
    :target-editor-id target-editor-id
    :target-tokens nil
    :target-source-string nil
    :-status :idle
    :stage nil
    :result nil}))
