(ns parsimony.workers.compile-parser
  "A worker that compiles a grammar definition"
  (:require [parsimony.parser :as parser]
            [parsimony.util :refer [matches-schema? pprint-str newline-pprint-str]]
            [parsimony.views.info :refer [IDetail]]
            [parsimony.models.colors :as colors]
            [parsimony.models.editor :refer [-string ->char-range inject-error-overlays remove-overlays]]
            [parsimony.query :as q]
            [parsimony.worker :refer [IWorker ISyncWorker step reset status render cache-key current-progress max-progress progress-description]]
            [parsimony.workers.common :refer [malformed-syntax->overlays source-hyperlink] :as workers.common]
            [parsimony.workers.compile-lexer :as compile-lexer]
            [re-com.core :refer [v-box label]]
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
          :cfg-source-string (s/maybe s/Str)
          (s/optional-key :compiled-lexer) s/Any
          :stage (s/maybe (s/pred :name))
          :result (s/maybe {(s/optional-key :success) s/Any
                            (s/optional-key :error) s/Any})}))

(defn verbose-matches-schema?
  [a-schema this]
  (matches-schema? a-schema this {:worker :compile-parser
                                  :status (status this)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ->cache-key [token-editor-id cfg-editor-id]
  {:algo :compile-parser
   :token-editor-id token-editor-id
   :cfg-editor-id cfg-editor-id})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run Stage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti run-stage (fn [this _]
                      {:pre [(verbose-matches-schema? schema this)]}
                      (let [s (get-in this [:stage :name])]
                        (console/debug ::run-stage s)
                        s)))

(defmethod run-stage :default [this _]
  (console/error ::run-stage :unrecognized-stage this)
  (assoc this
         :-status :failure
         :stage nil))

(defmethod run-stage :check-stale [{:keys [cfg-editor-id] :as this} db]
  (assoc this
         :cfg-source-string (-string db cfg-editor-id)
         :-status :running
         :stage {:name :recompile})

  ;; XXX: the below is commented out since parser compilation is cheap. There
  ;; doesn't seem to be a need for the added complication of checking for stale
  ;; inputs. Revisit if performance becomes a problem.
  #_(let [cfg-source-string (-string db cfg-editor-id)]
      (if-not (= cfg-source-string (:cfg-source-string this))
        ;; cfg-source-string has changed, so initiate recompile
        (assoc this
               :cfg-source-string cfg-source-string
               :-status :running
               :stage {:name :recompile})
        ;; cfg-source-string has not changed, so do nothing
        (assoc this
               :-status :success
               :stage nil))))

(defmethod run-stage :recompile [{:keys [token-editor-id cfg-editor-id cfg-source-string compiled-lexer] :as this} db]
  (let [result
        (try
          {:success (parser/definition-parser cfg-source-string (map first compiled-lexer)) }
          (catch js/Error e
            {:error (ex-data e)}))]
    (assoc this
           :result result
           :-status (if (:error result) :failure :success)
           :stage nil)))

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

(defmethod -step :idle [{:keys [token-editor-id cfg-editor-id] :as this} db]
  (let [cl-cache-key (compile-lexer/->cache-key token-editor-id)]
    (if-let [cl-worker (get-in db [:workers cl-cache-key])]
      ;;  XXX: should check for cl-worker success here
      (assoc this
             :compiled-lexer (get-in cl-worker [:result :success])
             :-status :running
             :stage {:name :check-stale})
      (do (console/error ::-step :idle (str "No compile-lexer worker with cache key " cl-cache-key " found"))
          (assoc this
                 :-status :failure
                 :stage nil)))))

(defmethod -step :running [this db]
  (run-stage this db))

(defmethod -step :success [this db]
  (step (assoc this :-status :idle) db))

(defmethod -step :failure [this db]
  (step (assoc this
               :cfg-source-string nil ;; prevent :check-stale stage from skipping recompilation
               :-status :idle)
        db))

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

(defmethod -render :success [{:keys [token-editor-id cfg-editor-id result] :as this} db]
  (if-let [success (:success result)]
    ;; attach all decoration affinities for symbols defined in this grammar
    (let [nts (into #{}
                    (map parser/lhs)
                    (:productions success))]
      (console/debug ::-render :final (newline-pprint-str success))
      (update db :decoration-affinities colors/allocate-affinities (map name nts)))
    db))

(declare error->overlays)

(defmethod -render :failure [{:keys [cfg-editor-id] :as this} db]
  (inject-error-overlays db cfg-editor-id (error->overlays (get-in this [:result :error]))))

(defrecord UndefinedSymbolInfo [error]
  IDetail
  (detail-view [this]
    [v-box
     :children
     [[label :label "The following symbols are used but never defined:"]
      (for [[i [k _]] (map-indexed vector (:instances error))]
        ^{:key i}
        [label :label (name k)])]]))

(defn undefined-symbol->overlays
  [{:keys [causes instances] :as error}]
  {(name (first causes))
   (vec (for [[_ {[from to] :span}] instances]
          (->char-range from to (map->UndefinedSymbolInfo {:error error}))))})

(defrecord RedundantProductionInfo [error]
  IDetail
  (detail-view [this]
    [v-box
     :children
     [[label :label "The highlighted productions are identical."]]]))

(defn redundant-production->overlays
  [{:keys [causes declarations] :as error}]
  {(name (first causes))
   (vec (for [[_ {[from to] :span}] declarations]
          (->char-range from to (map->RedundantProductionInfo {:error error}))))})

(defrecord ProductionCycleInfo [error]
  IDetail
  (detail-view [this]
    (let [plural? (> (count (:declarations error)) 1)]
      [v-box
       :children
       [[label :label (if plural?
                        "The highlighted productions form a cycle."
                        "The highlighted production is cyclic.")]]])))

(defn- production-cycle->overlays
  [{:keys [causes declarations] :as error}]
  {(name (first causes))
   (vec (for [[_ {[from to] :span}] declarations]
          (->char-range from to (map->ProductionCycleInfo {:error error}))))})

(defrecord IncompatibleAssociativityInfo [error]
  IDetail
  (detail-view [this]
    [v-box
     :children
     [[label :label (str "You have declared " (parser/emit-one (:production error)) " to be")]
      [label :label "both left and right associative."]]]))

(defn- incompatible-associativity->overlays
  [{:keys [causes declarations] :as error}]
  {(name (first causes))
   (vec (for [[_ {[from to] :span}] declarations]
          (->char-range from to (map->IncompatibleAssociativityInfo {:error error}))))})

(defrecord PriorityCycleInfo [error]
  IDetail
  (detail-view [this]
    [v-box
     :children
     [[label :label (str "You have declared a cyclic priority,")]
      [label :label (str "causing " (parser/emit-one (:production error)) " to have higher priority than itself.")]]]))

(defn- priority-cycle->overlays
  [{:keys [causes declarations] :as error}]
  {(name (first causes))
   (vec (for [[_ {[from to] :span}] declarations]
          (->char-range from to (map->PriorityCycleInfo {:error error}))))})

(defrecord UndefinedAssociativityProductionsInfo [error]
  IDetail
  (detail-view [this]
    (let [plural? (> (count (:declarations error)) 1)]
      [v-box
       :children
       [[label :label (str "You have declared associativity on "
                           (if plural? "productions" "a production"))]
        [label :label (str "that "
                           (if plural? "have" "has")
                           " not been previously defined")]]])))

(defn- undefined-associativity-productions->overlays
  [{:keys [causes declarations] :as error}]
  {(name (first causes))
   (vec (for [[_ {[from to] :span}] declarations]
          (->char-range from to (map->UndefinedAssociativityProductionsInfo {:error error}))))})

(defrecord UndefinedPriorityProductionsInfo [error]
  IDetail
  (detail-view [this]
    (let [plural? (> (count (:declarations error)) 1)]
      [v-box
       :children
       [[label :label (str "You have declared priority on "
                           (if plural? "productions" "a production"))]
        [label :label (str "that "
                           (if plural? "have" "has")
                           " not been previously defined")]]])))

(defn- undefined-priority-productions->overlays
  [{:keys [causes declarations] :as error}]
  {(name (first causes))
   (vec (for [[_ {[from to] :span}] declarations]
          (->char-range from to (map->UndefinedPriorityProductionsInfo {:error error}))))})

(defrecord UndefinedStartSymbolInfo [error]
  IDetail
  (detail-view [this]
    [label :label (str "The start symbol " (name (:symbol error)) " is not defined")]))

(defn- undefined-start-symbol->overlays
  [{:keys [causes declarations] :as error}]
  {(name (first causes))
   (vec (for [[_ {[from to] :span}] declarations]
          (->char-range from to (map->UndefinedStartSymbolInfo {:error error})))) })

(defn error->overlays
  [{:keys [causes] :as error}]
  (console/error ::error error)
  (cond
    (:parse-failure causes)
    (malformed-syntax->overlays error)
    (:undefined-symbol causes)
    (undefined-symbol->overlays error)
    (:redundant-production causes)
    (redundant-production->overlays error)
    (:incompatible-associativity causes)
    (incompatible-associativity->overlays error)
    (:priority-cycle causes)
    (priority-cycle->overlays error)
    (:production-cycle causes)
    (production-cycle->overlays error)
    (:undefined-associativity-productions causes)
    (undefined-associativity-productions->overlays error)
    (:undefined-priority-productions causes)
    (undefined-priority-productions->overlays error)
    (:undefined-start-symbol causes)
    (undefined-start-symbol->overlays error)
    :else
    nil))

(defn- -clear-previous-render [{:keys [cfg-editor-id] :as this} db]
  #_(console/debug ::-clear-previous-render)
  (remove-overlays db cfg-editor-id :error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Worker Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare compile-parser-worker)

(defrecord CompileParserWorker [token-editor-id
                                cfg-editor-id
                                cfg-source-string
                                compiled-lexer
                                -status
                                stage
                                result]
  IWorker

  (status [this]
    (:-status this))

  (render [this db]
    (-render this (-clear-previous-render this db)))

  (cache-key [this]
    (->cache-key (:token-editor-id this) (:cfg-editor-id this)))

  (dependencies [{:keys [token-editor-id] :as this} _]
    [(compile-lexer/->cache-key token-editor-id)])

  (current-progress [this _]
    (if (#{:failure :success} (status this))
      1
      0))

  (max-progress [_ _]
    1)

  (progress-description [this db]
    (letfn [(error-msg [suffix]
              [source-hyperlink {:source-id (first (q/editors->sources db [cfg-editor-id]))
                                 :label "Grammar definition"
                                 :suffix suffix}])]
      (case (status this)
        :idle ""
        :running "Compiling parser"
        :success "Finished compiling parser"
        :failure
        (when-let [{:keys [causes]} (:error result)]
          (condp #(contains? %2 %1) causes
            :parse-failure (error-msg "has syntax errors")
            :undefined-symbol (error-msg "has undefined symbols")
            :redundant-production (error-msg "has redundant productions")
            :incompatible-associativity (error-msg "has invalid associativity declarations")
            :priority-cycle (error-msg "has cyclic priorities")
            :production-cycle (error-msg "has cyclic productions")
            :undefined-associativity-productions (error-msg "has invalid associativity declarations")
            :undefined-priority-productions (error-msg "has invalid priority declarations")
            :undefined-start-symbol (error-msg "has invalid start symbol")
            :empty-grammar (error-msg "is empty")
            (error-msg (str "has the following errors: " causes)))))))

  ISyncWorker

  (step [this db]
    (-step this db))

  (reset [this]
    (compile-parser-worker token-editor-id cfg-editor-id)))

(cljs.reader/register-tag-parser! "parsimony.workers.compile-parser.CompileParserWorker" map->CompileParserWorker)

(defn compile-parser-worker [token-editor-id cfg-editor-id]
  (map->CompileParserWorker
   {:token-editor-id token-editor-id
    :cfg-editor-id cfg-editor-id
    :cfg-source-string nil
    :-status :idle
    :stage nil
    :result nil}))
