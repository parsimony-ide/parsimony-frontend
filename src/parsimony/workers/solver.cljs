(ns parsimony.workers.solver
  (:refer-clojure :exclude [-reset])
  (:require [parsimony.solver-impl :as solver-impl]
            [parsimony.models.parse-dashboard :as parse-dashboard]
            [parsimony.models.solver :as models.solver]
            [parsimony.measurement :as measurement]
            [parsimony.util :refer [matches-schema?]]
            [parsimony.worker :refer [IWorker ISyncWorker step reset status render cache-key current-progress max-progress progress-description]]
            [parsimony.workers.common :refer [source-hyperlink]]
            [parsimony.workers.compile-lexer :as workers.compile-lexer]
            [parsimony.workers.compile-parser :as workers.compile-parser]
            [re-frame.core :refer [dispatch]]
            [parsimony.console :as console]
            [schema.core :as s :include-macros true]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def schema
  {:-status (s/enum :success :failure :idle :running)
   :stage (s/maybe (s/pred :name))
   :error (s/maybe (s/pred :causes))
   :token-editor-id (s/maybe s/Num)
   :cfg-editor-id (s/maybe s/Num)
   :impl-state solver-impl/schema})

(defn verbose-matches-schema?
  [a-schema this]
  (matches-schema? a-schema this {:worker :solver
                                  :stage (get-in this [:stage :name])
                                  :status (status this)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ->cache-key [token-editor-id cfg-editor-id]
  {:algo :solver
   :token-editor-id token-editor-id
   :cfg-editor-id cfg-editor-id})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Timed Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private timed-gen-constraint-states
  (measurement/->timed-function
    :solver/gen-constraint-states
    solver-impl/gen-constraint-states))

(def ^:private timed-partition-constraint-states
  (measurement/->timed-function
    :solver/partition-constraint-states
    solver-impl/partition-constraint-states
    #(map :provenance (:unpartitioned-constraint-states %))))

(def ^:private timed-run-heuristics
  (measurement/->timed-function
    :solver/run-heuristics
    solver-impl/run-heuristics
    #(map :provenance (:partitioned-constraint-states %))))

(def ^:private timed-solve-partitions
  (measurement/->timed-function
    :solver/solve-partitions
    solver-impl/solve-partitions
    #(map :provenance (:unpartitioned-constraint-states %))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run Stage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti run-stage (fn [this _]
                      {:pre [(verbose-matches-schema? schema this)]}
                      (get-in this [:stage :name])))

(defmethod run-stage :default [this _]
  (console/error ::run-stage :unrecognized-stage this)
  (assoc this
         :-status :failure
         :stage nil))

(defmethod run-stage :prepare-1 [this db]
  (console/debug ::run-stage :prepare-1)
  (try
    (-> this
        (update :impl-state
                #(-> %
                     (solver-impl/recompile-parser false)
                     solver-impl/populate-sample-cache))
        (assoc :stage {:name :prepare-2}))
    (catch js/Error e
      (assoc this
             :-status :failure
             :error (ex-data e)
             :stage nil))))

(def ^:private -prepare-2-update-fn
  (measurement/->timed-function
    :solver/run-stage-prepare-2
    #(-> %
         timed-gen-constraint-states
         timed-partition-constraint-states
         timed-run-heuristics)))

(defmethod run-stage :prepare-2 [this db]
  (console/debug ::run-stage :prepare-2)
  (-> this
      (update :impl-state -prepare-2-update-fn)
      (assoc :stage {:name :heuristics-1})))

(defmethod run-stage :heuristics-1 [this db]
  (let [heuristics (get-in this [:impl-state :heuristics])]
    (console/debug ::run-stage :heuristics-1 {:heuristics heuristics})
    (if (seq heuristics)
      (assoc this
             :stage {:name :heuristics-2
                     :last-accepted (get-in this [:impl-state :accepted-heuristics])}
             :-status :success)
      (assoc this :stage {:name :solve-partitions}))))

(defmethod run-stage :heuristics-2 [this db]
  (console/debug ::run-stage :heuristics-2)
  (let [this (assoc this :impl-state (models.solver/get-impl (:solver db)))
        last-accepted (get-in this [:stage :last-accepted])
        accepted (get-in this [:impl-state :accepted-heuristics])]
    (console/debug ::run-stage :heuristics-2 {:last-accepted last-accepted
                                              :accepted accepted})
    (if (not= (count last-accepted) (count accepted))
      (assoc this :stage {:name :prepare-1})
      (assoc this :stage {:name :solve-partitions}))))

(def ^:private -solve-partitions-update-fn
  (measurement/->timed-function
    :solver/run-stage-solve-partitions
    #(-> %
         timed-solve-partitions
         solver-impl/recompile-multiparser
         solver-impl/recompute-forests
         timed-gen-constraint-states
         timed-solve-partitions)))

(defmethod run-stage :solve-partitions [this db]
  (console/debug ::run-stage :solve-partitions)
  (let [{:keys [partitioned-constraint-states accepted-heuristics]} (:impl-state this)]
    (if (and (empty? partitioned-constraint-states)
             (empty? accepted-heuristics))
      ;; no need to run, all labels satisfied
      (assoc this
             :stage nil
             :-status :success)
      (-> this
          (update :impl-state -solve-partitions-update-fn)
          (assoc :stage {:name :view-solutions} :-status :success)))))

(defmethod run-stage :view-solutions [this db]
  (console/debug ::run-stage :view-solutions)
  (assoc this
         :stage {:name :view-solutions}
         :-status :success))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti -step (fn [this _]
                  {:pre [(verbose-matches-schema? schema this)]}
                  (status this)))

(defmethod -step :default [this db]
  (do (console/error ::-step :unrecognized-status this)
      (assoc this
             :-status :failure
             :stage nil)))

;; -----------------------------------------------------------------------------
;; Idle
;; -----------------------------------------------------------------------------

(defmethod -step :idle [{:keys [token-editor-id cfg-editor-id] :as this} db]
  (let [cl-cache-key (workers.compile-lexer/->cache-key token-editor-id)
        cp-cache-key (workers.compile-parser/->cache-key token-editor-id cfg-editor-id)]
    (if-let [cl-worker (get-in db [:workers cl-cache-key])]
      (if-let [cp-worker (get-in db [:workers cp-cache-key])]
        (if-let [lexer (get-in cl-worker [:result :success])]
          (if-let [parser (get-in cp-worker [:result :success])]
            (do (console/debug ::-step :idle)
                (try
                  (-> this
                      (update :impl-state solver-impl/init lexer parser (:parse-dashboard db))
                      (update :impl-state
                              #(-> %
                                   (solver-impl/recompile-parser true)
                                   solver-impl/populate-sample-cache
                                   solver-impl/compute-working-set))
                      (assoc :-status :running :stage {:name :prepare-2}))
                  (catch js/Error e
                    (assoc this
                           :-status :failure
                           :error (ex-data e)
                           :stage nil))))
            (do (console/error ::-step :idle :no-parser-found)
                this))
          (do (console/error ::-step :idle :no-lexer-found)
              this))
        (do (console/error ::-step :idle :no-compile-parser-found {:expected-cache-key cp-cache-key})
            this))
      (do (console/error ::-step :idle :no-compile-lexer-found {:expected-cache-key cl-cache-key})
          this))))

;; -----------------------------------------------------------------------------
;; Running
;; -----------------------------------------------------------------------------

(defmethod -step :running [this db]
  (run-stage this db))

;; -----------------------------------------------------------------------------
;; Success
;; -----------------------------------------------------------------------------

(defmethod -step :success [this db]
  (if (some? (:stage this))
    ;; still in the middle of flow
    (-step (assoc this :-status :running) db)
    ;; finished flow
    (-step (assoc this :-status :idle) db)))

;; -----------------------------------------------------------------------------
;; Failure
;; -----------------------------------------------------------------------------

(defmethod -step :failure [this db]
  (-step (assoc this :-status :idle) db))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare solver-worker)

(defn- free-parse-resources
  [this]
  (update this :impl-state solver-impl/reset-sample-cache))

(defn- -reset [{:keys [token-editor-id cfg-editor-id] :as this}]
  (free-parse-resources this)
  (solver-worker token-editor-id cfg-editor-id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Render Stage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti render-stage (fn [this _]
                         (get-in this [:stage :name])))

(defmethod render-stage :default [this db]
  (console/error ::render-stage :unrecognized-stage this)
  db)

(defmethod render-stage :heuristics-2 [this db]
  (console/debug ::render-stage :heuristics-2)
  (-> db
      (update :solver models.solver/set-mode :view-heuristics)
      (update :solver models.solver/set-impl (:impl-state this))
      (update :solver models.solver/populate-ux-state)))

(defmethod render-stage :view-solutions [this db]
  (console/debug ::render-stage :view-solutions)
  (-> db
      (update :solver models.solver/set-mode :view-solutions)
      (update :solver models.solver/set-impl (:impl-state this))
      (update :solver models.solver/populate-ux-state)
      (update :solver models.solver/gen-previews)
      (models.solver/populate-overlay-state)))

(defmethod render-stage nil [this db]
  (console/debug ::render-stage nil)
  (dispatch [:parse-dashboard/delete-all-samples])
  (update db :solver models.solver/set-mode :nothing-to-solve))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Render
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti -render (fn [this _]
                    (status this)))

(defmethod -render :default [this db]
  (console/error ::-render :unrecognized-status this)
  db)

(defmethod -render :success [this db]
  (console/debug ::-render :success)
  (render-stage this db))

(defmethod -render :failure [this db]
  (console/debug ::-render :failure)
  db)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Progress
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -max-progress [this db]
  1)

(defn- -current-progress [{:keys [stage] :as this} db]
  (case (:name stage)
    :solve 1
    ;; default
    (-max-progress this db)))

(defn- -progress-description [{:keys [error] :as this} db]
  (case (status this)
    :idle ""
    :running "Solving"
    :success "Finished solving"
    :failure
    (if-let [{:keys [causes]} error]
      (condp #(contains? %2 %1) causes
        :lex-failure
        (let [{:keys [source-id]} (parse-dashboard/get-sample (:parse-dashboard db) (:sample-id error))]
          [source-hyperlink {:source-id source-id
                             :label "Sample file"
                             :suffix "fails to lex"
                             :on-click #(dispatch [:source-hyperlink-click-and-lex %])}])
        (str causes))
      (str "Unknown failure: " error))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Worker Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord SolverWorker [-status stage error token-editor-id cfg-editor-id impl-state]

  IWorker

  (status [this]
    (:-status this))

  (render [this db]
    (-render this db))

  (cache-key [{:keys [token-editor-id cfg-editor-id] :as this}]
    (->cache-key token-editor-id cfg-editor-id))

  (dependencies [{:keys [token-editor-id cfg-editor-id] :as this} _]
    [(workers.compile-lexer/->cache-key token-editor-id)
     (workers.compile-parser/->cache-key token-editor-id cfg-editor-id)])

  (current-progress [{:keys [stage] :as this} db]
    (-current-progress this db))

  (max-progress [this db]
    (-max-progress this db))

  (progress-description [this db]
    (-progress-description this db))

  ISyncWorker

  (step [this db]
    (-step this db))

  (reset [this]
    (-reset this)))

(cljs.reader/register-tag-parser! "parsimony.workers.solver.SolverWorker" map->SolverWorker)

(defn solver-worker [token-editor-id cfg-editor-id]
  (map->SolverWorker
   {:token-editor-id token-editor-id
    :cfg-editor-id cfg-editor-id
    :-status :idle
    :stage nil
    :error nil
    :impl-state solver-impl/default-model}))
