(ns parsimony.workers.reformat
  (:refer-clojure :exclude [-reset])
  (:require [parsimony.models.editor :refer [-string]]
            [parsimony.parser :as parser]
            [parsimony.query :as q]
            [parsimony.refactor.parser :as refactor.parser]
            [parsimony.util
             :refer [pprint-str]
             :refer-macros [inspect-pp with-inspection]]
            [parsimony.worker :refer [IWorker ISyncWorker step reset status render cache-key current-progress max-progress progress-description]]
            [parsimony.workers.compile-lexer :as compile-lexer]
            [parsimony.workers.compile-parser :as compile-parser]
            [parsimony.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ->cache-key [token-editor-id cfg-editor-id]
  {:algo :reformat
   :token-editor-id token-editor-id
   :cfg-editor-id cfg-editor-id})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti -step (fn [this _] (status this)))

(defmethod -step :default [this db]
  (do (console/error :-step :unrecognized-status this)
      (assoc this
             :-status :failure
             :stage nil)))

;; -----------------------------------------------------------------------------
;; Idle
;; -----------------------------------------------------------------------------

(defmethod -step :idle [{:keys [token-editor-id cfg-editor-id] :as this} db]
  (console/debug :reformat :idle)
  (let [cl-cache-key (compile-lexer/->cache-key token-editor-id)]
    (if-let [cl-worker (get-in db [:workers cl-cache-key])]
      (let [cp-cache-key (compile-parser/->cache-key token-editor-id cfg-editor-id)]
        (if-let [cp-worker (get-in db [:workers cp-cache-key])]
          (if (= :success (status cp-worker))
            ;; parser compiled, continue
            (let [compiled-lexer (get-in cl-worker [:result :success])]
              (assoc this
                     :compiled-lexer compiled-lexer
                     :-status :running))
            ;; parser did not compile, fail
            (assoc this :-status :failure))
          (do (console/error :reformat :no-compile-parser-worker-found cp-cache-key)
              (assoc this :-status :failure))))
      (do (console/error :reformat :no-compile-lexer-worker-found cl-cache-key)
          (assoc this :-status :failure)))))

;; -----------------------------------------------------------------------------
;; Running
;; -----------------------------------------------------------------------------

(defmethod -step :running [{:keys [cfg-editor-id compiled-lexer] :as this} db]
  (console/debug :reformat :running)
  (let [ast (parser/definition-parser
              (-string db cfg-editor-id)
              (map first compiled-lexer)
              :skip-checks true)]
    (assoc this
           :result (refactor.parser/reformat ast :sort-productions true)
           :-status :success)))

;; -----------------------------------------------------------------------------
;; Success
;; -----------------------------------------------------------------------------

(defmethod -step :success [this db]
  (-step (assoc this :-status :idle) db))

;; -----------------------------------------------------------------------------
;; Failure
;; -----------------------------------------------------------------------------

(defmethod -step :failure [this db]
  (-step (assoc this :-status :idle) db))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare reformat-worker)

(defn- -reset [{:keys [token-editor-id cfg-editor-id sample-cache] :as this}]
  (console/debug :reformat :reset)
  (reformat-worker token-editor-id cfg-editor-id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti -render (fn [this _]
                    (status this)))

(defmethod -render :default [this db]
  (console/error :reformat :-render :unrecognized-status this)
  db)

(defmethod -render :success [{:keys [cfg-editor-id] :as this} db]
  (console/debug :reformat :-render :success)
  (let [buffer-id (first (q/editors->buffers db [cfg-editor-id]))]
    (assoc-in db [:buffers buffer-id :string] (:result this))))

(defmethod -render :failure [this db]
  (console/debug :reformat :-render :failure)
  db)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Worker Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare reformat-worker)

(defrecord ReformatWorker [-status compiled-lexer token-editor-id cfg-editor-id result]

  IWorker

  (status [this]
    (:-status this))

  (render [this db]
    (-render this db))

  (cache-key [{:keys [token-editor-id cfg-editor-id] :as this}]
    (->cache-key token-editor-id cfg-editor-id))

  (dependencies [{:keys [token-editor-id cfg-editor-id] :as this} _]
    [(compile-lexer/->cache-key token-editor-id)
     (compile-parser/->cache-key token-editor-id cfg-editor-id)])

  (current-progress [this db]
    1)

  (max-progress [this db]
    1)

  (progress-description [this db]
    "Finished formatting")

  ISyncWorker

  (step [this db]
    (-step this db))

  (reset [this]
    (reformat-worker token-editor-id cfg-editor-id)))

(defn reformat-worker [token-editor-id cfg-editor-id]
  (map->ReformatWorker
    {:token-editor-id token-editor-id
     :cfg-editor-id cfg-editor-id
     :-status :idle
     :result nil}))
