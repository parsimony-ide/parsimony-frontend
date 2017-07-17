(ns parsimony.workers.auto-parse
  "A worker that re-runs the parse worker for any editor whose :auto-parse flag is true"
  (:require [clojure.set :as set]
            [parsimony.models.editor :as editor]
            [parsimony.util :refer [matches-schema? pprint-str]]
            [parsimony.worker :refer [IWorker ISyncWorker step reset status render cache-key current-progress max-progress progress-description]]
            [parsimony.workers.common :as workers.common]
            [parsimony.workers.compile-parser :as workers.compile-parser]
            [parsimony.workers.lexer :as workers.lexer]
            [parsimony.workers.parser :as workers.parser]
            [re-frame.core :refer [dispatch]]
            [schema.core :as s :include-macros true]
            [parsimony.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def schema
  (merge workers.common/schema
         {:token-editor-id s/Num
          :cfg-editor-id s/Num}))

(defn verbose-matches-schema?
  [a-schema this]
  (matches-schema? a-schema this {:worker :auto-parse
                                  :status (status this)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ->cache-key [token-editor-id cfg-editor-id]
  {:algo :auto-parse
   :token-editor-id token-editor-id
   :cfg-editor-id cfg-editor-id})

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

(defmethod -step :idle [this db]
  (assoc this :-status :running))

(defmethod -step :running [this db]
  (assoc this :-status :success))

(defmethod -step :success [this db]
  (step (assoc this :-status :idle) db))

(defmethod -step :failure [this db]
  (step (assoc this :-status :idle) db))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Worker Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord AutoParseWorker [token-editor-id cfg-editor-id -status]

  IWorker
  (status [this]
    (:-status this))

  (render [_ db]
    (console/debug ::render)
    db)

  (cache-key [this]
    (->cache-key (:token-editor-id this) (:cfg-editor-id this)))

  (dependencies [{:keys [token-editor-id cfg-editor-id] :as this} db]
    (into [(workers.compile-parser/->cache-key token-editor-id cfg-editor-id)]
          (for [target-editor-id (editor/editors-with-auto-parse db)]
            (workers.parser/->cache-key token-editor-id cfg-editor-id target-editor-id))))

  (current-progress [_ _]
    nil)

  (max-progress [_ _]
    nil)

  (progress-description [_ _]
    nil)

  ISyncWorker

  (step [this db]
    (-step this db))

  (reset [this]
    this))

(cljs.reader/register-tag-parser! "parsimony.workers.auto-parse.AutoParseWorker" map->AutoParseWorker)

(defn auto-parse-worker [token-editor-id cfg-editor-id]
  (map->AutoParseWorker
    {:token-editor-id token-editor-id
     :cfg-editor-id cfg-editor-id
     :-status :idle}))
