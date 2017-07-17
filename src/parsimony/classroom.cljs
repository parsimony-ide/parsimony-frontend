(ns parsimony.classroom
  (:require [parsimony.classroom.api :refer [prerequisite-status]]
            [parsimony.classroom.ui :refer [task-incomplete-modal cannot-finish-modal]]
            [parsimony.models.source :as source]
            [parsimony.query :as q]
            [re-frame.core :refer [->interceptor dispatch]]
            [re-frame.interceptor :refer [assoc-effect]]
            [schema.core :as s :include-macros true]
            [parsimony.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def schema
  {s/Num s/Bool})

(def default-model
  {})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interceptor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti -handle-before
  (fn [context]
    (first (get-in context [:coeffects :event]))))

(defmethod -handle-before :default [context]
  context)

(defmethod -handle-before :file-picker-click
  [{:keys [coeffects] :as context}]
  (console/debug ::-handle-before :file-picker-click)
  (let [[event-kw & params] (:event coeffects)
        db (:db coeffects)
        source-id (first params)
        {:keys [source-path]} (get-in db [:sources source-id])
        status (prerequisite-status db source-id)]
    (if-not (:pass? status)
      ;; some tests failing
      (do (console/warn ::open-file-interceptor
                        :prerequisites-not-met
                        {:status status})
          (-> context
              (update :queue empty)
              (assoc-effect :db (assoc db :modal (task-incomplete-modal source-path (:source-path status))))))
      ;; all tests passing
      (do (console/debug ::open-file-interceptor
                         :prerequisites-met
                         {:status status})
          context))))

(defmethod -handle-before :open-project
  [context]
  (console/debug ::-handle-before :open-project)
  (dispatch [:classroom/pause])
  context)

(def open-file-interceptor
  (->interceptor
    :id :open-file
    :before -handle-before))

