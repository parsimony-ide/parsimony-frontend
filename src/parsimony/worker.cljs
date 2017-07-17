(ns parsimony.worker
  (:require [medley.core :refer [dissoc-in]]
            [parsimony.models.colors :as colors]
            [parsimony.models.overlay :as models.overlay]
            [parsimony.views.info :refer [IDetail]]
            [re-com.core :refer [v-box label]]
            [schema.core :as s :include-macros true]))

(defprotocol IWorker
  (status [this] "Return one of :idle :running :success :failure")
  (render [this db] "Update app-state to reflect UI updates for this worker")
  (cache-key [this] "A unique key for this worker")
  (dependencies [this db] "A collection of cache-keys for workers on which this worker is dependent")
  (current-progress [this db] "How much progress has been made")
  (max-progress [this db] "How much progress must be made to complete this unit of work, or nil if unknown")
  (progress-description [this db] "A string or hiccup describing the current progress, or nil if none"))

(defprotocol ISyncWorker
  "Synchronous worker"
  (step [this db] "Execute one tick of this worker.")
  (reset [this] "Return to idle state"))

(defprotocol IAsyncWorker
  "Asynchronous worker"
  (start [this db succ-cb fail-cb] "Start this asynchronous worker.  When finished, calls cb with one argument: this")
  (complete-success [this payload] "Called when this asynchronous worker has completed successfully")
  (complete-failure [this payload] "Called when this asynchronous worker has completed unsuccessfully"))
