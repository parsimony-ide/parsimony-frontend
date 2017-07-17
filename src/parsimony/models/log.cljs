(ns parsimony.models.log
  (:require [schema.core :as s :include-macros true]
            [parsimony.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def entry-schema {:type (s/enum :info :warn :error :success)
                   :timestamp s/Num
                   :value s/Any})

(def schema {:entries [entry-schema]})

(def default-model
  {:entries #queue []})

(def MAX_ENTRIES 20)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn reset [_]
  (console/debug ::reset)
  default-model)

(defn- discard-old-entries [{:keys [entries] :as state}]
  (if (> (count entries) MAX_ENTRIES)
    (recur (update state :entries pop))
    state))

(defn- make-entry [type value]
  {:type type
   :timestamp (.now js/Date)
   :value value})

(defn- add-entry [state entry]
  (-> state
      (update :entries conj entry)
      (discard-old-entries)))

(defn info [state value]
  (add-entry state (make-entry :info value)))

(defn warn [state value]
  (add-entry state (make-entry :warn value)))

(defn error [state value]
  (add-entry state (make-entry :error value)))

(defn success [state value]
  (add-entry state (make-entry :success value)))
