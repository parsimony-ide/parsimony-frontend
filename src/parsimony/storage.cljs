(ns parsimony.storage
  (:require [parsimony.transit :as transit :refer [cljs->transit transit->cljs]]
            [reagent.core :as reagent]
            [schema.core :as s :include-macros true]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn clear! []
  (.clear js/localStorage))

(defn persist! [key value]
  (.setItem js/localStorage
            (cljs->transit key)
            (cljs->transit value)))

(defn delete! [key]
  (.removeItem js/localStorage
               (cljs->transit key)))

(defn all []
  (let [keys (js->clj (js/Object.keys js/localStorage))]
    (when (seq keys)
      (into {}
            (map (fn [k]
                   [(transit->cljs k)
                    (transit->cljs (.getItem js/localStorage k))]))
            keys))))
