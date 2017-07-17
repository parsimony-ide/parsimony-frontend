(ns parsimony.async-workers.common
  (:require [parsimony.worker :refer [cache-key]]
            [parsimony.comm :as comm]
            [parsimony.util :refer [pprint-str]]
            [re-frame.core :refer [dispatch]]
            [parsimony.console :as console]))

(defn do-comm [worker message-id payload]
  (letfn [(callback [response]
            #_(console/debug :do-comm-response (pprint-str response))
            (cond
              (:success response)
              (dispatch [:async-worker-complete-success (cache-key worker) (:success response)])
              (:failure response)
              (dispatch [:async-worker-complete-failure (cache-key worker) (:failure response)])
              :else ;; default failure
              (dispatch [:async-worker-complete-failure (cache-key worker) response])))]
    #_(console/debug :do-comm message-id (str payload))
    (comm/request message-id payload callback callback)))

(defn force-async-success [worker payload]
  (dispatch [:async-worker-complete-success (cache-key worker) payload]))
