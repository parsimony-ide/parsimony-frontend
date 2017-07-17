(ns parsimony.comm
  "Network communication"
  (:require [parsimony.config :as config]
            [re-frame.core :refer [dispatch]]
            [taoensso.sente :as sente :refer [cb-success?]]
            [schema.core :as s :include-macros true]
            [parsimony.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def schema {:open? s/Bool
             :in-flight (s/maybe #{[(s/one s/Num "registry-id")
                                    (s/one s/Keyword "request-id")]})
             :registry (s/maybe {s/Num {:registry-id s/Num
                                        :request-id s/Keyword
                                        :payload s/Any
                                        :start-timestamp s/Num
                                        (s/optional-key :end-timestamp) s/Num
                                        :status (s/enum :in-flight :success :failure)}})
             :num-failures s/Num
             :needs-attention? s/Bool})

(def default-model {:open? false
                    :in-flight nil
                    :registry nil
                    :num-failures 0
                    :needs-attention? false})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Channel Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Sente initialization code, taken directly from https://github.com/ptaoussanis/sente README
(let [{:keys [chsk ch-recv send-fn state]}
      (sente/make-channel-socket! "/chsk" ; Note the same path as before
                                  {:type :auto ; e/o #{:auto :ajax :ws}
                                   :host (config/host)})]
  (def chsk       chsk)
  (def ch-chsk    ch-recv) ; ChannelSocket's receive channel
  (def chsk-send! send-fn) ; ChannelSocket's send API fn
  (def chsk-state state)   ; Watchable, read-only atom
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Client->Server Push
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce registry-number (atom 0))

(defn- limit-assoc [max-elems m k v]
  (if (< (count m) max-elems)
    (assoc m k v)
    (let [ks (->> (keys m)
                  (sort)
                  (reverse)
                  (take (dec max-elems)))]
      (assoc (select-keys m ks) k v))))

(defn- registry-assoc [m k v]
  (if (contains? m k)
    (assoc m k v)
    (limit-assoc 25 m k v)))

(defn register-in-flight [state registry-id request-id payload]
  (-> state
      (update :in-flight (fnil conj #{}) [registry-id request-id])
      (update :registry
              (fnil registry-assoc {})
              registry-id
              {:registry-id registry-id
               :request-id request-id
               :payload payload
               :start-timestamp (.now js/Date)
               :status :in-flight})))

(defn complete-in-flight [state registry-id request-id edn-reply status-kw]
  (let [state (-> state
                  (update :in-flight disj [registry-id request-id])
                  (update :num-failures
                          (fn [n]
                            (if (= :failure status-kw)
                              (inc n)
                              n)))
                  (update :needs-attention?
                          (fn [b]
                            (if (= :failure status-kw)
                              true
                              b))))]
    (if-let [registry-entry (get-in state [:registry registry-id])]
      (-> state
          (update :registry
                  (fnil registry-assoc {})
                  registry-id
                  (assoc registry-entry
                         :end-timestamp (.now js/Date)
                         :status status-kw)))
      (do (console/warn ::complete-in-flight
                        :no-registry-entry
                        {:registry-id registry-id
                         :request-id request-id})
          state))))

(defn request [request-id data succ-cb fail-cb]
  (let [registry-id (swap! registry-number inc)]
    (dispatch [:comm/register-in-flight registry-id request-id data])
    (chsk-send! [request-id data]
                30000
                (fn [edn-reply]
                  (if (sente/cb-success? edn-reply)
                    (do (dispatch [:comm/complete-in-flight registry-id request-id edn-reply :success])
                        (succ-cb edn-reply))
                    (do (dispatch [:comm/complete-in-flight registry-id request-id edn-reply :failure])
                        (fail-cb edn-reply)))))))

(defn ninja-request
  "Perform request without in-flight tracking machinery"
  [request-id data succ-cb fail-cb]
  (chsk-send! [request-id data]
              30000
              (fn [edn-reply]
                (if (sente/cb-success? edn-reply)
                  (succ-cb edn-reply)
                  (fail-cb edn-reply)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Client Side Handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti -event-msg-handler :id)

(defn event-msg-handler [ev-msg]
  (-event-msg-handler ev-msg))

(defmethod -event-msg-handler :default
  [ev-msg]
  (console/warn ::-event-msg-handler :default {:event (:event ev-msg)}))

(defmethod -event-msg-handler :chsk/state
  [{:keys [?data] :as ev-msg}]
  (console/debug ::-event-msg-handler :chsk/state {:data ?data})
  (dispatch [:comm/set-open-flag (boolean (:open? ?data))]))

(defmethod -event-msg-handler :chsk/recv
  [{:keys [?data] :as ev-msg}]
  (console/debug ::-event-msg-handler :chsk/recv {:data ?data}))

(defmethod -event-msg-handler :chsk/handshake
  [{:keys [?data] :as ev-msg}]
  (console/debug ::-event-msg-handler :chsk/handshake {:data ?data}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Client Side Router
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce router (atom nil))

(defn stop-router! []
  (console/debug ::stop-router!)
  (when-let [stop-f @router] (stop-f)))

(defn start-router! []
  (stop-router!)
  (console/debug ::start-router!)
  (reset! router
    (sente/start-chsk-router!
      ch-chsk event-msg-handler)))
