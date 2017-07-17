(ns parsimony.workers.common
  "Common utilities needed to write workers"
  (:require [parsimony.models.editor :refer [->char-range]]
            [parsimony.views.info :refer [IDetail]]
            [re-com.core :refer [h-box v-box label hyperlink]]
            [re-frame.core :refer [dispatch]]
            [schema.core :as s :include-macros true]
            [parsimony.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def schema
  {:-status (s/enum :success :failure :idle :running)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Purpose API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-worker-keys-by-algo
  "Given the :workers db state, return seq of all worker cache-keys with the given algo"
  [workers algo]
  (filter (fn [cache-key] (= algo (:algo cache-key))) (keys workers)))

(defn get-workers-by-algo
  "Given the :workers db state, return seq of all workers with the given algo"
  [workers algo]
  (let [cache-keys (get-worker-keys-by-algo workers algo)]
    (vals (select-keys workers cache-keys))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Malformed Syntax Rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- reason-view [r]
  (case (:tag r)
    :regexp [:div [:code (str \# \" (:expecting r) \")]]
    :string [:div [:code (str (:expecting r))]]))

(defn- reasons-view [rs]
  [v-box
   :children [[label :label "Expected one of:"]
              (for [[i r] (map-indexed vector rs)]
                ^{:key i}
                [reason-view r])]])

(defrecord MalformedSyntaxInfo [reason]
  IDetail
  (detail-view [this]
    [v-box
     :children [[reasons-view (:reason this)]]]))

(defn malformed-syntax-info [reason]
  (map->MalformedSyntaxInfo {:reason reason}))

(defn malformed-syntax->overlays
  [{absolute-index :index failure :failure :as error}]
  (let [{:keys [index line column reason]} failure
        index (+ absolute-index index)]
    (console/debug ::malformed-syntax->overlays
                   :index index
                   :failure failure)
    (when-let [cause (first (:causes error))]
      {(name cause) [(->char-range index
                                   (inc index)
                                   (malformed-syntax-info reason))]})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Progress Description
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn source-hyperlink
  [{:keys [source-id tooltip tooltip-position suffix on-click] -label :label
    :or {tooltip "Go to file"
         tooltip-position :above-center}
    :as args}]
  [h-box
   :gap "6px"
   :children
   [[hyperlink
     :label -label
     :tooltip tooltip
     :tooltip-position tooltip-position
     :on-click
     (fn []
       (if on-click
         (on-click source-id)
         (dispatch [:source-hyperlink-click source-id])))]
    (when suffix
      [label :label suffix])]])
