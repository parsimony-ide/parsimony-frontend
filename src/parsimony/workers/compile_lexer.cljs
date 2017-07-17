(ns parsimony.workers.compile-lexer
  "A worker that compiles a lexer definition"
  (:require [parsimony.async-workers.common :refer [do-comm force-async-success]]
            [parsimony.comm :as comm]
            [parsimony.models.colors :as colors]
            [parsimony.query :as q]
            [parsimony.util :refer [matches-schema? pprint-str]]
            [parsimony.worker :refer [IWorker IAsyncWorker start status render cache-key complete-success]]
            [parsimony.models.editor :refer [-string inject-error-overlays remove-overlay remove-overlays]]
            [parsimony.workers.common :refer [malformed-syntax->overlays source-hyperlink] :as workers.common]
            [schema.core :as s :include-macros true]
            [parsimony.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def schema
  (merge workers.common/schema
         {:token-editor-id s/Num
          :token-source-string (s/maybe s/Str)
          :listeners {:succ-cb [s/Any]
                      :fail-cb [s/Any]}
          :result {(s/optional-key :success) s/Any
                   (s/optional-key :error) s/Any}}))

(defn verbose-matches-schema?
  [a-schema this]
  (matches-schema? a-schema this {:worker :compile-lexer
                                  :status (status this)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ->cache-key [token-editor-id]
  {:algo :compile-lexer
   :token-editor-id token-editor-id})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti -start (fn [this _ _ _]
                  {:pre [(verbose-matches-schema? schema this)]}
                  (let [s (status this)]
                    (console/debug ::-start s)
                    s)))

(defmethod -start :default [this _ _ _]
  (do (console/error ::-start :unrecognized-status this)
      (assoc this :-status :failure)))

;; -----------------------------------------------------------------------------
;; Idle
;; -----------------------------------------------------------------------------

(defmethod -start :idle [this db succ-cb fail-cb]
  (let [token-source-string (-string db (:token-editor-id this))]
    (do-comm this :compiler/compile-lexer token-source-string)
    (-> this
        (update-in [:listeners :succ-cb] conj succ-cb)
        (update-in [:listeners :fail-cb] conj fail-cb)
        (assoc :token-source-string token-source-string)
        (assoc :-status :running))))

;; -----------------------------------------------------------------------------
;; Running
;; -----------------------------------------------------------------------------

(defmethod -start :running [this _ succ-cb fail-cb]
  (-> this
      (update-in [:listeners :succ-cb] conj succ-cb)
      (update-in [:listeners :fail-cb] conj fail-cb)))

;; -----------------------------------------------------------------------------
;; Success
;; -----------------------------------------------------------------------------

(defmethod -start :success [this db succ-cb fail-cb]
  (let [token-source-string (-string db (:token-editor-id this))]
    (if-not (= token-source-string (:token-source-string this))
      ;; token-source-string has changed, so initiate recompile
      (do (console/debug ::recompile true)
          (start (assoc this :-status :idle) db succ-cb fail-cb))
      ;; token-source-string has not changed, so do nothing and loop back to success state
      (do (console/debug ::recompile false)
          (let [this (-> this
                         (update-in [:listeners :succ-cb] conj succ-cb)
                         (update-in [:listeners :fail-cb] conj fail-cb))]
            (force-async-success this (get-in this [:result :success]))
            this)))))

;; -----------------------------------------------------------------------------
;; Failure
;; -----------------------------------------------------------------------------

(defmethod -start :failure [this db succ-cb fail-cb]
  ;; TODO: first, check for stale. if not, fire callback immediately
  (start (assoc this :-status :idle) db succ-cb fail-cb))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti -render (fn [this _]
                    (let [s (status this)]
                      (console/debug ::-render s)
                      s)))

(defmethod -render :default [this db]
  (console/error ::-render :unrecognized-status this)
  db)

(defmethod -render :success [{:keys [result] :as this} db]
  ;; attach all decoration affinities for symbols defined in this lexer
  (if-let [success (:success result)]
    (do (console/debug ::-render {:success
                                  (for [[sym m] success]
                                    [sym (select-keys m [:source-str :regex-str])])})
        (let [syms (map first success)]
          (update db :decoration-affinities colors/allocate-affinities (map name syms))))
    db))

(defmethod -render :failure [this db]
  (-> db
      (inject-error-overlays (:token-editor-id this)
                             (malformed-syntax->overlays (get-in this [:result :error])))))

(defn- -clear-previous-render [{:keys [token-editor-id] :as this} db]
  #_(console/debug ::-clear-previous-render)
  (remove-overlay db token-editor-id :error "parse-failure"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Worker Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord CompileLexerWorker [token-editor-id
                               token-source-string
                               -status
                               listeners
                               result]

  IWorker
  (status [this]
    -status)

  (render [this db]
    (-render this (-clear-previous-render this db)))

  (cache-key [this]
    (->cache-key (:token-editor-id this)))

  (dependencies [_ _]
    nil)

  (current-progress [this _]
    (if (#{:failure :success} (status this))
      1
      0))

  (max-progress [_ _]
    1)

  (progress-description [this db]
    (case (status this)
      :idle ""
      :running "Compiling lexer"
      :success "Finished compiling lexer"
      :failure
      (if-let [causes (get-in result [:error :causes])]
        (cond
          (:parse-failure causes)
          [source-hyperlink {:source-id (first (q/editors->sources db [token-editor-id]))
                             :label "Token definition"
                             :suffix (str "has syntax error")}]
          :else
          (str causes))
        "Unknown cause")
      "Unknown status"))

  IAsyncWorker
  (start [this db succ-cb fail-cb]
    (-start this db succ-cb fail-cb))

  (complete-success [this payload]
    (console/debug ::complete-success)
    (let [this
          (-> this
              (assoc :-status :success)
              (assoc-in [:result :success] payload))]
      (doseq [cb (get-in this [:listeners :succ-cb])]
        (cb this))
      (-> this
          (update-in [:listeners :succ-cb] empty)
          (update-in [:listeners :fail-cb] empty))))

  (complete-failure [this payload]
    (console/debug ::complete-failure {:cache-key (cache-key this)})
    (let [this
          (-> this
              (assoc :-status :failure)
              (assoc-in [:result :error] payload))]
      (doseq [cb (get-in this [:listeners :fail-cb])]
        (cb this))
      (-> this
          (update-in [:listeners :succ-cb] empty)
          (update-in [:listeners :fail-cb] empty)))))

(defn compile-lexer-worker [token-editor-id]
  (map->CompileLexerWorker
   {:token-editor-id token-editor-id
    :token-source-string nil
    :-status :idle
    :listeners {:succ-cb []
                :fail-cb []}
    :result {}}))
