(ns parsimony.workers.lexer
  "A worker that runs a lexer definition against a target editor"
  (:require [parsimony.lexer :as lexer]
            [parsimony.query :as q]
            [parsimony.workers.compile-lexer :as compile-lexer]
            [parsimony.util :refer [pprint-str]]
            [parsimony.views.info :refer [IDetail]]
            [parsimony.worker :refer [IWorker ISyncWorker step reset status render cache-key current-progress max-progress progress-description]]
            [parsimony.workers.common :refer [source-hyperlink] :as workers.common]
            [parsimony.models.colors :as colors]
            [parsimony.models.editor
             :refer [-string inject-normal-overlays inject-error-overlays
                     remove-overlay remove-overlays ->char-range
                     disable-overlay enable-overlay exclusively-enable-overlays-by-type]]
            [re-com.core :refer [v-box label]]
            [parsimony.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ->cache-key [token-editor-id target-editor-id]
  {:algo :lexer
   :token-editor-id token-editor-id
   :target-editor-id target-editor-id})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -step-idle [this db]
  (let [cl-cache-key (compile-lexer/->cache-key (:token-editor-id this))]
    (if-let [cl-worker (get-in db [:workers cl-cache-key])]
      ;; XXX: should check for cl-worker success here
      (assoc this
             :compiled-lexer (get-in cl-worker [:result :success])
             :target-source-string (-string db (:target-editor-id this))
             :-status :running)
      (do
        (console/error ::-step-idle (str "No compile-lexer worker with cache key " cl-cache-key " found"))
        this))))

(defn- -step-running [this db]
  (let [result
        (try
          (let [tokens (lexer/lex (:compiled-lexer this) (:target-source-string this))
                failed? (lexer/lex-error? (last tokens))]
            (if-not failed?
              {:success tokens}
              {:success (drop-last tokens)
               :error (last tokens)}))
          (catch js/Error e
            {:error (ex-data e)}))]
    #_(console/debug ::-step-running {:result result})
    (assoc this
           :result result
           :-status (if (:error result) :failure :success))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -tokens->overlays
  "Return map from token label to corresponding overlay"
  [tokens]
  (let [f (fn [acc {:keys [label start end] :as token}]
            (update-in acc [(name label)] (fnil conj []) [start end nil]))]
    (reduce f {} tokens)))

(defrecord NoMatchingTokenInfo [fail-idx]
    IDetail
    (detail-view [this]
      [v-box
       :children [[label :label (str "No token match found")]]]))

(defn no-matching-token-info [fail-idx]
  (map->NoMatchingTokenInfo {:fail-idx fail-idx}))

(defn- -no-matching-token->overlays
  [{:keys [fail-idx] :as error}]
  (console/debug ::-no-matching-token->overlays {:fail-idx fail-idx})
  {(name (:reason error)) [(->char-range fail-idx
                                         (inc fail-idx)
                                         (no-matching-token-info fail-idx))]})

(defn- -render-success [{:keys [target-editor-id] :as this} db]
  #_(console/debug ::-render-success)
  (-> db
      (inject-normal-overlays
        target-editor-id
        :tokens
        (-tokens->overlays (get-in this [:result :success]))
        colors/no-mod)
      (exclusively-enable-overlays-by-type target-editor-id :tokens)
      ;; disable the whitespace overlay by default
      (disable-overlay target-editor-id :tokens "ws")))

(defn- -render-failure [{:keys [target-editor-id] :as this} db]
  #_(console/debug ::-render-failure)
  (-> db
      (inject-normal-overlays
        target-editor-id
        :tokens
        (-tokens->overlays (get-in this [:result :success]))
        colors/no-mod)
      (inject-error-overlays
        target-editor-id
        (-no-matching-token->overlays (get-in this [:result :error])))
      (exclusively-enable-overlays-by-type target-editor-id :tokens)
      (enable-overlay target-editor-id :error "no-matching-token")))

(defn- -clear-previous-render
  "Remove vestiges of previous runs of this worker"
  [this db]
  #_(console/debug ::-clear-previous-render)
  (-> db
      (remove-overlay (:target-editor-id this) :error "no-matching-token")
      (remove-overlays (:target-editor-id this) :tokens)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Worker Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare lexer-worker)

(defrecord LexerWorker [token-editor-id
                        target-editor-id
                        compiled-lexer
                        target-source-string
                        -status
                        result]

  IWorker

  (status [this]
    (:-status this))

  (render [this db]
    (console/debug ::render {:status (status this) :cache-key (cache-key this)})
    (let [db (-clear-previous-render this db)]
      (case (status this)
        :success
        (-render-success this db)
        :failure
        (-render-failure this db)
        ;; default
        db)))

  (cache-key [this]
    (->cache-key (:token-editor-id this) (:target-editor-id this)))

  (dependencies [{:keys [token-editor-id] :as this} _]
    [(compile-lexer/->cache-key token-editor-id)])

  (current-progress [this _]
    (if (#{:failure :success} (status this))
      1
      0))

  (max-progress [_ _]
    1)

  (progress-description [this db]
    (case (status this)
      :running "Lexing"
      :success "Finished lexing"
      :failure
      [source-hyperlink {:source-id (first (q/editors->sources db [target-editor-id]))
                         :label "Sample file"
                         :suffix "has unmatched characters"}]
      ""))

  ISyncWorker

  (step [this db]
    (case (status this)
      :idle (-step-idle this db)
      :running (-step-running this db)
      :success (step (assoc this :-status :idle) db)
      :failure (step (assoc this :-status :idle) db)
      ;; default
      (do (console/error ::step :no-matching-clause {:this this})
          this)))

  (reset [this]
    (lexer-worker token-editor-id target-editor-id)))

(cljs.reader/register-tag-parser! "parsimony.workers.lexer.LexerWorker" map->LexerWorker)

(defn lexer-worker [token-editor-id target-editor-id]
  (map->LexerWorker
   {:token-editor-id token-editor-id
    :target-editor-id target-editor-id
    :target-source-string nil
    :-status :idle
    :result nil}))
