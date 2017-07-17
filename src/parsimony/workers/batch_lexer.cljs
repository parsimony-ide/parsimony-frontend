(ns parsimony.workers.batch-lexer
  (:require [parsimony.lexer :as lexer]
            [parsimony.query :as q]
            [parsimony.workers.compile-lexer :as compile-lexer]
            [parsimony.workers.lexer :as workers.lexer]
            [parsimony.util :refer [pprint-str string-idx->line-col]]
            [parsimony.views.info :refer [IDetail]]
            [parsimony.worker :refer [IWorker ISyncWorker step reset status render cache-key current-progress max-progress progress-description]]
            [parsimony.workers.common :refer [source-hyperlink] :as workers.common]
            [parsimony.models.colors :as colors]
            [parsimony.models.editor
             :refer [-string inject-normal-overlays inject-error-overlays
                     remove-overlay remove-overlays ->char-range
                     disable-overlay]]
            [re-com.core :refer [v-box label]]
            [re-frame.core :refer [dispatch]]
            [parsimony.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ->cache-key [token-editor-id target-editor-id]
  {:algo :batch-lexer
   :token-editor-id token-editor-id
   :target-editor-id target-editor-id})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- token->hiccup [{:keys [string label] :as token}]
  [:span.token
   [:span.token-label (name label)]
   [:span.token-string string]])

(defn token-list->hiccup [tokens]
  (into [:div.token-list]
        (map token->hiccup)
        tokens))

(defn- -render-success [{:keys [result] :as this} db]
  (console/debug ::-render-success)
  (when-let [tokens (:success result)]
    (dispatch [:log/info [:div
                          [:span (str "Lexer success. Computed " (count tokens) " tokens:")]
                          (token-list->hiccup tokens)]]))
  db)

(defn- -render-failure [{:keys [result target-source-string] :as this} db]
  (console/debug ::-render-failure)
  (when-let [{:keys [fail-idx] :as error} (:error result)]
    (let [{:keys [line col]} (string-idx->line-col target-source-string fail-idx)]
      (dispatch [:log/error [:div
                             [:span (str "Lexer failure at line " line ":" col ". No token match found")]
                             [:div.lexer-failure
                              "First unmatched character is '" (get target-source-string fail-idx) "'"]]])))
  db)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Worker Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare batch-lexer-worker)

(defrecord BatchLexerWorker [token-editor-id
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
    (case (status this)
      :success
      (-render-success this db)
      :failure
      (-render-failure this db)
      ;; default
      db))

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
      :idle (workers.lexer/-step-idle this db)
      :running (workers.lexer/-step-running this db)
      :success (step (assoc this :-status :idle) db)
      :failure (step (assoc this :-status :idle) db)
      ;; default
      (do (console/error ::step :no-matching-clause {:this this})
          this)))

  (reset [this]
    (batch-lexer-worker token-editor-id target-editor-id)))

(cljs.reader/register-tag-parser! "parsimony.workers.batch-lexer.BatchLexerWorker" map->BatchLexerWorker)

(defn batch-lexer-worker [token-editor-id target-editor-id]
  (map->BatchLexerWorker
    {:token-editor-id token-editor-id
     :target-editor-id target-editor-id
     :target-source-string nil
     :-status :idle
     :result nil}))
