(ns parsimony.workers.test-answer
  (:refer-clojure :exclude [-reset])
  (:require [cljs.reader :as reader]
            [clojure.set :as set]
            [parsimony.classroom.api :as classroom.api]
            [parsimony.config :as config]
            [parsimony.inference :refer [run-parser-unconstrained]]
            [parsimony.lexer :as lexer]
            [parsimony.query :as q]
            [parsimony.parser :as parser]
            [parsimony.asm.parser :as asm.parser]
            [parsimony.models.editor :as editor]
            [parsimony.util :refer [matches-schema? pprint-str]]
            [parsimony.worker :refer [IWorker ISyncWorker step reset status render cache-key current-progress max-progress progress-description]]
            [parsimony.workers.batch-lexer :as workers.batch-lexer]
            [parsimony.workers.compile-lexer :as workers.compile-lexer]
            [parsimony.workers.compile-parser :as workers.compile-parser]
            [re-frame.core :refer [dispatch]]
            [schema.core :as s :include-macros true]
            [parsimony.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def schema
  {:-status (s/enum :success :failure :idle :running)
   :stage (s/maybe (s/pred :name))
   :token-editor-id s/Num
   :cfg-editor-id s/Num
   :target-editor-id s/Num
   :lexer s/Any
   :parser s/Any
   :result (s/maybe {(s/optional-key :pass?) s/Bool
                     (s/optional-key :error) s/Any})})

(defn verbose-matches-schema?
  [a-schema this]
  (matches-schema? a-schema this {:worker :test-answer
                                  :status (status this)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ->cache-key [token-editor-id cfg-editor-id target-editor-id]
  {:algo :test-answer
   :token-editor-id token-editor-id
   :cfg-editor-id cfg-editor-id
   :target-editor-id target-editor-id})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run Stage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti run-stage (fn [this _]
                      {:pre [(verbose-matches-schema? schema this)]}
                      (get-in this [:stage :name])))

(defmethod run-stage :default [this _]
  (console/error ::run-stage :unrecognized-stage this)
  (assoc this
         :-status :failure
         :stage nil))

(defn check-lexer [lexer answer-key]
  (if-let [error (first
                   (for [{:keys [string expected]} answer-key
                         :let [tokens (vec (lexer/lex lexer string))]
                         :when (not= tokens expected)]
                     {:pass? false
                      :error
                      (if (lexer/lex-error? (peek tokens))
                        ;; lex error
                        {:causes #{:lexer-failure}
                         :string string
                         :expected expected
                         :actual tokens
                         :message
                         [:div.test-failure
                          "Lexer fails on string"
                          [:pre string]
                          "expected"
                          (workers.batch-lexer/token-list->hiccup expected)]}
                        ;; no lex error
                        {:causes #{:tokens-do-not-match}
                         :string string
                         :expected expected
                         :actual tokens
                         :message
                         [:div.test-failure
                          "Incorrect lex for string"
                          [:pre string]
                          "expected"
                          (workers.batch-lexer/token-list->hiccup expected)
                          "actual"
                          (workers.batch-lexer/token-list->hiccup tokens)]})}))]
    error
    {:pass? true}))

(defmethod run-stage :check-lexer [{:keys [stage lexer] :as this} db]
  (if (seq (get-in stage [:answer-key :lexer]))
    (let [result (check-lexer lexer (get-in stage [:answer-key :lexer]))
          this (assoc this :result result)]
      (console/debug ::run-stage :check-lexer)
      (if (:pass? result)
        (assoc-in this [:stage :name] :check-parser)
        (assoc this
               :stage nil
               :-status :failure)))
    (do (console/debug ::run-stage :skipping-check-lexer)
        (assoc-in this [:stage :name] :check-parser))))

(defn- parse-string [lexer parser start-nt string]
  (let [tokens (lexer/lex lexer string)]
    (when-not (lexer/lex-error? (last tokens))
      (let [{:keys [cyk codec]} (run-parser-unconstrained parser tokens)
            token-vec (into [] (map :label) tokens)
            forest (-> (asm.parser/reconstruct codec
                                               cyk
                                               token-vec
                                               parser
                                               start-nt 0 (count tokens))
                       (parser/disambiguate parser))]
        (asm.parser/cpp-free cyk)
        {:tokens tokens
         :forest forest}))))

(defn check-parser [lexer parser answer-key]
  (if-let [error
           (first
             (remove nil?
                     (for [{:keys [string nt expected forbidden]} answer-key
                           :let [{:keys [forest tokens]} (parse-string lexer parser nt string)
                                 nodes (parser/all-nodes forest)
                                 expected (set expected)
                                 forbidden (set forbidden)
                                 expected-found (set/intersection nodes expected)
                                 forbidden-found (set/intersection nodes forbidden)]]
                       (cond
                         (not= expected expected-found)
                         (let [missing (set/difference expected expected-found)
                               [nt i l] (first missing)
                               [from-char to-char] (lexer/token-range->char-range tokens i l)
                               substring (if (and from-char to-char)
                                           (subs string from-char to-char)
                                           "")]
                           {:pass? false
                            :error {:causes #{:missing-parse-nodes}
                                    :string string
                                    :tokens tokens
                                    :forest forest
                                    :expected expected
                                    :expected-found expected-found
                                    :missing missing
                                    :message
                                    [:div.test-failure
                                     "Incorrect parse: expected " [:code nt] "for"
                                     [:pre substring]]}})
                         (seq forbidden-found)
                         (let [[nt i l] (first forbidden-found)
                               [from-char to-char] (lexer/token-range->char-range tokens i l)
                               substring (if (and from-char to-char)
                                           (subs string from-char to-char)
                                           "")]
                         {:pass? false
                          :error {:causes #{:forbidden-parse-nodes}
                                  :string string
                                  :tokens tokens
                                  :forest forest
                                  :forbidden forbidden
                                  :forbidden-found forbidden-found
                                  :message
                                  [:div.test-failure
                                   "Incorrect parse: did not expect " [:code nt] "for"
                                   [:pre substring]]}})
                         :else nil))))]
    error
    {:pass? true}))

(defmethod run-stage :check-parser
  [{:keys [stage lexer parser] :as this} db]
  (if (seq (get-in stage [:answer-key :parser]))
    (let [result (check-parser lexer parser (get-in stage [:answer-key :parser]))
          this (assoc this :result result)]
      (console/debug ::run-stage :check-parser)
      (if (:pass? result)
        (assoc this
               :stage nil
               :-status :success)
        (assoc this
               :stage nil
               :-status :failure)))
    (do (console/debug ::run-stage :skipping-check-parser)
        (assoc this
               :stage nil
               :-status :success))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti -step (fn [this _]
                  {:pre [(verbose-matches-schema? schema this)]}
                  (status this)))

(defmethod -step :default [this db]
  (do (console/error ::-step :unrecognized-status this)
      (assoc this
             :-status :failure
             :stage nil)))

;;------------------------------------------------------------------------------
;; Idle
;;------------------------------------------------------------------------------

(defmethod -step :idle [{:keys [token-editor-id cfg-editor-id target-editor-id] :as this} db]
  (if-let [answer-key (classroom.api/get-answer-key db target-editor-id)]
    (let [lexer (get-in db [:workers
                            (workers.compile-lexer/->cache-key token-editor-id)
                            :result
                            :success])
          parser (get-in db [:workers
                             (workers.compile-parser/->cache-key
                               token-editor-id
                               cfg-editor-id)
                             :result
                             :success])]
      (console/debug ::-step :idle {:answer-key answer-key})
      (assoc this
             :-status :running
             :stage {:name :check-lexer :answer-key answer-key}
             :lexer lexer
             :parser parser
             :result nil))
    (do (console/debug ::-step :idle :no-answer-key-found)
        (assoc this
               :-status :success
               :result nil))))

;;------------------------------------------------------------------------------
;; Running
;;------------------------------------------------------------------------------

(defmethod -step :running [this db]
  (run-stage this db))

;;------------------------------------------------------------------------------
;; Success
;;------------------------------------------------------------------------------

(defmethod -step :success [this db]
  (step (assoc this :-status :idle) db))

;;------------------------------------------------------------------------------
;; Failure
;;------------------------------------------------------------------------------

(defmethod -step :failure [this db]
  (step (assoc this :-status :idle) db))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Render
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti -render (fn [this _]
                    (status this)))

(defmethod -render :default [this db]
  (console/error ::-render :unrecognized-status this)
  db)

(defmethod -render :success [{:keys [target-editor-id] :as this} db]
  (console/debug ::-render :success)
  (if-let [source (editor/backing-source db target-editor-id)]
    (do (dispatch [:log/success [:div (str "All tests passed for " (:source-path source) ".")]])
        (assoc-in db [:classroom (:id source)] true))
    db))

(defmethod -render :failure [{:keys [result] :as this} db]
  (console/debug ::-render :failure)
  (if-let [error (:error result)]
    (do (console/error ::-render :failure {:error error})
        (dispatch [:log/error (:message error)])
        db)
    (do (console/debug ::-render :failure :no-error)
        db)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Progress
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -max-progress [this db]
  1)

(defn- -current-progress [{:keys [stage] :as this} db]
  (if (contains? #{:failure :success} (status this))
    1
    0))

(defn- -progress-description [this db]
  (case (status this)
    :idle ""
    :running "Testing"
    :success "All tests passed"
    :failure "Tests failed"
    ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Worker Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord TestAnswerWorker
  [-status stage token-editor-id cfg-editor-id target-editor-id lexer parser result]

  IWorker

  (status [this]
    (:-status this))

  (render [this db]
    (-render this db))

  (cache-key [{:keys [token-editor-id cfg-editor-id target-editor-id] :as this}]
    (->cache-key token-editor-id cfg-editor-id target-editor-id))

  (dependencies [{:keys [token-editor-id cfg-editor-id target-editor-id] :as this} db]
    (let [answer-key (classroom.api/get-answer-key db target-editor-id)]
      (-> #{}
          (into
            (when (seq (:lexer answer-key))
              [(workers.compile-lexer/->cache-key token-editor-id)]))
          (into
            (when (seq (:parser answer-key))
              [(workers.compile-lexer/->cache-key token-editor-id)
               (workers.compile-parser/->cache-key token-editor-id cfg-editor-id)]))
          (vec))))

  (current-progress [this db]
    (-current-progress this db))

  (max-progress [this db]
    (-max-progress this db))

  (progress-description [this db]
    (-progress-description this db))

  ISyncWorker

  (step [this db]
    (-step this db))

  (reset [this]
    this))

(cljs.reader/register-tag-parser! "parsimony.workers.test-answer.TestAnswerWorker" map->TestAnswerWorker)

(defn test-answer-worker [token-editor-id cfg-editor-id target-editor-id]
  (map->TestAnswerWorker
    {:token-editor-id token-editor-id
     :cfg-editor-id cfg-editor-id
     :target-editor-id target-editor-id
     :-status :idle
     :stage nil
     :lexer nil
     :parser nil
     :result nil}))
