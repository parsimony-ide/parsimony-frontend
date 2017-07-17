(ns parsimony.workers.parser
  "A worker that runs a parser definition against a target editor"
  (:refer-clojure :exclude [-reset])
  (:require [medley.core :refer [dissoc-in]]
            [parsimony.parser :as parser]
            [parsimony.asm.parser :as asm.parser]
            [parsimony.lexer :as lexer]
            [parsimony.util :refer [matches-schema? pprint-str]]
            [parsimony.views.info :refer [IDetail]]
            [parsimony.worker :refer [IWorker ISyncWorker step reset status render cache-key current-progress max-progress progress-description]]
            [parsimony.models.colors :refer [dashed-outline-mod]]
            [parsimony.models.editor :refer [-string inject-normal-overlays remove-overlays ->char-range apply-emphasis polygon-index exclusively-enable-overlays-by-type]]
            [parsimony.models.overlay :as overlay]
            [parsimony.models.parse-dashboard :as parse-dashboard]
            [parsimony.query :as q]
            [parsimony.workers.common :refer [source-hyperlink] :as workers.common]
            [parsimony.workers.compile-lexer :as compile-lexer]
            [parsimony.workers.compile-parser :as compile-parser]
            [parsimony.workers.lexer :as workers.lexer]
            [re-frame.core :refer [dispatch]]
            [schema.core :as s :include-macros true]
            [parsimony.console :as console]))

;; one of #{:fast :nice} to choose either:
;; - :fast = asm.js without preemption (CYK::parse and CYK::colorize)
;; - :nice = asm.js with preemption (CYK::parse_partial and CYK::colorize_partial)
(def implementation :fast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def schema
  (merge workers.common/schema
         {:token-editor-id s/Num
          :cfg-editor-id s/Num
          :target-editor-id s/Num
          :target-tokens (s/maybe s/Any)
          :compiled-parser (s/maybe s/Any)
          :stage (s/maybe (s/pred :name))
          :result (s/maybe {(s/optional-key :success) s/Any
                            (s/optional-key :error) s/Any})}))

(defn verbose-matches-schema?
  [a-schema this]
  (matches-schema? a-schema this {:worker :parser
                                  :status (status this)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ->cache-key [token-editor-id cfg-editor-id target-editor-id]
  {:algo :parser
   :token-editor-id token-editor-id
   :cfg-editor-id cfg-editor-id
   :target-editor-id target-editor-id})

(defn fast-parse-forest
  "Return the parse forest for the given nt i l triplet. Fast asm.js implementation."
  [{:keys [target-tokens compiled-parser result] :as this} nt [start end :as char-range]]
  (let [[i l :as token-range] (lexer/char-range->token-range target-tokens start end)
        token-vec (into [] (map :label) target-tokens)]
    (when (some? token-range)
      (when-let [{:keys [cyk codec]} (:success result)]
        (-> (asm.parser/reconstruct codec
                                    cyk
                                    token-vec
                                    compiled-parser
                                    nt i l)
            (parser/disambiguate compiled-parser))))))

(defn parse-forest
  [this & args]
  (when-not (= :running (status this))
    (case implementation
      :fast (apply fast-parse-forest this args)
      :nice (apply fast-parse-forest this args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Step
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti -step (fn [this db]
                  {:pre [(verbose-matches-schema? schema this)]}
                  (let [s (status this)]
                    (console/debug ::-step s)
                    s)))

(defmethod -step :default [this _]
  (console/error ::-step :unrecognized-status this)
  (assoc this :-status :failure))

;; FSMs:
;; - fast stages: nil -> :init-fast-cyk -> :apply-negative-labels -> :fast-cyk -> :fast-coloring -> nil
;; - nice stages: nil -> :init-nice-cyk -> :apply-negative-labels -> (cycle :nice-cyk) -> :init-nice-coloring -> (cycle :nice-coloring) -> nil

(defmethod -step :idle [{:keys [token-editor-id cfg-editor-id target-editor-id] :as this} db]
  (let [l-cache-key (workers.lexer/->cache-key token-editor-id target-editor-id)]
    (if-let [l-worker (get-in db [:workers l-cache-key])]
      (let [cp-cache-key (compile-parser/->cache-key token-editor-id cfg-editor-id)]
        (if-let [cp-worker (get-in db [:workers cp-cache-key])]
          (do
            ;; free previously allocated heap space used by asm.parser
            (when-let [cyk (get-in this [:result :success :cyk])]
              (asm.parser/cpp-free cyk))

            (-> this
                (assoc :result nil) ;; discard old result
                (assoc :compiled-parser (get-in cp-worker [:result :success])
                       :target-tokens (get-in l-worker [:result :success])
                       :-status :running
                       :stage {:name (case implementation
                                       :fast :init-fast-cyk
                                       :nice :init-nice-cyk)})))
          (do (console/error ::-step :idle (str "No compile-parser worker with cache key " cp-cache-key " found"))
              this)))
      (do (console/error ::-step :idle (str "No lexer worker with cache key " l-cache-key " found"))
          this))))

(declare run-stage)

(defmethod -step :running [this db]
  (run-stage this db))

(defmethod -step :success [this db]
  (step (assoc this :-status :idle) db))

(defmethod -step :failure [this db]
  (step (assoc this :-status :idle) db))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run Stage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti run-stage (fn [this _]
                      #_{:pre [(verbose-matches-schema? schema this)]}
                      (let [s (get-in this [:stage :name])]
                        (console/debug ::run-stage s)
                        s)))

(defmethod run-stage :default [this _]
  (console/error ::run-stage :unrecognized-stage this)
  (assoc this
         :-status :failure
         :stage nil))

;; -----------------------------------------------------------------------------
;; Apply Negative Labels
;; -----------------------------------------------------------------------------

(defmethod run-stage :apply-negative-labels
  [{:keys [target-editor-id target-tokens result] :as this} db]
  (let [target-string (-string db target-editor-id)
        samples (parse-dashboard/all-samples (:parse-dashboard db))
        matching-samples (filter #(= target-string (:string %)) samples)
        negative-labels (into []
                              (comp (mapcat :labels)
                                    (filter #(= :negative (:type %)))
                                    (map (partial parse-dashboard/sample-label->token-indexed-label target-tokens)))
                              matching-samples)
        {:keys [cyk codec]} (:success result)]
    #_(console/debug ::run-stage :apply-negative-labels {:matching-samples matching-samples
                                                         :negative-labels negative-labels})
    ;; XXX: I've commented the below since I'm not sure that the parse worker should apply negative labels. The issue is
    ;; that the parse worker should reflect the current state of the grammar, but applying negative labels can cause
    ;; subsequent colorings and parse forests to exclude negated nodes even though the parser would otherwise generate
    ;; them. This gives the illusion that things are "fixed" when in fact they've just been hidden by a negative label.
    #_(doseq [[nt i l] negative-labels]
        (if (= 1 l)
          ;; if l = 1, then it's set as part of cpp-init-cyk, so unset it
          (do (console/debug ::unset-1 nt i l)
              (asm.parser/unset-cyk codec cyk nt i l))
          ;; if l > 1, then it's set as part of cpp-run-cyk, so set it
          (do (console/debug ::unset-2 nt i l)
              (asm.parser/set-cyk codec cyk nt i l))))
    (-> this
        (assoc-in [:stage :name]
                  (case implementation
                    :fast :fast-cyk
                    :nice :nice-cyk)))))

;; -----------------------------------------------------------------------------
;; Fast CYK
;; -----------------------------------------------------------------------------

(defmethod run-stage :init-fast-cyk
  [{:keys [compiled-parser target-tokens] :as this} db]
  (let [result
        (try
          (let [{:keys [cyk codec exec-time]} (asm.parser/cpp-init-cyk (into [] (map :label) target-tokens) compiled-parser)]
            (console/debug ::cyk-runtime {:init exec-time})
            {:success {:cyk cyk :codec codec}})
          (catch js/Error e
            (console/error ::run-stage :init-fast-cyk {:error e})
            {:error (ex-data e)}))]
    (assoc this
           :result result
           :-status (if (:error result) :failure :running)
           :stage (if (:error result) nil {:name :apply-negative-labels}))))

(defmethod run-stage :fast-cyk
  [{:keys [result] :as this} db]
  (let [result
        (try
          (let [{:keys [cyk codec]} (:success result)
                cyk-time (asm.parser/cpp-run-cyk cyk)]
            (console/debug ::cyk-runtime {:cyk cyk-time})
            result)
          (catch js/Error e
            (console/error ::run-stage :fast-cyk {:error e})
            {:error (ex-data e)}))]
    (assoc this
           :result result
           :-status (if (:error result) :failure :running)
           :stage (if (:error result) nil {:name :fast-coloring}))))

;; -----------------------------------------------------------------------------
;; Nice CYK
;; -----------------------------------------------------------------------------

(defmethod run-stage :init-nice-cyk
  [{:keys [compiled-parser target-tokens] :as this} db]
  (let [result
        (try
          (let [{:keys [cyk codec exec-time]} (asm.parser/cpp-init-cyk (into [] (map :label) target-tokens) compiled-parser)]
            (console/debug ::cyk-runtime {:init exec-time})
            {:success {:cyk cyk :codec codec}})
          (catch js/Error e
            (console/error ::run-stage :init-nice-cyk {:error e})
            {:error (ex-data e)}))]
    (assoc this
           :result result
           :-status (if (:error result) :failure :running)
           :stage (if (:error result) nil {:name :apply-negative-labels
                                           :l 2
                                           :cyk-time 0
                                           :cyk-wall-time (system-time)
                                           :lmax (asm.parser/cpp-get-lmax (get-in result [:success :cyk]))}))))

(defmethod run-stage :nice-cyk
  [{:keys [stage result] :as this} db]
  (let [{:keys [cyk]} (:success result)
        {:keys [l exec-time]} (asm.parser/cpp-run-cyk-partial cyk (:l stage))]
    (if (= 0 l)
      (do
        (console/debug ::cyk-runtime {:cyk (+ exec-time (:cyk-time stage))
                                      :cyk-wall (- (system-time) (:cyk-wall-time stage))})
        (-> this
            (assoc-in [:stage :name] :init-nice-coloring)
            (dissoc-in [:stage :cyk-time])
            (dissoc-in [:stage :cyk-wall-time])))
      (-> this
          (assoc-in [:stage :l] l)
          (update-in [:stage :cyk-time] + exec-time)))))

;; -----------------------------------------------------------------------------
;; Fast Coloring
;; -----------------------------------------------------------------------------

(defmethod run-stage :fast-coloring
  [{:keys [result] :as this} db]
  (let [result
        (try
          (let [{:keys [cyk]} (:success result)
                color-time (asm.parser/cpp-run-color cyk)]
            (console/debug ::cyk-runtime {:color color-time})
            result)
          (catch js/Error e
            (console/error ::run-stage :fast-coloring {:error e})
            {:error (ex-data e)}))]
    (assoc this
           :result result
           :-status (if (:error result) :failure :success)
           :stage nil)))

;; -----------------------------------------------------------------------------
;; Nice Coloring
;; -----------------------------------------------------------------------------

(defmethod run-stage :init-nice-coloring
  [{:keys [stage result] :as this} db]
  (let [{:keys [cyk]} (:success result)
        exec-time (asm.parser/cpp-init-color-partial cyk)]
    (console/debug ::cyk-runtime {:init-color exec-time})
    (-> this
        (assoc-in [:stage :name] :nice-coloring)
        (assoc-in [:stage :color-time] 0)
        (assoc-in [:stage :l] 2))))

(defmethod run-stage :nice-coloring
  [{:keys [stage result] :as this} db]
  (let [{:keys [cyk]} (:success result)
        {:keys [l exec-time]} (asm.parser/cpp-run-color-partial cyk (:l stage))]
    (if (= 0 l)
      (do
        (console/debug ::cyk-runtime {:color (+ exec-time (:color-time stage))})
        (assoc this :-status :success :stage nil))
      (-> this
          (assoc-in [:stage :l] l)
          (update-in [:stage :color-time] + exec-time)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Progress
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- nice-max-progress [{:keys [stage] :as this} db]
  (if-let [lmax (:lmax stage)]
    (* 2 lmax)
    100))

(defn- nice-progress [{:keys [stage] :as this} db]
  (case (:name stage)
    :init-nice-cyk 0
    :nice-cyk (:l stage)
    :init-nice-coloring (:lmax stage)
    :nice-coloring (+ (:lmax stage) (:l stage))
    (max-progress this db)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare parser-worker)

(defn- -reset [{:keys [token-editor-id cfg-editor-id target-editor-id] :as this}]
  (when-let [cyk (get-in this [:result :success :cyk])]
    (try
      (asm.parser/cpp-free cyk)
      (catch js/Error _
        (console/warn ::-reset "Free failed"))))
  (parser-worker token-editor-id cfg-editor-id target-editor-id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti -render (fn [this _]
                    (let [s (status this)]
                      (console/debug ::-render {:status s :cache-key (cache-key this)})
                      s)))

(defmethod -render :default [this db]
  (console/error ::-render :unrecognized-status this)
  db)

(defn- coloring->overlays [coloring tokens]
  (reduce
    (fn [overlays [sym i l :as ti-label]]
      (if-let [[start end :as char-range] (lexer/token-range->char-range tokens i l)]
        (update overlays (name sym) (fnil conj []) [start end nil])
        overlays))
    {}
    (sort-by second coloring)))

(defn- remaining-overlays
  "Not all nonterminals are represented by a coloring, yet we want to inject empty overlays so that the editor knows
  about them, and so they get assigned colors. This function returns those empty overlays."
  [compiled-parser]
  (into {}
        (comp (remove #{:&}) ;; ignore placeholder
              (map #(vector (name %) [])))
        (parser/lhs-nts (:productions compiled-parser))))

(defn- apply-ambiguous-emphasis-mods [db editor-id codec cyk tokens parser coloring]
  (let [token-vec (into [] (map :label) tokens)
        ambiguous? (fn [[nt i l]]
                     (let [forest (-> (asm.parser/reconstruct codec
                                                              cyk
                                                              token-vec
                                                              parser
                                                              nt i l)
                                      (parser/disambiguate parser))]
                       (seq (parser/ambiguous-nodes forest))))
        ambiguous-labels (into #{}
                               (filter ambiguous?)
                               coloring)]
    (reduce
      (fn [db [nt i l]]
        (if-let [[start end] (lexer/token-range->char-range tokens i l)]
          (if-let [index (polygon-index db editor-id :parse (name nt) start end)]
            (apply-emphasis db editor-id :parse (name nt) :ambiguous index dashed-outline-mod)
            db)
          db))
      db
      ambiguous-labels)))

(defmethod -render :success
  [{:keys [target-editor-id target-tokens compiled-parser] :as this} db]
  (let [{:keys [cyk codec]} (get-in this [:result :success])
        max-coloring (asm.parser/get-colors codec cyk 0 (count target-tokens))
        min-coloring (asm.parser/succinct-coloring codec
                                                   cyk
                                                   (into [] (map :label) target-tokens)
                                                   compiled-parser
                                                   max-coloring)
        overlays (coloring->overlays min-coloring target-tokens)]
    #_(console/debug ::-render {:max-coloring max-coloring
                                :min-coloring min-coloring
                                :overlays overlays})
    ;; XXX: kind of ugly to dispatch :live-parse-view/refresh here, but we need
    ;; to make sure live-parse-view stays in sync with the potentially new
    ;; parse forest as a result of re-parse. Due to the nature of dispatch
    ;; scheduling, we know the handler will not execute until after this render
    ;; function returns
    (dispatch [:live-parse-view/refresh])
    (-> db
        (inject-normal-overlays target-editor-id :parse (remaining-overlays compiled-parser))
        (inject-normal-overlays target-editor-id :parse overlays)
        (apply-ambiguous-emphasis-mods target-editor-id codec cyk target-tokens compiled-parser min-coloring)
        (exclusively-enable-overlays-by-type target-editor-id :parse))))

(defmethod -render :failure
  [this db]
  db)

(defn- -clear-previous-render [{:keys [target-editor-id] :as this} db]
  #_(console/debug ::-clear-previous-render)
  (remove-overlays db target-editor-id :parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Worker Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord ParserWorker [token-editor-id
                         cfg-editor-id
                         target-editor-id
                         compiled-parser
                         target-tokens
                         -status
                         stage
                         result]

  IWorker

  (status [this]
    (:-status this))

  (render [this db]
    (-render this (-clear-previous-render this db)))

  (cache-key [this]
    (->cache-key (:token-editor-id this) (:cfg-editor-id this) (:target-editor-id this)))

  (dependencies [{:keys [token-editor-id cfg-editor-id target-editor-id] :as this} _]
    [(compile-parser/->cache-key token-editor-id cfg-editor-id)
     (workers.lexer/->cache-key token-editor-id target-editor-id)])

  (current-progress [{:keys [stage] :as this} db]
    (if (= :nice implementation)
      ;; provide fine-grained progress measure when using the :nice implementation
      (nice-progress this db)
      ;; use a very coarse progress measure otherwise
      (if (#{:failure :success} (status this))
        (max-progress this db)
        0)))

  (max-progress [this db]
    (if (= :nice implementation)
      (nice-max-progress this db)
      1))

  (progress-description [{:keys [stage result] :as this} db]
    (case (status this)
      :idle ""
      :running
      (case (:name stage)
        :fast-cyk "Parsing (fast)"
        :init-nice-cyk "Parsing (nice)"
        :init-fast-cyk "Parsing (fast)"
        :nice-cyk "Parsing (nice)"
        :fast-coloring "Coloring (fast)"
        :init-nice-coloring "Coloring (nice)"
        :nice-coloring "Coloring (nice)"
        :apply-negative-labels "Applying negative labels"
        ;; default
        (str (:name stage)))
      :success "Finished parsing"
      :failure
      (if-let [{:keys [causes]} (:error result)]
        (cond
          (:empty-token-stream causes)
          [source-hyperlink {:source-id (first (q/editors->sources db [target-editor-id]))
                             :label "Sample file"
                             :suffix "is empty"}]
          (:empty-grammar causes)
          [source-hyperlink {:source-id (first (q/editors->sources db [cfg-editor-id]))
                             :label "Grammar definition"
                             :suffix "is empty"}]
          :else
          (str causes))
        "")
      ""))

  ISyncWorker

  (step [this db]
    (-step this db))

  (reset [this]
    (-reset this)))

(cljs.reader/register-tag-parser! "parsimony.workers.parser.ParserWorker" map->ParserWorker)

(defn parser-worker [token-editor-id cfg-editor-id target-editor-id]
  (map->ParserWorker
   {:token-editor-id token-editor-id
    :cfg-editor-id cfg-editor-id
    :target-editor-id target-editor-id
    :target-tokens nil
    :-status :idle
    :stage nil
    :result nil}))
