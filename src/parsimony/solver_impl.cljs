(ns parsimony.solver-impl
  (:require [clojure.set :as set]
            [parsimony.asm.parser :as asm.parser]
            [parsimony.inference :as inference]
            [parsimony.asm.inference :as asm.inference]
            [parsimony.heuristic :as heuristic :include-macros true]
            [parsimony.lexer :as lexer]
            [parsimony.models.lexer :refer [token-schema]]
            [parsimony.models.parse-dashboard :as parse-dashboard]
            [parsimony.parser :as parser]
            [parsimony.refactor.parser :as refactor.parser]
            [parsimony.util :refer [cartesian-product map-values]]
            [parsimony.console :as console]
            [schema.core :as s :include-macros true]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def cache-entry-schema
  {:string s/Str                         ;; the full text of the given sample
   :tokens [token-schema]                ;; the token stream for the given lexer
   :cyk s/Any                            ;; the asm.parser CYK table
   :codec s/Any                          ;; the asm.parser codec
   :check                                ;; whether each label-id is satisfied via top-down/bottom-up criteria
   {s/Num
    {:type (s/enum :positive :negative)
     :top-down s/Bool
     :bottom-up s/Bool}}})

(def sample-cache-schema
  {s/Num ;; sample-id
   cache-entry-schema})

(def heuristic-schema
  {:type s/Keyword
   :provenance s/Any
   :raw [[s/Num]]
   :path [#{s/Keyword}]
   :path-strings [[s/Str]]
   :params {s/Keyword s/Any}})

(def solution-schema
  {:provenance s/Any
   :candidates [{:path s/Any}]})

(def schema
  {:sample-cache sample-cache-schema
   :lexer s/Any
   :orig-parser s/Any
   :parser s/Any
   :parse-dashboard (select-keys parse-dashboard/schema [:samples])
   :working-set [[(s/one s/Num "sample-id") (s/one s/Any "ti-label")]]
   :unpartitioned-constraint-states [s/Any]
   :partitioned-constraint-states [s/Any]
   :heuristics [heuristic-schema]
   :rejected-heuristics s/Any
   :accepted-heuristics s/Any
   :solutions [solution-schema]})

(def default-model
  {:sample-cache {}
   :lexer nil
   :orig-parser nil
   :parser nil
   :parse-dashboard (select-keys parse-dashboard/default-model [:samples])
   :working-set []
   :unpartitioned-constraint-states []
   :partitioned-constraint-states []
   :heuristics []
   :rejected-heuristics nil
   :accepted-heuristics nil
   :solutions []})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn init [state lexer parser parse-dashboard]
  (console/debug ::init)
  (assoc state
         :lexer lexer
         :orig-parser parser
         :parser parser
         :parse-dashboard (select-keys parse-dashboard [:samples])))

(defn get-cache-entry [{:keys [sample-cache] :as state} sample-id]
  (get sample-cache sample-id))

(defn set-cache-entry-attr [{:keys [sample-cache] :as state} sample-id key attr]
  (assoc-in state [:sample-cache sample-id key] attr))

;;------------------------------------------------------------------------------
;; recompile-parser
;;------------------------------------------------------------------------------

(defn add-filters [parser filters]
  (reduce
    (fn [p f]
      (case (:type f)
        :associativity
        (refactor.parser/add-block-associativity p (:productions f) (:direction f))
        :priority
        (refactor.parser/add-priority p (:low f) (:high f))
        ;; default
        p))
    parser
    filters))

(defn trash-filters
  "If a production is referenced in `filters`, then remove all filters
   belonging to `parser` that reference the production"
  [parser filters]
  (let [referenced-productions
        (into []
              (for [f filters
                    p (case (:type f)
                        :associativity (:productions f)
                        :priority [(:high f) (:low f)]
                        nil)]
                p))
        result (refactor.parser/remove-filter-mentions parser referenced-productions)]
    #_(console/debug ::trash-filters {:filters filters :before parser :after result})
    result))

(defn- apply-heuristic
  "Apply accepted-heuristic to parser"
  [parser accepted-heuristic]
  (let [productions (heuristic/heuristic->productions accepted-heuristic)
        filters (heuristic/heuristic->filters accepted-heuristic)
        result (-> parser
                   (refactor.parser/add-productions productions)
                   (trash-filters filters)
                   (add-filters filters))]
    #_(console/debug ::apply-heuristic {:accepted-heuristic accepted-heuristic
                                        :productions productions
                                        :filters filters
                                        :result result})
    result))

(defn recompile-parser
  [{:keys [accepted-heuristics orig-parser] :as state} run-checks?]
  (let [result (parser/compile-parser
                 (reduce apply-heuristic
                         orig-parser
                         accepted-heuristics)
                 {:skip-checks (not run-checks?)})]
    #_(console/debug ::recompile-parser {:accepted-heuristics accepted-heuristics
                                         :orig-parser orig-parser
                                         :result result})
    (assoc state :parser result)))

;;------------------------------------------------------------------------------
;; populate-sample-cache
;;------------------------------------------------------------------------------

;; FIXME: if any stage fails to complete, do not continue on

(defn- free-sample
  [state sample-id]
  (let [{:keys [cyk]} (get-cache-entry state sample-id)]
    (when (some? cyk)
      (try
        (asm.parser/cpp-free cyk)
        (catch js/Error _
          (console/warn ::free-sample :failure))))
    state))

(defn- lex-sample [{:keys [lexer parse-dashboard] :as state} sample-id]
  (let [{:keys [string]} (parse-dashboard/get-sample parse-dashboard sample-id)
        tokens (lexer/lex lexer string)
        failed? (lexer/lex-error? (last tokens))]
    (when failed?
      (throw (ex-info "Testing"
                      {:causes #{:lex-failure}
                       :sample-id sample-id
                       :tokens tokens})))
    (-> state
        (set-cache-entry-attr sample-id :string string)
        (set-cache-entry-attr sample-id :tokens tokens))))

(defn- check-sample [{:keys [parser parse-dashboard] :as state} sample-id]
  (let [{:keys [tokens]} (get-cache-entry state sample-id)
        {:keys [cyk codec]} (inference/run-parser-unconstrained parser tokens)
        sample (parse-dashboard/get-sample parse-dashboard sample-id)
        check-result (inference/check-sample
                       {:parser parser
                        :tokens tokens
                        :parse {:cyk cyk
                                :codec codec}}
                       sample)
        pruned-result (map-values #(select-keys % [:type :top-down :bottom-up]) check-result)]
    (asm.parser/cpp-free cyk)
    (set-cache-entry-attr state sample-id :check pruned-result)))

(defn- get-negative-labels [{:keys [parse-dashboard] :as state} sample-id]
  (let [{:keys [tokens]} (get-cache-entry state sample-id)]
    (into []
          (map (partial parse-dashboard/sample-label->token-indexed-label tokens))
          (parse-dashboard/negative-labels-for parse-dashboard sample-id))))

(defn- parse-sample [{:keys [parser] :as state} sample-id]
  (let [{:keys [tokens]} (get-cache-entry state sample-id)
        negative-labels (get-negative-labels state sample-id)
        {:keys [cyk codec]} (inference/run-parser-constrained parser tokens negative-labels)]
    (console/debug ::parse-sample {:negative-labels negative-labels})
    (-> state
        (set-cache-entry-attr sample-id :cyk cyk)
        (set-cache-entry-attr sample-id :codec codec))))

(defn- refresh-sample [{:keys [parser] :as state} sample-id]
  ;; FIXME: may be a memory leak if exception thrown after solver-impl/parse-sample executes
  (-> state
      (free-sample sample-id)
      (lex-sample sample-id)
      (check-sample sample-id)
      (parse-sample sample-id)))

(defn reset-sample-cache [{:keys [sample-cache] :as state}]
  (reduce free-sample
          state
          (keys sample-cache))
  (assoc state :sample-cache (:sample-cache default-model)))

(defn populate-sample-cache [{:keys [parse-dashboard] :as state}]
  (reduce refresh-sample
          (reset-sample-cache state)
          (parse-dashboard/all-sample-ids parse-dashboard)))

;;------------------------------------------------------------------------------
;; compute-working-set
;;------------------------------------------------------------------------------

(defn- node-constraints [{:keys [parser parse-dashboard] :as state} sample-id]
  (let [{:keys [check tokens]} (get-cache-entry state sample-id)
        sample (parse-dashboard/get-sample parse-dashboard sample-id)
        passing-label-ids (set (for [[label-id {:keys [top-down]}] check
                                     :when top-down]
                                 label-id))
        sample-failing (update sample :labels
                               (fn [labels]
                                 (into []
                                       (remove #(passing-label-ids (:label-id %)))
                                       labels)))]
    {:failing (inference/sample->node-constraints sample-failing tokens)
     :all (inference/sample->node-constraints sample tokens)}))

(defn- compute-one-sample-working-set [state sample-id]
  (into []
        (map (partial vector sample-id))
        (get-in (node-constraints state sample-id) [:failing :positive])))

(defn compute-working-set [{:keys [sample-cache] :as state}]
  (assoc state :working-set
         (into []
               (mapcat (partial compute-one-sample-working-set state))
               (keys sample-cache))))

;;------------------------------------------------------------------------------
;; gen-constraint-states
;;------------------------------------------------------------------------------

(defn- gen-one-sample-constraint-states
  [{:keys [parser] :as state} sample-id]
  (let [{:keys [tokens cyk codec]} (get-cache-entry state sample-id)
        {:keys [failing all]} (node-constraints state sample-id)
        constraint-states
        (into []
              (map (partial inference/gen-one-constraint-state
                            codec
                            cyk
                            tokens
                            parser
                            all
                            sample-id))
              (:positive failing))]
    #_(console/debug ::gen-one-sample-constraint-states
                     {:sample-id sample-id
                      :num-constraints-generated (count constraint-states)
                      :constraint-states constraint-states
                      :failing failing
                      :all all})
    constraint-states))

(defn gen-constraint-states
  [{:keys [sample-cache] :as state}]
  (assoc state
         :unpartitioned-constraint-states
         (into []
               (mapcat (partial gen-one-sample-constraint-states state))
               (keys sample-cache))))

;;------------------------------------------------------------------------------
;; partition-constraint-states
;;------------------------------------------------------------------------------

(defn partition-constraint-states
  [{:keys [unpartitioned-constraint-states lexer parser] :as state}]
  (let [codec (asm.parser/gen-codec (:cnf parser))]
    (assoc state
           :partitioned-constraint-states
           (asm.inference/learn-partitions unpartitioned-constraint-states lexer codec))))

;;------------------------------------------------------------------------------
;; run-heuristics
;;------------------------------------------------------------------------------

(defn- get-path-string [state raw idx [sample-id ti-label]]
  (let [{:keys [tokens]} (get-cache-entry state sample-id)
        {:keys [string]} (parse-dashboard/get-sample (:parse-dashboard state) sample-id)
        token-indices (into []
                            (map #(get % idx))
                            raw)
        token-pairs (partition 2 1 token-indices)
        token-ranges (into []
                           (map (fn [[i j]] [i (- j i)]))
                           token-pairs)
        char-ranges (into []
                          (map (fn [[i l]] (lexer/token-range->char-range tokens i l)))
                          token-ranges)]
    (into []
          (map (fn [[start end]] (subs string start end)))
          char-ranges)))

(defn- add-path-strings [state {:keys [raw provenance] :as heuristic}]
  (let [path-strings (into []
                           (map-indexed (partial get-path-string state raw))
                           provenance)]
    #_(console/debug ::add-path-strings {:raw raw
                                         :provenance provenance
                                         :path-strings path-strings})
    (assoc heuristic :path-strings path-strings)))

(defn- run-heuristic-battery [{:keys [lexer parser] :as state} {:keys [provenance] :as constraint-state}]
  (when-not (inference/has-unit-solution? constraint-state)
    (let [result
          (heuristic/with-cpp-query
            (-> []
                (into (heuristic/expression constraint-state lexer parser))
                (into (heuristic/undelimited-list constraint-state))
                (into (heuristic/delimited-list constraint-state))
                (into (heuristic/enclosed-undelimited-list constraint-state lexer))
                (into (heuristic/enclosed-delimited-list constraint-state lexer))))]
      (console/debug ::run-heuristic-battery {:result result})
      (when (seq result)
        (into []
              (comp (map #(assoc % :provenance provenance))
                    (map (partial add-path-strings state)))
              result)))))

(defn- heuristic-already-seen? [state heuristic]
  (let [key-fn #(select-keys % [:provenance])
        seen (-> #{}
                 (into (map key-fn) (:accepted-heuristics state))
                 (into (map key-fn) (:rejected-heuristics state)))]
    (contains? seen (key-fn heuristic))))

(defn run-heuristics [{:keys [partitioned-constraint-states] :as state}]
  (assoc state
         :heuristics
         (into []
               (comp (mapcat (partial run-heuristic-battery state))
                     (keep identity)
                     (remove (partial heuristic-already-seen? state)))
               partitioned-constraint-states)))

(defn accept-heuristic [state heuristic]
  (console/debug ::accept-heuristic {:heuristic heuristic})
  (update state :accepted-heuristics (fnil conj []) heuristic))

(defn reject-heuristic [state heuristic]
  (console/debug ::reject-heuristic {:heuristic heuristic})
  (update state :rejected-heuristics (fnil conj []) heuristic))

;;------------------------------------------------------------------------------
;; solve-partitions
;;------------------------------------------------------------------------------

(defn- xform-solution
  "Convert a solution from asm.inference format to solver-impl candidate format"
  [{:keys [provenance path shortest-path] :as solution}]
  {:provenance provenance
   :candidates (into []
                     (comp (remove empty?)
                           (map (partial hash-map :path)))
                     [shortest-path path])})

(defn- production-compiles?
  "Return true iff adding production to parser results in a parser that
   compiles correctly"
  [parser production]
  (try
    (let [parser' (refactor.parser/add-productions parser [production])]
      (parser/compile-parser parser' {:skip-checks false})
      true)
    (catch js/Error e
      false)))

(defn- prune-candidate
  "Remove any path elements from the given candidate that would result in a
   parser compile failure. If resulting candidate would have empty path, then
   return nil"
  [parser lhs-nt {:keys [path] :as candidate}]
  (let [num-possibilities (apply * 1 (map count path))]
    (if (and (= 1 (count path))       ;; must be unit path
             (< num-possibilities 5)) ;; limit pruning to at most 5 possibilities
      (let [ps (into []
                     (comp (map #(vector lhs-nt (vec %)))
                           (filter (partial production-compiles? parser)))
                     (cartesian-product path))]
        (when (seq ps)
          (let [path' (->> ps
                           (map parser/rhs)
                           (map (partial into [] (map hash-set)))
                           (apply map set/union)
                           (vec))]
            (when-not (= path path')
              (console/warn ::prune-candidate
                            {:candidate candidate
                             :ps ps
                             :path' path'}))
            (assoc candidate :path path'))))
      ;; leave candidate untouched
      candidate)))

(defn- prune-solution
  "Remove candidates that would cause compile failure. If no candidates remain
   after pruning, return nil"
  [parser {:keys [provenance candidates] :as solution}]
  (let [lhs-nt (first (inference/extract-provenance-syms provenance))
        pruned-candidates
        (into []
              (comp (map (partial prune-candidate parser lhs-nt))
                    (remove nil?))
              candidates)]
    (when (seq pruned-candidates)
      (assoc solution :candidates pruned-candidates))))

(defn solve-partitions [{:keys [lexer parser unpartitioned-constraint-states accepted-heuristics] :as state}]
  (let [codec (asm.parser/gen-codec (:cnf parser))
        heuristic-provenance-elems (into #{}
                                         (mapcat :provenance)
                                         accepted-heuristics)
        heuristically-covered-provenances (into #{}
                                                (comp (map :provenance)
                                                      (filter #(contains? heuristic-provenance-elems (first %))))
                                                unpartitioned-constraint-states)
        constraint-states (into []
                                (remove #(contains? heuristically-covered-provenances (:provenance %)))
                                unpartitioned-constraint-states)
        solutions (asm.inference/learn-and-solve-partitions
                    constraint-states
                    lexer
                    codec)
        xformed-solutions (into []
                                (map xform-solution)
                                solutions)
        pruned-solutions (into []
                               (comp (map (partial prune-solution parser))
                                     (remove nil?))
                               xformed-solutions)
        ;; set of provenances that are no longer covered due to pruning
        missing-provenances (set/difference (set (map :provenance xformed-solutions))
                                            (set (map :provenance pruned-solutions)))
        ;; break into unpartitioned provenances
        missing-singleton-provenances (set (for [p missing-provenances
                                                 e p]
                                             [e]))
        ;; unpartitioned constraint states that need to be solved again
        missing-constraint-states (into []
                                        (filter #(contains? missing-singleton-provenances (:provenance %)))
                                        unpartitioned-constraint-states)
        missing-solutions (asm.inference/solve-constraints
                            missing-constraint-states
                            lexer
                            codec)
        xformed-missing-solutions (into []
                                        (map xform-solution)
                                        missing-solutions)
        pruned-missing-solutions (into []
                                       (comp (map (partial prune-solution parser))
                                             (remove nil?))
                                       xformed-missing-solutions)]
    (when (seq missing-provenances)
      (console/warn ::solve-partitions
                    (sorted-map :all-provenances (set (map :provenance solutions))
                                :missing-provenances missing-provenances
                                :heuristically-covered-provenances heuristically-covered-provenances)))
    (assoc state :solutions (into pruned-solutions pruned-missing-solutions))))

;;------------------------------------------------------------------------------
;; recompile-multiparser
;;------------------------------------------------------------------------------

(defn- fresh-nt [n]
  [(keyword (str "-" n)) (inc n)])

(defn- path-node->productions
  [node n]
  (if (> (count node) 1)
    (let [[nt n] (fresh-nt n)]
      [nt
       (into [] (map #(vector nt [%])) node)
       n])
    [(first node) [] n]))

(defn- path->production-body
  [path n]
  (reduce
    (fn [[body productions n] node]
      (let [[nt sub-productions n] (path-node->productions node n)]
        [(conj body nt)
         (into productions sub-productions)
         n]))
    [[] [] n]
    path))

(defn- candidates->productions
  [lhs-nt candidates n]
  (reduce
    (fn [[acc n] {:keys [path]}]
      (let [[body sub-productions n] (path->production-body path n)
            head-production [lhs-nt body]]
        [(-> acc
             (conj head-production)
             (into sub-productions))
         n]))
    [[] n]
    candidates))

(defn- solutions->multigrammar-productions
  [solutions]
  (first
    (reduce
      (fn [[acc n] {:keys [provenance candidates]}]
        (let [lhs-nt (first (inference/extract-provenance-syms provenance))
              [productions n] (candidates->productions lhs-nt candidates n)]
          [(into acc productions) n]))
      [[] 0]
      solutions)))

(defn recompile-multiparser [{:keys [parser solutions] :as state}]
  (let [productions (solutions->multigrammar-productions solutions)]
    #_(console/debug ::recompile-multiparser {:solutions solutions :productions productions})
    (try
      (let [parser (-> parser
                       (refactor.parser/add-productions productions)
                       (parser/compile-parser))]
        (assoc state :parser parser))
      (catch js/Error e
        (console/error ::recompile-multiparser :compile-failure {:error (ex-data e)})
        state))))

;;------------------------------------------------------------------------------
;; recompute-forests
;;------------------------------------------------------------------------------

(defn- recompute-forest [state sample-id]
  (-> state
      (free-sample sample-id)
      (parse-sample sample-id)))

(defn recompute-forests [{:keys [parse-dashboard] :as state}]
  (reduce recompute-forest
          state
          (parse-dashboard/all-sample-ids parse-dashboard)))
