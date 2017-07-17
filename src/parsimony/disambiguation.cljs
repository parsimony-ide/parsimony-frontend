(ns parsimony.disambiguation
  (:require [clojure.set :as set]
            [parsimony.inference :as infer]
            [parsimony.models.parse-dashboard :as parse-dashboard]
            [parsimony.lexer :as lexer]
            [parsimony.parser :as parser]
            [parsimony.refactor.parser :as refactor.parser]
            [parsimony.util
             :refer [cartesian-product newline-pprint-str pprint-str llast]
             :refer-macros [inspect-pp with-inspection]]
            [schema.core :as s :include-macros true]
            [parsimony.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def prod-schema [(s/one s/Keyword "lhs") (s/one [s/Keyword] "rhs")])

(def candidate-schema (s/maybe
                        {(s/optional-key :prio) [{:high prod-schema :low prod-schema}]
                         (s/optional-key :lass) [[prod-schema]]
                         (s/optional-key :rass) [[prod-schema]]
                         (s/optional-key :pref) [prod-schema]}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- elems-with-head
  "Return all forest elements with the given head (parent) node"
  [forest head]
  (into #{}
        (filter #(= (first %) head))
        forest))

;; NOTES:
;; - if an ambig-elem contains a negated label on its RHS, then it is inferior
;; - if an ambig-elem contains a positive label on its RHS, then it is superior
;; - if two ambig-elems have the same LHS, then a filter must be computed for the pair

(defn- workup [forest head body]
  (let [nt-nodes (into []
                       (remove #(parser/terminal? (first %)))
                       body)
        child-elems (into []
                          (map (partial elems-with-head forest))
                          nt-nodes)
        child-prods (vec (for [elems child-elems]
                           (into #{}
                                 (map parser/underlying-production)
                                 elems)))]
    {:head head
     :body body
     :prod (parser/underlying-production [head body])
     :child-elems child-elems
     :child-prods child-prods}))

(defn- ambiguous-workup? [{:keys [child-elems] :as workup}]
  (some #(> (count %) 1) child-elems))

(defn- rhs-nonterminals [[_ rhs]]
  (remove parser/terminal? rhs))

(defn- possibly-associative [prod]
  (and (> (count (second prod)) 1) ;; not a unit production
       (> (count (rhs-nonterminals prod)) 0))) ;; at least 1 nonterminal

;; TODO: assoc and prio and prefer must have same head

(defn- diagnose-associativity [{:keys [prod child-prods] :as winner-workup} loser-workups dir]
  #_(inspect-pp :diagnose-associativity
                [dir {:winner-workup winner-workup
                      :loser-workups loser-workups}])
  (when (possibly-associative prod)
    (let [primary-prod (case dir
                         :left (ffirst child-prods)
                         :right (llast child-prods)
                         ;; default
                         nil)
          rest-prods (case dir
                       :left (apply concat (next child-prods))
                       :right (apply concat (drop-last child-prods))
                       ;; default
                       nil)]
      (when (possibly-associative primary-prod)
        (when-not (contains? (set rest-prods) primary-prod)
          #_(inspect-pp :diagnose-associativity-result
                        [dir {:prod prod
                              :primary-prod primary-prod
                              :rest-prods rest-prods}])
          (-> #{prod primary-prod}
              (vec)
              (vector)))))))

(defn- diagnose-priority [{:keys [prod child-prods] :as winner-workup} loser-workups]
  ;; if winner has A > B
  ;; and losers have B > A
  ;; then guess A > B
  #_(inspect-pp :diagnose-priority
                {:winner-workup winner-workup
                 :loser-workups loser-workups})
  (when (possibly-associative prod)
    (let [child-prods (into #{}
                            (comp (mapcat identity)
                                  (filter possibly-associative))
                            child-prods)]
      (when (and (seq child-prods)
                 (not (contains? child-prods prod)))
        (into []
              (for [c child-prods]
                {:high c :low prod}))))))

(defn- diagnose-prefer [{:keys [prod] :as winner-workup} loser-workups]
  ;; if winner has different production than all losers
  #_(inspect-pp :diagnose-prefer
                {:winner-workup winner-workup
                 :loser-workups loser-workups})
  (let [loser-prods (into #{}
                          (map :prod)
                          loser-workups)]
    (when-not (contains? loser-prods prod)
      [prod])))

(defn- diagnose-head [parser forest allow-ambiguous-child? [head {:keys [winner losers]}]]
  ;; - if winner and losers all have same underlying production, then associativity
  ;; - if winner and losers have different underlying production, then either prefer or priority
  (let [winner-workup (workup forest head winner)
        loser-workups (into []
                            (map (partial workup forest head))
                            losers)]
    (cond
      ;; ambiguous descendant
      (and (not allow-ambiguous-child?)
           (or (ambiguous-workup? winner-workup)
               (some ambiguous-workup? loser-workups)))
      (do (console/debug :diagnose-head :workup-is-ambiguous :skipping
                         {:head head
                          :winner-workup winner-workup
                          :loser-workups loser-workups})
          nil)

      ;; no winner
      (not (some? winner))
      (do (console/debug :diagnose-head :no-winner :skipping head)
          nil)

      :else
      (let [lass (diagnose-associativity winner-workup loser-workups :left)
            rass (diagnose-associativity winner-workup loser-workups :right)
            prio (diagnose-priority winner-workup loser-workups)
            pref (diagnose-prefer winner-workup loser-workups)
            result (->> [(when lass {:lass lass})
                         (when rass {:rass rass})
                         (when prio {:prio prio})
                         ;; prefer only if associativity doesn't work
                         (when (and pref (not (or lass rass))) {:pref pref})]
                        (keep identity)
                        (vec))]
        (if (seq result)
          (do (console/debug :diagnose-head :result (pprint-str [head result]))
              result)
          (do (console/debug :diagnose-head :no-result head)
              nil))))))

(defn- apply-lass [parser lass]
  (reduce #(refactor.parser/add-block-associativity %1 %2 :left)
          parser
          lass))

(defn- apply-rass [parser rass]
  (reduce #(refactor.parser/add-block-associativity %1 %2 :right)
          parser
          rass))

(defn- apply-prio [parser prio]
  (reduce #(refactor.parser/add-priority %1 (:low %2) (:high %2))
          parser
          prio))

(defn- apply-pref [parser pref]
  (reduce #(refactor.parser/add-attributes %1 %2 #{:prefer})
          parser
          pref))

(defn apply-candidate [parser {:keys [lass rass prio pref] :as candidate}]
  (-> parser
      (apply-lass lass)
      (apply-rass rass)
      (apply-prio prio)
      (apply-pref pref)))

(defn- incompatible?
  "Return true if the parser and candidate are incompatible"
  [parser candidate]
  (try
    (let [parser' (-> parser
                      (apply-candidate candidate)
                      (parser/compile-parser {:skip-checks false}))]
      #_(console/debug :parser (pprint-str parser'))
      false)
    (catch js/Error _
      (console/warn :incompatible? (newline-pprint-str candidate))
      true)))

(defn- simplify-candidate [parser candidate]
  (letfn [(parser->candidate [{{:keys [priorities associativities attributes]} :orig}]
            (let [lass (into []
                             (comp (filter #(= :left (first %)))
                                   (map second))
                             associativities)
                  rass (into []
                             (comp (filter #(= :right (first %)))
                                   (map second))
                             associativities)
                  attr-lefts (into []
                                   (comp (map (fn [[p attrs]]
                                                (when (:left attrs)
                                                  [p])))
                                         (keep identity))
                                   attributes)
                  attr-rights (into []
                                    (comp (map (fn [[p attrs]]
                                                 (when (:right attrs)
                                                   [p])))
                                          (keep identity))
                                    attributes)
                  pref (into []
                             (comp (map (fn [[p attrs]]
                                          (when (:prefer attrs)
                                            p)))
                                   (keep identity))
                             attributes)
                  lass (into lass attr-lefts)
                  rass (into rass attr-rights)]
              (merge (when (seq priorities)
                       {:prio priorities})
                     (when (seq lass)
                       {:lass lass})
                     (when (seq rass)
                       {:rass rass})
                     (when (seq pref)
                       {:pref pref}))))]
    (let [candidate' (-> parser
                         (refactor.parser/remove-all-filters)
                         (apply-candidate candidate)
                         (refactor.parser/simplify-filters)
                         (parser->candidate))]
      #_(console/debug :simplify-candidate (pprint-str {:candidate candidate
                                                        :candidate' candidate'}))
      candidate')))

(defn- compose-diagnoses [parser diagnoses]
  ;; test cartesian product of diagnoses
  ;; filter out incompatible diagnoses
  ;; remainder are possible solutions
  (letfn [(compose-product [xs]
            (reduce
              (fn [acc {:keys [lass rass prio pref]}]
                (let [lass (into (vec (:lass acc))
                                 lass)
                      rass (into (vec (:rass acc))
                                 rass)
                      prio (into (vec (:prio acc))
                                 prio)
                      pref (into (vec (:pref acc))
                                 pref)
                      res (merge {}
                                 (when (seq lass) {:lass lass})
                                 (when (seq rass) {:rass rass})
                                 (when (seq prio) {:prio prio})
                                 (when (seq pref) {:pref pref}))]
                  (when (seq res)
                    res)))
              xs))]
    (let [candidates (->> diagnoses
                          (cartesian-product)
                          (map compose-product)
                          (keep identity)
                          (map (partial simplify-candidate parser))
                          (distinct))
          compatible (into []
                           (remove (partial incompatible? parser))
                           candidates)]
      (console/debug ::compose-diagnoses
                     {:diagnoses diagnoses
                      :candidates candidates
                      :compatible compatible})
      compatible)))

(defn- elem-descendants [forest [lhs rhs]]
  ;; XXX: this is inefficient because parser/descendant-nodes recomputes dag on each call
  (-> (into #{}
            (mapcat (partial parser/descendant-nodes forest))
            rhs)
      (into rhs)))

(defn- tag-one-head [forest positive-ti-labels negative-ti-labels [_ elems]]
  (let [shared-descendants (->> elems
                                (map (partial elem-descendants forest))
                                (apply set/intersection))
        positive-ti-labels (set/difference positive-ti-labels shared-descendants)
        negative-ti-labels (set/difference negative-ti-labels shared-descendants)
        inferior-elems (into #{}
                             (filter #(seq (set/intersection negative-ti-labels (set (second %)))))
                             elems)
        superior-elems (into #{}
                             (comp
                               (filter #(seq (set/intersection positive-ti-labels (set (second %)))))
                               (remove inferior-elems))
                             elems)
        unmarked-elems (into #{}
                             (comp (remove inferior-elems)
                                   (remove superior-elems))
                             elems)
        tagged-inferior-elems (into #{}
                                    (map (fn [[lhs rhs]]
                                           [lhs {:inferior rhs}]))
                                    inferior-elems)
        tagged-superior-elems (into #{}
                                    (map (fn [[lhs rhs]]
                                           [lhs {:superior rhs}]))
                                    superior-elems)
        tagged-unmarked-elems (into #{}
                                    (map (fn [[lhs rhs]]
                                           [lhs {:unmarked rhs}]))
                                    unmarked-elems)
        result (group-by first (set/union tagged-inferior-elems tagged-superior-elems tagged-unmarked-elems))]
    #_(console/debug ::tag-one-head {:elems elems
                                     :shared-descendants shared-descendants
                                     :positive-ti-labels positive-ti-labels
                                     :negative-ti-labels negative-ti-labels
                                     :tagged-inferior-elems tagged-inferior-elems
                                     :tagged-superior-elems tagged-superior-elems
                                     :tagged-unmarked-elems tagged-unmarked-elems
                                     :result result})
    result))

(defn- tag-heads [forest tokens string {:keys [productions] :as parser} sample]
  (let [roots (parser/root-nodes forest)]
    (when (> (count roots) 1)
      (throw (ex-info "Multiple roots found for forest" {:causes #{:multiple-roots}
                                                         :roots roots
                                                         :forest forest})))
    (let [[nt i l] (first roots)
          [char-from char-to] (lexer/token-range->char-range tokens i l)
          root-label {:nt nt
                      :char-from char-from
                      :char-to char-to
                      :type :positive}
          {:keys [positive-ti-labels
                  negative-ti-labels]}
          (infer/-td-check forest tokens parser sample root-label)
          ambig-nodes (parser/ambiguous-nodes forest)
          ambig-elems (into []
                            (filter #(contains? ambig-nodes (first %)))
                            forest)
          result (->> ambig-elems
                      (group-by first)
                      (map (partial tag-one-head
                                    forest
                                    (set positive-ti-labels)
                                    (set negative-ti-labels)))
                      (apply merge))]
      #_(console/debug ::tag-heads {:root-label root-label
                                    :ambig-nodes ambig-nodes
                                    :ambig-elems ambig-elems
                                    :result result})
      result)))

(defn- compute-winner-per-head [parser forest [head tagged-elems]]
  (let [bodies (map second tagged-elems)
        inferior (->> bodies
                      (filter :inferior)
                      (mapcat vals)
                      (set))
        unmarked  (->> bodies
                       (filter :unmarked)
                       (mapcat vals)
                       (set))
        superior  (->> bodies
                       (filter :superior)
                       (mapcat vals)
                       (set))
        losers (if (seq superior)
                 (set/union inferior unmarked)
                 inferior)
        winner (cond
                 ;; if only one superior, then it's the winner
                 (= (count superior) 1)
                 (first superior)

                 ;; if more than one superior, then conflict
                 (> (count superior) 1)
                 (do
                   (console/error ::compute-winner-per-head
                                    :conflict
                                    {:head head
                                     :tagged-elems tagged-elems
                                     :superior superior
                                     :inferior inferior
                                     :unmarked unmarked
                                     :losers losers})
                   ;; ::conflict
                   nil)

                 ;; if no superior:
                 ;;   - if 1+ inferior and one unmarked, then unmarked is the winner
                 (and (> (count inferior) 0)
                      (= (count unmarked) 1))
                 (first unmarked)

                 ;;   - if no inferior, then no winner
                 (zero? (count inferior))
                 nil

                 ;; default
                 :else nil)
        result (vector head {:winner winner
                             :losers losers})]
    (inspect-pp :compute-winner-per-head result)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Idea:
;; - After one iteration of diagnose-forest, we have a diagnosis that should
;; resolve _some_ of the ambiguities present.
;; - Strip all losers from the forest, then re-run diagnose-forest with a
;; _hopefully_ smaller number of ambiguities
;; - Continue iterating until no new losers are found

(def MAX-ITERATIONS 5)

(defn diagnose-forest [forest tokens string parser sample]
  (letfn [(head-span [[[_ _ l] _]] l)
          (iterate-once-a [acc forest]
            ;; Candidate Implementation A:
            ;; - Diagnose multiple heads simultaneously.
            ;; - Do not diagnose when a descendant is ambiguous.
            (let [individual-diagnoses
                  (->> (tag-heads forest tokens string parser sample)
                       (map (partial compute-winner-per-head parser forest))
                       (sort-by head-span) ;; sort by increasing size so lower levels are resolved first
                       (map (partial diagnose-head parser forest false)))
                  group-diagnoses
                  (->> (conj individual-diagnoses acc)
                       (keep identity) ;; only keep non-nil
                       (compose-diagnoses parser))]
              (console/debug ::iterate-once-a
                             {:acc acc
                              :individual-diagnoses individual-diagnoses
                              :group-diagnoses group-diagnoses})
              group-diagnoses))

          (iterate-once-b [acc forest]
            ;; Candidate Implementation B:
            ;; - Diagnose only one lowest level head with a winner.
            ;; - Allow diagnosis even if a descendant is ambiguous.
            (let [processed-heads (->> (tag-heads forest tokens string parser sample)
                                       (map (partial compute-winner-per-head parser forest)))]
              (if-let [my-head (->> processed-heads
                                    (filter (comp seq :winner second))
                                    (sort-by head-span) ;; sort by increasing size so lower levels are resolved first
                                    (first))]
                (if-let [diagnosis (diagnose-head parser forest true my-head)]
                  (let [composed (->> [diagnosis acc]
                                      (keep identity)
                                      (compose-diagnoses parser))]
                    (console/debug ::iterate-once-b :diagnosis-found
                                   {:diagnosis diagnosis
                                    :acc acc
                                    :composed composed})
                    composed)
                  (do (console/debug ::iterate-once-b :no-diagnosis-found {:my-head my-head
                                                                           :acc acc})
                      acc))
                (do (console/debug ::iterate-once-b :no-viable-head-found {:processed-heads processed-heads :acc acc})
                    acc))))]
    (loop [diagnoses-so-far nil forest forest n 0]
      (console/debug :diagnose-forest :iteration n)
      (if (< n MAX-ITERATIONS)
        (let [new-diagnoses (iterate-once-b diagnoses-so-far forest)]
          (if (not= (set diagnoses-so-far) (set new-diagnoses))
            (let [parser' (parser/compile-parser (apply-candidate parser (first new-diagnoses)))
                  forest' (parser/disambiguate forest parser')]
              ;; TODO: check that parser actually compiles successfully here
              (console/debug :diagnose-forest :recur (newline-pprint-str {:forest forest'}))
              (recur new-diagnoses forest' (inc n)))
            (do (console/debug :diagnose-forest :base-case-equals)
                diagnoses-so-far)))
        (do (console/debug :diagnose-forest :base-case-limit)
            diagnoses-so-far)))))
