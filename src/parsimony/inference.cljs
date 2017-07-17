(ns parsimony.inference
  "Parser inference algorithms"
  (:require [cljs.pprint :refer [pprint]]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.zip :as zip]
            [parsimony.util :refer [pprint-str matches-schema? vec-remove cartesian-product] :refer-macros [inspect inspect-pp noinspect]]
            [parsimony.parser :as parser]
            [parsimony.asm.parser :as asm.parser]
            [parsimony.lexer :as lexer]
            [parsimony.dag :as dag]
            [parsimony.union-find :as uf]
            [parsimony.models.parse-dashboard :refer [sample-label->token-indexed-label nested-sample-labels]]
            [schema.core :as s :include-macros true]
            [parsimony.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run-parser-unconstrained [parser tokens]
  (let [{:keys [cyk codec exec-time]} (asm.parser/cpp-init-cyk (into [] (map :label) tokens) parser)
        cyk-time (asm.parser/cpp-run-cyk cyk)
        color-time (asm.parser/cpp-run-color cyk)]
    #_(console/debug ::run-parser-unconstrained :cyk-runtime {:init exec-time :cyk cyk-time :color color-time})
    {:cyk cyk :codec codec}))

(defn- apply-negative-labels [codec cyk negative-labels]
  (doseq [[nt i l] negative-labels]
    (if (= 1 l)
      ;; If l = 1, then cpp-init-cyk may have already set it, so unset it to undo the effect.
      (do (console/debug ::apply-negative-labels :unset [nt i l])
          (asm.parser/unset-cyk codec cyk nt i l))
      ;; If l > 1, then cpp-run-cyk will try to set it when filling out the CYK table. Set it to true as a sentinel for
      ;; the CPP implementation to pin the table entry to false.
      (do (console/debug ::apply-negative-labels :set [nt i l])
          (asm.parser/set-cyk codec cyk nt i l)))))

(defn run-parser-constrained
  [parser tokens negative-labels]
  (let [{:keys [cyk codec exec-time]} (asm.parser/cpp-init-cyk (into [] (map :label) tokens) parser)]
    (apply-negative-labels codec cyk negative-labels)
    (let [cyk-time (asm.parser/cpp-run-cyk cyk)]
      ;; don't run color since we don't need it
      #_(console/debug ::run-parser-constrained :cyk-runtime {:init exec-time :cyk cyk-time})
      {:cyk cyk :codec codec})))

(defn sample->node-constraints
  "Convert a sample into a set of node constraints for use in constraint solving"
  [{:keys [labels] :as sample} tokens]
  (let [pos (into #{}
                  (comp (filter #(= :positive (:type %)))
                        (map (partial sample-label->token-indexed-label tokens)))
                  labels)
        neg (into #{}
                  (comp (filter #(= :negative (:type %)))
                        (map (partial sample-label->token-indexed-label tokens)))
                  labels)]
    {:positive pos
     :negative neg}))

(defn ti-identical-extent?
  "Return true iff the first token-indexed label has the same extent as the second"
  [[_ i l] [_ i' l']]
  (and (= i i') (= l l')))

(defn ti-strictly-contains?
  "Return true iff the first token-indexed label strictly contains the second"
  [[_ i l] [_ i' l']]
  (let [j (+ i l)
        j' (+ i' l')]
    (or (and (<= i i') (> j j'))
        (and (< i i') (>= j j')))))

(defn ti-contains?
  "Return true iff first arg's extent contains or is identical to second arg's extent"
  [[_ i l] [_ j k]]
  (and (<= i j)
       (>= (+ i l) (+ j k))))

(defn unique-extents
  "Return a map from [i l] to nodes with the same extent"
  [nodes]
  (let [f (fn [m [_ i l :as node]]
            (update m [i l] (fnil conj #{}) node))]
    (reduce f {} nodes)))

(defn get-cyk-syms
  "Return all symbols that match in the CYK table at the given extent"
  [codec cyk tokens parser i l]
  (into #{}
        (comp
          (remove parser/multinode-nonterminal?)
          (filter #(asm.parser/get-cyk codec cyk % i l)))
        (parser/all-syms (:productions parser))))

(defn compute-unit-partial-order
  "Return partial order on nonterminals related by unit-equivalence. N > M iff
   there exists a chain of production rules N -> ... -> M. If there exists a cycle,
   then the partial order returned is empty."
  [parser]
  (let [f (fn [g p]
            (if (and (parser/unit-rule? p)
                     (not (parser/terminal? (first (parser/rhs p)))))
              (dag/add-edge g (parser/lhs p) (first (parser/rhs p)))
              g))
        g (reduce f (dag/new-dag) (:productions parser))]
    (if (dag/cyclic? g)
      (dag/new-dag)
      (dag/transitive-closure g))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn subtree-pos-match?
  "Returns true iff the given parse forest contains a node matching the given label"
  [forest label]
  (contains? (parser/all-nodes forest) label))

(defn -td-check [forest tokens parser sample root-label]
  (let [nested-labels (nested-sample-labels sample root-label)
        nested-positive-labels (filter #(= :positive (:type %)) nested-labels)
        nested-negative-labels (filter #(= :negative (:type %)) nested-labels)
        nested-positive-ti-labels (map (partial sample-label->token-indexed-label tokens) nested-positive-labels)
        nested-negative-ti-labels (map (partial sample-label->token-indexed-label tokens) nested-negative-labels)
        positive-failures (seq (filter #(not (subtree-pos-match? forest %)) nested-positive-ti-labels))
        negative-failures (seq (filter #(subtree-pos-match? forest %) nested-negative-ti-labels))
        passing? (not (or positive-failures
                          negative-failures))]
    #_(inspect-pp
      (sorted-map
        :sample sample
        :root-label root-label
        :nested-positive-ti-labels nested-positive-ti-labels
        :nested-negative-ti-labels nested-negative-ti-labels
        :forest forest
        :underlying-productions (parser/underlying-productions forest)
        :positive-failures positive-failures
        :negative-failures negative-failures
        :passing? (not (or (seq positive-failures)
                           (seq negative-failures)))))
    (merge {:passing? passing?
            :positive-ti-labels nested-positive-ti-labels
            :negative-ti-labels nested-negative-ti-labels}
           (when-not passing?
             {:positive-failures positive-failures
              :negative-failures negative-failures}))))

(defn td-check [codec cyk tokens parser sample root-label]
  (let [token-vec (into [] (map :label) tokens)
        [nt i l :as root-ti-label] (sample-label->token-indexed-label tokens root-label)
        forest (parser/disambiguate
                 (asm.parser/reconstruct codec cyk token-vec parser nt i l)
                 parser)]
    (-> (-td-check forest tokens parser sample root-label)
        (merge {:forest forest})
        (select-keys [:passing? :positive-failures :negative-failures]))))

(defn bu-check [codec cyk tokens sample-label]
  (let [[nt i l :as label] (sample-label->token-indexed-label tokens sample-label)
        res (boolean (asm.parser/get-cyk codec cyk nt i l))]
    {:passing?
     (case (:type sample-label)
       :positive res
       :negative (not res)
       ;; default
       (do (console/error ::bu-check :unrecognized-sample-type {:sample-label sample-label})
           false))}))

(defn check-sample
  "Return a map from label id to {:top-down :bottom-up} map for a single sample"
  [{:keys [parser tokens parse]} {:keys [labels] :as sample}]
  (let [{:keys [codec cyk]} parse]
    (into {}
          (for [{:keys [label-id type] :as label} labels]
            (let [bu (bu-check codec cyk tokens label)]
              (if (or (= :negative type)
                      (not (:passing? bu)))
                ;; we don't perform top-down checks on negative labels, and a positive label that fails the
                ;; bottom-up check can't possibly pass the top-down check
                [label-id {:type type
                           :bottom-up (:passing? bu)
                           :top-down false}]
                ;; if the bottom-up check passes, then try the top-down check
                (let [td (td-check codec cyk tokens parser sample label)]
                  [label-id (merge {:type type
                                    :top-down (:passing? td)
                                    :bottom-up (:passing? bu)}
                                   (select-keys td [:forest :positive-failures :negative-failures]))])))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraint Solving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- gen-one-constraint-state
  "Return one constraint state for the positive constraint root"
  [codec cyk tokens parser constraints sample-id [nt j k :as root]]
  (let [constrained-extents (dissoc (unique-extents (:positive constraints)) [j k])
        tokens (vec tokens)
        lexer-syms (into #{}
                         (comp
                           (map :label)
                           (map parser/->terminal))
                         tokens)
        parser-syms (into #{}
                          (remove parser/multinode-nonterminal?)
                          (parser/all-syms (:productions parser)))
        all-syms (vec (set/union lexer-syms parser-syms))
        imax (+ j k)
        lmax k
        add-edge (fn [g i l v]
                   (let [from [i]
                         to [(+ i l)]]
                     (-> g
                         (dag/add-edge from to)
                         (dag/update-edge-attr from to :syms (fnil conj #{}) v))))
        table
        (loop [i j
               l lmax
               syms all-syms
               curr-token (get tokens i)
               g (dag/new-dag)]
          #_(inspect-pp [i l syms curr-token g])
          (cond
            ;; all extents covered
            (>= i imax)
            g

            ;; all extents [i _] covered, increment i
            (<= l 0)
            (recur (inc i)
                   (- lmax (- i j) 1)
                   all-syms
                   (get tokens (inc i))
                   g)

            ;; all vocabulary symbols covered for extent [i l], decrement l
            (empty? syms)
            (recur i
                   (dec l)
                   all-syms
                   curr-token
                   g)

            ;; this extent is mentioned in a constraint, add to table and skip
            ;; its sub-extents
            (contains? constrained-extents [i l])
            (recur (inc i)
                   (- lmax (- i j) 1)
                   all-syms
                   (get tokens (inc i))
                   (reduce #(add-edge %1 i l %2) g
                           ;; we also add all other syms corresponding to the
                           ;; extent [i l], due the possibility of unit-rules
                           (into (get-cyk-syms codec cyk tokens parser i l)
                                 (map first (get constrained-extents [i l])))))

            ;; this is a singleton extent [i 1], so add terminal entry if the
            ;; current symbol matches the current terminal
            (and (= l 1)
                 (parser/terminal? (peek syms))
                 (= (:label curr-token) (parser/->nonterminal (peek syms))))
            (recur i
                   l
                   (pop syms)
                   curr-token
                   (add-edge g i l (peek syms)))

            ;; none of the previous conditions hold, so just add the CYK entry
            ;; at extent [i l] unless it is the root constraint (since we don't
            ;; want trivial solutions of the form E = E
            (and (not (and (= (peek syms) nt)
                           (= i j)
                           (= l k)))
                 (asm.parser/get-cyk codec cyk (peek syms) i l))
            (recur i
                   l
                   (pop syms)
                   curr-token
                   (add-edge g i l (peek syms)))

            ;; no CYK entry match for the current symbol, move to next symbol
            :else
            (recur i
                   l
                   (pop syms)
                   curr-token
                   g)))]
    {:table table
     :provenance [[sample-id root]]}))

(defn- intersect-tables
  "Naive implementation of constraint table intersection: enumeration of all pairs of nodes"
  [g1 g2]
  (let [nodes (for [n1 (dag/nodes g1)
                    n2 (dag/nodes g2)]
                [n1 n2])
        edges (for [[n1 n2 :as n] nodes
                    [n1' n2' :as n'] nodes
                    :when (and (not (= n n'))
                               (dag/has-edge? g1 n1 n1')
                               (dag/has-edge? g2 n2 n2'))
                    :let [g1-syms (dag/edge-attr g1 n1 n1' :syms)
                          g2-syms (dag/edge-attr g2 n2 n2' :syms)
                          syms (set/intersection g1-syms g2-syms)]
                    :when (seq syms)]
                [;; n
                 (apply into n)
                 ;; n'
                 (apply into n')
                 syms])]
    (reduce
      (fn [g [n n' syms]]
        (-> g
            (dag/add-edge n n')
            (dag/add-edge-attr n n' :syms syms)))
      (dag/new-dag)
      edges)))

(defn- fast-intersect-tables
  "Better implementation of constraint table intersection: pairwise DAG traversal"
  [g1 g2]
  (loop [node-pairs (into #{} (for [n1 (dag/roots g1)
                                    n2 (dag/roots g2)]
                                [n1 n2]))
         g3 (dag/new-dag)]
    (if-let [[n1 n2 :as np] (first node-pairs)]
      (let [n1s (dag/successors g1 n1)
            n2s (dag/successors g2 n2)
            edges (for [n1' n1s
                        n2' n2s
                        :let [g1-syms (dag/edge-attr g1 n1 n1' :syms)
                              g2-syms (dag/edge-attr g2 n2 n2' :syms)
                              syms (set/intersection g1-syms g2-syms)]
                        :when (seq syms)]
                    [[n1 n2] [n1' n2'] syms])
            g3 (reduce
                 (fn [g [n n' syms]]
                   (let [from (apply into n)
                         to (apply into n')]
                     (-> g
                         (dag/add-edge from to)
                         (dag/add-edge-attr from to :syms syms))))
                 g3
                 edges)]
        (recur (into (disj node-pairs np) (map second edges))
               g3))
      g3)))

(defn- start-node [{:keys [provenance] :as constraint-state}]
  (vec (for [[_ [_ i _]] provenance]
         i)))

(defn- end-node [{:keys [provenance] :as constraint-state}]
  (vec (for [[_ [_ i l]] provenance]
         (+ i l))))

(defn- remove-non-solution-nodes
  "Given a constraint state, return a new constraint state stripped of all
   nodes that contribute to no solution path"
  [{:keys [table] :as constraint-state}]
  (let [path-nodes (dag/all-path-nodes table
                                       (start-node constraint-state)
                                       (end-node constraint-state))
        non-path-nodes (set/difference (dag/nodes table) path-nodes)]
    #_(console/debug ::remove-non-solution-nodes {:non-path-nodes non-path-nodes})
    (assoc constraint-state :table (dag/remove-nodes* table non-path-nodes))))

(defn- intersect-constraint-states
  [c1 c2]
  (remove-non-solution-nodes
   {:provenance (into (:provenance c1) (:provenance c2))
    :table (fast-intersect-tables (:table c1) (:table c2))}))

(defn extract-provenance-syms [provenance]
  (into #{}
        (map (fn [[_ [sym]]] sym))
        provenance))

(defn same-lhs?
  [c1 c2]
  (= (extract-provenance-syms (:provenance c1))
     (extract-provenance-syms (:provenance c2))))

(defn compatible?
  "Return true iff c1 and c2 have non-empty intersection"
  [c1 c2]
  (when (same-lhs? c1 c2)
    (let [{:keys [table]} (intersect-constraint-states c1 c2)]
      (not (dag/empty? table)))))

(defn c1-score
  [cs i j]
  ;; XXX: can improve efficiency by caching intersections
  (let [n (count cs)
        ci (get cs i)
        cj (get cs j)
        score-one (fn [ci cj ck]
                    (if (= (compatible? ci ck)
                           (compatible? cj ck)
                           (compatible? (intersect-constraint-states ci cj) ck))
                      1
                      0))]
    (reduce + (for [k (range 0 n)
                    :when (and (not= i k)
                               (not= j k))]
                (score-one ci cj (get cs k))))))

(defn score-partitions
  "Assign a score to each pair of constraint states. The highest scoring
   compatible pair is intersected in each round."
  [cs]
  (let [n (count cs)
        candidates
        (for [i (range n)
              j (range (inc i) n)
              :when (compatible? (get cs i) (get cs j))]
          [i j])
        score (fn [[i j]]
                [[i j] (c1-score cs i j)])]
    (into {} (map score) candidates)))

(defn learn-partitions-iterate
  "One iteration of the learn-partitions algorithm"
  [cs]
  (let [scores (score-partitions cs)
        max-score (apply max (vals scores))
        winners (for [[k v] scores :when (= max-score v)]
                  k)
        statistics {:count (count cs)
                    :partitions (into (sorted-map) (map vector (range) (map :provenance cs)))
                    :scores scores
                    :max-score max-score
                    :winners winners}]
    (console/debug ::learn-partitions-iterate {:statistics statistics})
    (when-let [[i j] (first winners)]
      {:statistics statistics
       :result (-> cs
                   (vec-remove j)
                   (vec-remove i)
                   (conj (intersect-constraint-states (get cs i) (get cs j))))})))

(defn learn-partitions
  "cs is a vector of constraint states. Either return a smaller vector of
   constraint states in which some have been intersected, or return cs
   unchanged if no constraint states are compatible."
  [cs]
  (loop [cs (vec cs)]
    (if-let [{:keys [result]} (learn-partitions-iterate cs)]
      (recur result)
      cs)))

(defn- decode-solution-path
  [{:keys [table] :as constraint-state} path]
  {:raw (vec path)
   :path (into []
               (map (fn [[n1 n2]]
                      (dag/edge-attr table n1 n2 :syms)))
               (partition 2 1 path))})

(defn solve
  "Return vector of one or more unique paths that satisfy the given constraint
   state"
  [{:keys [table] :as constraint-state}]
  (let [paths (->> (dag/all-paths table
                                  (start-node constraint-state)
                                  (end-node constraint-state))
                   (map (partial decode-solution-path constraint-state)))]
    (reduce
      (fn [m {:keys [raw path]}]
        (update m path (fnil conj []) raw))
      {}
      paths)))

(defn- remove-unit-paths
  "Given a constraint state, remove all unit (length 1) nonterminal path
   solutions"
  [{:keys [table] :as constraint-state}]
  (let [start (start-node constraint-state)
        end (end-node constraint-state)
        unit-edge-syms (dag/edge-attr table start end :syms)
        terminals (into #{}
                        (filter parser/terminal?)
                        unit-edge-syms)]
    (if (seq terminals)
      (update constraint-state :table
             dag/update-edge-attr start end :syms set/intersection terminals)
      (update constraint-state :table
              dag/remove-edge start end))))

(defn has-unit-solution?
  "Return true iff the given constraint state has a length 1 solution."
  [{:keys [table] :as constraint-state}]
  (dag/has-edge? table
                 (start-node constraint-state)
                 (end-node constraint-state)))

(defn solve-shortest
  "Return all shortest path solutions to the given constraint state."
  [{:keys [table] :as constraint-state}]
  (let [paths (as-> table x
                (dag/shortest-paths x
                                    (start-node constraint-state)
                                    (end-node constraint-state))
                (map (partial decode-solution-path constraint-state) x))]
    (reduce
      (fn [m {:keys [raw path]}]
        (update m path (fnil conj []) raw))
      {}
      paths)))

(defn solve-shortest-non-unit
  "Return all non-unit shortest path solutions to the given constraint state."
  [constraint-state]
  (solve-shortest (remove-unit-paths constraint-state)))

(defn enumerate-path
  "Given a solution path, return an explicit enumeration of all encoded
   production bodies.  This leads to exponential space explosion, so only use
   on paths known to be small"
  [path]
  (cartesian-product path))

(defn compute-provenance-groups
  "Given a collection of provenances, return a sequence of vectors such that
   each vector contains provenances that would belong to the same parse forest"
  [provenances]
  (let [grouped-provenances (group-by first provenances)]
    (apply set/union
           (for [[sample-id group] (group-by first provenances)]
             (let [uf (reduce
                        uf/add
                        (uf/new-union-find)
                        group)
                   edges (for [[_ x-label :as x] group
                               [_ y-label :as y] group
                               :when (and (not= x y)
                                          (ti-strictly-contains? x-label y-label))]
                           [x y])
                   uf (reduce
                        (fn [acc [x y]] (uf/union acc x y))
                        uf
                        edges)]
               (uf/->sets uf))))))

(defn all-syms
  "Return set of all symbols mentioned in the given constraint state"
  [{:keys [table provenance] :as constraint-state}]
  (let [edge-syms (into #{}
                        (mapcat (fn [[from to]]
                                  (dag/edge-attr table from to :syms)))
                        (dag/edges table))
        provenance-syms (into #{}
                              (map (fn [[_ [nt]]] nt))
                              provenance)]
    (into edge-syms provenance-syms)))
