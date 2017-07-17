(ns parsimony.asm.inference
  (:require [clojure.set :as set]
            [parsimony.asm-impl-js]
            [parsimony.asm.parser :as asm.parser]
            [parsimony.dag :as dag]
            [parsimony.inference :as inference]
            [parsimony.lexer :as lexer]
            [parsimony.parser :as parser]
            [parsimony.util :refer [vec-remove]]
            [parsimony.console :as console]))

(defn extend-codec
  "Extend a parser codec with lexer symbols"
  [cs lexer {:keys [encode decode n] :as codec}]
  (let [constraint-syms (into #{} (mapcat inference/all-syms) cs)
        terminal-syms (into #{} (map parser/->terminal) (lexer/all-syms lexer))
        new-syms (set/difference (into constraint-syms terminal-syms) (set (keys encode)))]
    #_(console/debug ::extend-codec {:syms new-syms})
    (reduce
      (fn [{:keys [n] :as acc} sym]
        (-> acc
            (assoc-in [:encode sym] n)
            (assoc-in [:decode n] sym)
            (update :n inc)))
      codec
      (sort new-syms)))) ;; must sort to ensure stable encoding

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emscripten <-> CLJS Shim
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn cpp-vertex-info
  "Return a VertexInfo cpp object from the given node"
  [node]
  (let [vertex-info (new js/Module.VertexInfo)]
    (doseq [pos node]
      (.add_position vertex-info pos))
    vertex-info))

(defn cpp-init-edge
  "Add edge to the given cpp ConstraintState"
  [cpp-constraint codec from to syms]
  #_(console/debug ::cpp-init-edge {:from from :to to})
  (let [u (cpp-vertex-info from)
        v (cpp-vertex-info to)]
    (.add_edge cpp-constraint u v)
    (doseq [sym syms]
      #_(console/debug ::add-edge-sym {:from from :to to :sym sym})
      (.add_edge_sym cpp-constraint u v (asm.parser/encode codec sym)))
    (.delete u)
    (.delete v)))

(defn cpp-init-constraint
  "Given a Clojurescript constraint state, return a cpp ConstraintState
   instance"
  [{:keys [table provenance] :as cljs-constraint} codec]
  (let [cpp-constraint (new js/Module.ConstraintState)]
    (doseq [[sample-id [nt i l]] provenance]
      (.add_provenance cpp-constraint sample-id (asm.parser/encode codec nt) i l))
    (doseq [[from to] (dag/edges table)]
      (let [syms (dag/edge-attr table from to :syms)]
        (cpp-init-edge cpp-constraint codec from to syms)))
    (doseq [[sym n] (:encode codec)]
      (when (parser/terminal? sym)
        (.mark_as_terminal cpp-constraint n)))
    cpp-constraint))

(defn cpp-free
  "Free heap space taken by the given cpp object"
  [cpp-object]
  #_(console/warn ::cpp-free "Freeing asm.inference heap space")
  (.delete cpp-object))

(defn provenance
  "Return provenance vector for the given cpp ConstraintState"
  [cpp-constraint codec]
  (let [->provenance
        (fn [n]
          (let [sample-id (.get_provenance_sample_id cpp-constraint n)
                nt (.get_provenance_nt cpp-constraint n)
                i (.get_provenance_i cpp-constraint n)
                l (.get_provenance_l cpp-constraint n)]
            [sample-id [(asm.parser/decode codec nt) i l]]))]
    (into []
          (map ->provenance)
          (range (.num_provenance_elements cpp-constraint)))))

(defn- vint->cljs
  [vint]
  (into []
        (map #(.get vint %))
        (range (.size vint))))

(defn- vvint->cljs
  [vvint]
  (into []
        (map #(vint->cljs (.get vvint %)))
        (range (.size vvint))))

(defn cpp->cljs-constraint
  "Convert a cpp ConstraintState object to a ClojureScript constraint-state.
   Does not free the underlying cpp object"
  [cpp-constraint codec]
  (let [cpp-sources (new js/Module.VVInt)
        cpp-targets (new js/Module.VVInt)
        cpp-syms (new js/Module.VVInt)]
    (.get_edges cpp-constraint cpp-sources cpp-targets cpp-syms)
    (let [sources (vvint->cljs cpp-sources)
          targets (vvint->cljs cpp-targets)
          syms (vvint->cljs cpp-syms)
          table
          (reduce
            (fn [g [source target syms]]
              (-> g
                  (dag/add-edge source target)
                  (dag/add-edge-attr source target :syms
                                     (into #{}
                                           (map (partial asm.parser/decode codec))
                                           syms))))
            (dag/new-dag)
            (map vector sources targets syms))]
      (cpp-free cpp-sources)
      (cpp-free cpp-targets)
      (cpp-free cpp-syms)
      {:table table
       :provenance (provenance cpp-constraint codec)})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraint Solving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn intersect-constraint-states
  "Return a newly allocated cpp ConstraintState representing the intersection
   of the two input ConstraintState objects"
  [c1 c2]
  (let [c3 (new js/Module.ConstraintState)]
    (js/Module.ConstraintState.intersect c1 c2 c3)
    c3))

(defn solve-shortest
  "Return cpp Solution object representing the shortest-path solution
   for the given cpp ConstraintState object"
  [c]
  (let [solution (new js/Module.Solution)]
    (.solve_shortest c solution)
    solution))

(defn solve-shortest-non-unit
  "Return cpp Solution object representing the non-unit shortest-path solution
   for the given cpp ConstraintState object. WARNING: this function mutates the
   underlying ConstraintState object by removing non-unit paths."
  [c]
  (let [solution (new js/Module.Solution)]
    (.solve_shortest_non_unit c solution)
    solution))

(defn- decode-cpp-path
  [cpp-path codec]
  (into []
        (map #(into #{} (map (partial asm.parser/decode codec)) %))
        (vvint->cljs cpp-path)))

(defn decode-solution-compressed-path
  [solution codec]
  (let [cpp-compressed-path (new js/Module.VVInt)
        _ (.get_compressed_path solution cpp-compressed-path)
        decoded (decode-cpp-path cpp-compressed-path codec)]
    (cpp-free cpp-compressed-path)
    decoded))

(defn decode-solution-paths
  [solution codec]
  (let [cpp-raws (new js/Module.VVVInt)
        cpp-paths (new js/Module.VVVInt)
        _ (.get_raws solution cpp-raws)
        _ (.get_paths solution cpp-paths)
        decoded (into []
                      (map (fn [i]
                             {:raw (-> (.get cpp-raws i)
                                       (vvint->cljs))
                              :path (-> (.get cpp-paths i)
                                        (decode-cpp-path codec))}))
                      (range (.size cpp-paths)))]
    (cpp-free cpp-raws)
    (cpp-free cpp-paths)
    decoded))

;;------------------------------------------------------------------------------
;; Learn Partitions
;;------------------------------------------------------------------------------

(defn same-lhs?
  "Return true iff the two cpp constraint states have provenances with the same
   set of LHS nonterminals"
  [c1 c2 codec]
  (letfn [(extract-syms [provenance]
            (into #{}
                  (map (fn [[_ [sym]]] sym))
                  provenance))]
    ;; XXX: Technically, do not need to decode syms before comparing.
    ;; Investigate comparing _encoded_ syms if performance is an issue.
    (= (extract-syms (provenance c1 codec))
       (extract-syms (provenance c2 codec)))))

(defn compatible?
  [c1 c2 codec]
  (when (same-lhs? c1 c2 codec)
    (let [ci (intersect-constraint-states c1 c2)
          result (not (.empty ci))]
      (cpp-free ci)
      result)))

(defn- c1-score
  [cs i j codec]
  (let [n (count cs)
        ci (get cs i)
        cj (get cs j)
        score-one (fn [ci cj ck]
                    (let [cij (intersect-constraint-states ci cj)
                          result (if (= (compatible? ci ck codec)
                                        (compatible? cj ck codec)
                                        (compatible? cij ck codec))
                                   1
                                   0)]
                      (cpp-free cij)
                      result))]
    (reduce + (for [k (range 0 n)
                    :when (and (not= i k)
                               (not= j k))]
                (score-one ci cj (get cs k))))))

(defn- score-partitions
  [cs codec]
  (let [n (count cs)
        candidates
        (for [i (range n)
              j (range (inc i) n)
              :when (compatible? (get cs i) (get cs j) codec)]
          [i j])
        score (fn [[i j]]
                [[i j] (c1-score cs i j codec)])]
    (into {} (map score) candidates)))

(defn- cpp-learn-partitions-iterate
  "One iteration of the learn-partitions algorithm"
  [cs codec]
  (let [scores (score-partitions cs codec)
        max-score (apply max (vals scores))
        winners (for [[k v] scores :when (= max-score v)]
                  k)
        statistics {:count (count cs)
                    :partitions (into (sorted-map) (map vector (range) (map #(provenance % codec) cs)))
                    :scores scores
                    :max-score max-score
                    :winners winners}]
    (console/debug ::cpp-learn-partitions-iterate {:statistics statistics})
    (when-let [[i j] (first winners)]
      (let [winner-constraint (intersect-constraint-states (get cs i) (get cs j))]
        (cpp-free (get cs i))
        (cpp-free (get cs j))
        {:statistics statistics
         :result (-> cs
                     (vec-remove j)
                     (vec-remove i)
                     (conj winner-constraint))}))))

(defn cpp-learn-partitions
  "Low-level interface to partition learning. cs is a vector of cpp
   ConstraintState objects that must be freed by the caller. Use
   learn-and-solve-partitions unless you really know what you're doing."
  [cs codec]
  (loop [cs (vec cs)]
    (if-let [{:keys [result]} (cpp-learn-partitions-iterate cs codec)]
      (recur result)
      cs)))

(defn- differential-learn-partitions
  "Runs the cpp and ClojureScript learn-partitions algorithms in lock-step and
   compares their results. Useful for debugging. Not for use in production."
  [cljs-cs cpp-cs codec]
  (loop [cljs-cs (vec cljs-cs) cpp-cs (vec cpp-cs)]
    (let [a (inference/learn-partitions-iterate cljs-cs)
          b (cpp-learn-partitions-iterate cpp-cs codec)]
      (when (not= (:statistics a) (:statistics b))
        (console/warn ::differential-learn-partitions
                      {:a (:statistics a)
                       :b (:statistics b)})
        (doseq [[x y] (map vector cljs-cs cpp-cs)]
          (let [cljs-y (cpp->cljs-constraint y codec)]
            (when (not= x cljs-y)
              (console/warn ::mismatched-constraints
                            {:x x
                             :y cljs-y})))))
      (if (and a b)
        (recur (:result a) (:result b))
        cpp-cs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn learn-partitions
  "High-level interface to partition learning. cs is a vector of ClojureScript
   constraint states.  Returns a vector of partitioned ClojureScript constraint
   states."
  [cs lexer codec]
  (let [extended-codec (extend-codec cs lexer codec)
        cpp-cs (into []
                     (map #(cpp-init-constraint % extended-codec))
                     cs)
        cpp-partitions (cpp-learn-partitions cpp-cs extended-codec)
        cljs-partitions (into []
                              (map #(cpp->cljs-constraint % extended-codec))
                              cpp-partitions)]
    (doseq [c cpp-partitions]
      (try
        (cpp-free c)
        (catch js/Error _
          (console/warn ::learn-partitions "already freed"))))
    cljs-partitions))

(defn- -solve-fn
  "Internal use only. Helper for implementation of solve-constraints and
   learn-and-solve-constraints."
  [cpp-cs extended-codec]
  (let [solutions
        (into []
              (map
                (fn [c]
                  (let [->path (fn [soln]
                                 (decode-solution-compressed-path soln extended-codec))
                        shortest-soln (solve-shortest c)
                        shortest-path (->path shortest-soln)
                        non-unit-soln (solve-shortest-non-unit c)
                        non-unit-path (->path non-unit-soln)]
                    (cpp-free shortest-soln)
                    (cpp-free non-unit-soln)
                    (merge {:provenance (provenance c extended-codec)
                            :path non-unit-path}
                           (when (not= shortest-path non-unit-path)
                             {:shortest-path shortest-path})))))
              cpp-cs)]
    #_(console/warn ::-solve-fn {:solutions solutions})
    (doseq [c cpp-cs]
      (try
        (cpp-free c)
        (catch js/Error _
          (console/warn ::-solve-fn "already freed"))))
    solutions))

(defn solve-constraints
  "High-level interface to constraint solving. cs is a vector of ClojureScript
   constraint states.  Returns a vector of solutions to the given constraints.
   Does not perform partitioning -- for that, use learn-and-solve-partitions."
  [cs lexer codec]
  (let [extended-codec (extend-codec cs lexer codec)
        cpp-cs (into []
                     (map #(cpp-init-constraint % extended-codec))
                     cs)]
    (-solve-fn cpp-cs extended-codec)))

(defn learn-and-solve-partitions
  "High-level interface to partition learning. cs is a vector of ClojureScript
   constraint states.  Returns a vector of solutions to the resulting
   partitions."
  [cs lexer codec]
  (let [extended-codec (extend-codec cs lexer codec)
        cpp-cs (into []
                     (map #(cpp-init-constraint % extended-codec))
                     cs)
        partitions (cpp-learn-partitions cpp-cs extended-codec)
        ;; partitions (differential-learn-partitions cs cpp-cs extended-codec)
        ;; _ (console/warn ::learn-and-solve-partitions
        ;;                 {:unpartitioned cs
        ;;                  :partitioned (map #(cpp->cljs-constraint % extended-codec) partitions)})
        ]
    (-solve-fn partitions extended-codec)))


