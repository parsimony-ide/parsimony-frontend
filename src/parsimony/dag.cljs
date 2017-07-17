(ns parsimony.dag
  "Generic DAG data structure and query/manipulation API"
  (:refer-clojure :exclude [ancestors descendants empty?])
  (:require [cljs.pprint :refer [pprint]]
            [clojure.set :as set]
            [medley.core :refer [dissoc-in]]
            [parsimony.util :refer [find-first] :refer-macros [inspect inspect-pp with-inspection]]
            [parsimony.console :as console]))

(defn new-dag []
  {:edges (hash-map)
   :reverse-edges (hash-map)
   :nodes #{}
   :edge-attrs (hash-map)
   :node-attrs (hash-map)})

(declare predecessors successors out-degree edges nodes incident-edges num-incident
         ancestors roots successor-subgraph)

(defn add-node [g n]
  (-> g
      (update :nodes conj n)))

(defn add-nodes* [g nodes]
  (-> g
      (update :nodes into nodes)))

(defn add-nodes [g & nodes]
  (add-nodes* g nodes))

(defn add-node-attr [g n k v]
  (assoc-in g [:node-attrs n k] v))

(defn add-node-attrs [g n m]
  (assoc-in g [:node-attrs n] m))

(defn remove-node-attrs [g n]
  (as-> g g
    (dissoc-in g [:node-attrs n])
    (if-not (:node-attrs g)
      (assoc g :node-attrs {})
      g)))

(defn add-edge* [g [n1 n2]]
  (-> g
      (update-in [:edges n1] (fnil conj #{}) n2)
      (update-in [:reverse-edges n2] (fnil conj #{}) n1)
      (update :nodes conj n1 n2)))

(defn add-edge [g n1 n2]
  (add-edge* g [n1 n2]))

(defn add-edges* [g es]
  (reduce add-edge* g es))

(defn add-edges [g & es]
  (add-edges* g es))

(defn add-edge-attr [g n1 n2 k v]
  (assoc-in g [:edge-attrs n1 n2 k] v))

(defn update-edge-attr [g n1 n2 k f v]
  (update-in g [:edge-attrs n1 n2 k] #(f % v)))

(defn add-edge-attrs [g n1 n2 m]
  (assoc-in g [:edge-attrs n1 n2] m))

(defn remove-edge-attrs [g n1 n2]
  (as-> g g
    (dissoc-in g [:edge-attrs n1 n2])
    (if-not (:edge-attrs g)
      (assoc g :edge-attrs {})
      g)))

(defn- merge-edge-attrs [g g']
  (assoc g :edge-attrs (merge-with merge (:edge-attrs g) (:edge-attrs g'))))

(defn- merge-node-attrs [g g']
  (assoc g :node-attrs (merge-with merge (:node-attrs g) (:node-attrs g'))))

(defn add-subgraph
  "Add the subgraph g' to g"
  [g g']
  (-> g
      (add-edges* (edges g'))
      (add-nodes* (nodes g'))
      (merge-edge-attrs g')
      (merge-node-attrs g')))

(defn remove-edge* [g [n1 n2]]
  (let [ss (disj (successors g n1) n2)
        ps (disj (predecessors g n2) n1)
        g' (if (seq ss)
             (assoc-in g [:edges n1] ss)
             (dissoc-in g [:edges n1]))
        g'' (if (seq ps)
              (assoc-in g' [:reverse-edges n2] ps)
              (dissoc-in g' [:reverse-edges n2]))]
    ;; merge with empty dag to ensure all required keys exist
    (merge (new-dag) (remove-edge-attrs g'' n1 n2))))

(defn remove-edge [g n1 n2]
  (remove-edge* g [n1 n2]))

(defn remove-edges* [g es]
  (reduce remove-edge* g es))

(defn remove-edges [g & es]
  (remove-edges* g es))

(defn remove-node
  "Remove a node from the DAG. Also removes incident edges"
  [g n]
  (let [ss (successors g n)
        ps (predecessors g n)]
    (-> g
        (update :nodes disj n)
        (remove-node-attrs n)
        (remove-edges* (map (partial vector n) ss))
        (remove-edges* (map #(vector % n) ps)))))

(defn remove-nodes* [g nodes]
  (reduce remove-node g nodes))

(defn remove-nodes [g & nodes]
  (remove-nodes* g nodes))

(defn remove-subgraph
  "Remove the subgraph g' from g. Don't remove a node if it has incident edges
   outside of g'."
  [g g']
  (as-> g g
    (remove-edges* g (edges g'))
    (remove-nodes* g (filter #(zero? (num-incident g %)) (nodes g')))))

(defn- rename-edge-attrs [g replacement-map]
  (assoc g :edge-attrs
         (reduce (fn [acc [n1 m]]
                   (assoc acc (get replacement-map n1 n1) (set/rename-keys m replacement-map)))
                 {}
                 (:edge-attrs g))))

(defn replace-nodes
  "Replace every instance of a key in replacement-map with its corresponding value"
  [g replacement-map]
  (let [f (fn [n] (get replacement-map n n))
        f' (fn [[n1 n2]] [(f n1) (f n2)])]
    (-> (new-dag)
        (add-nodes* (map f (nodes g)))
        (add-edges* (map f' (edges g)))
        (assoc :edge-attrs (:edge-attrs (rename-edge-attrs g replacement-map)))
        (assoc :node-attrs (set/rename-keys (:node-attrs g) replacement-map)))))

(defn has-edge*? [g [n1 n2]]
  (contains? (get-in g [:edges n1]) n2))

(defn has-edge? [g n1 n2]
  (has-edge*? g [n1 n2]))

(defn has-node? [g n]
  (contains? (:nodes g) n))

(defn successors
  "Return set of successors of n, or nil if none"
  [g n]
  (get-in g [:edges n]))

(defn predecessors
  "Return set of predecessors of n, or nil if none"
  [g n]
  (get-in g [:reverse-edges n]))

(defn in-edges [g n]
  (set (for [p (predecessors g n)]
         [p n])))

(defn out-edges [g n]
  (set (for [s (successors g n)]
         [n s])))

(defn incident-edges [g n]
  (set/union (in-edges g n)
             (out-edges g n)))

(defn in-degree [g n]
  (count (predecessors g n)))

(defn out-degree [g n]
  (count (successors g n)))

(defn num-incident [g n]
  (+ (in-degree g n) (out-degree g n)))

(defn edges
  "Return set of edges, where each edge is a 2-element vector [from to]"
  [g]
  (set (for [[n1 nodes] (:edges g)
             n2 nodes]
         [n1 n2])))

(defn nodes
  "Return set of nodes of g"
  [g]
  (:nodes g))

(defn node-attr
  "Return the attribute with the given key on the supplied node if it exists, otherwise nil"
  [g n k]
  (get-in g [:node-attrs n k]))

(defn node-attrs
  "Return the entire attribute map for the given node if it exists, otherwise nil"
  [g n]
  (get-in g [:node-attrs n]))

(defn edge-attr
  "Return the attribute with the given key on the supplied edge if it exists, otherwise nil"
  [g n1 n2 k]
  (get-in g [:edge-attrs n1 n2 k]))

(defn edge-attrs
  "Return the entire attribute map for the given edge if it exists, otherwise nil"
  [g n1 n2]
  (get-in g [:edge-attrs n1 n2]))

(defn successor-subgraph
  "Return a new DAG containing only those nodes reachable from `nodes`. If
   given three arguments, the third argument is a set of nodes through which
   reachability is blocked."
  ([g nodes]
   (successor-subgraph g nodes nil))
  ([g nodes blocking-nodes]
   (loop [worklist (vec nodes) g' (new-dag)]
     (if-let [n (peek worklist)]
       (let [nattrs (if-let [na (node-attrs g n)]
                      (assoc (:node-attrs g') n na)
                      (:node-attrs g'))]
         (if-let [ss (seq (successors g n))]
           (let [edges (map (partial vector n) ss)
                 eattrs (reduce
                          (fn [acc e]
                            (if-let [ea (apply edge-attrs g e)]
                              (assoc-in acc e ea)
                              acc))
                          (:edge-attrs g')
                          edges)]
             (recur (into (pop worklist) (remove (set blocking-nodes) ss))
                    (-> g'
                        (add-edges* edges)
                        (assoc :edge-attrs eattrs)
                        (assoc :node-attrs nattrs))))
           (recur (pop worklist)
                  (-> g'
                      (add-node n)
                      (assoc :node-attrs nattrs)))))
       g'))))

(defn predecessor-subgraph
  "Return a new DAG containing only those nodes that reach some node in
   `nodes`. If given three arguments, the third argument is a set of nodes
   through which reachability is blocked."
  ([g nodes]
   (predecessor-subgraph g nodes nil))
  ([g nodes blocking-nodes]
   (loop [worklist (vec nodes) g' (new-dag)]
     (if-let [n (peek worklist)]
       (let [nattrs (if-let [na (node-attrs g n)]
                      (assoc (:node-attrs g') n na)
                      (:node-attrs g'))]
         (if-let [ps (seq (predecessors g n))]
           (let [edges (map #(vector % n) ps)
                 eattrs (reduce
                          (fn [acc e]
                            (if-let [ea (apply edge-attrs g e)]
                              (assoc-in acc e ea)
                              acc))
                          (:edge-attrs g')
                          edges)]
             (recur (into (pop worklist) (remove (set blocking-nodes) ps))
                    (-> g'
                        (add-edges* edges)
                        (assoc :edge-attrs eattrs)
                        (assoc :node-attrs nattrs))))
           (recur (pop worklist)
                  (-> g'
                      (add-node n)
                      (assoc :node-attrs nattrs)))))
       g'))))

(defn ancestors
  "Return set of ancestor nodes of n in g"
  [g n]
  (disj (nodes (predecessor-subgraph g [n])) n))

(defn descendants
  "Return set of descendant nodes of n in g"
  [g n]
  (disj (nodes (successor-subgraph g [n])) n))

(defn root? [g n]
  (not (seq (predecessors g n))))

(defn roots
  [g]
  (set (for [n (nodes g) :when (root? g n)]
         n)))

(defn leaf? [g n]
  (not (seq (successors g n))))

(defn unit? [g n]
  (= 1 (count (successors g n))))

(defn leaves
  "Return set of leaves in g"
  [g]
  (loop [worklist (vec (roots g)) acc #{}]
    (if-let [n (peek worklist)]
      (if (leaf? g n)
        (recur (pop worklist) (conj acc n))
        (recur (into (pop worklist) (successors g n)) acc))
      acc)))

(defn compose-dags
  ([g1 g2]
   (-> g1
       (add-edges* (edges g2))
       (add-nodes* (nodes g2))))
  ([g1 g2 g3 & gs]
   (reduce compose-dags g1 (into [g2 g3] gs))))

(defn compose-dags* [gs]
  (apply compose-dags gs))

(defn transitive-closure
  [g]
  (add-edges* (new-dag)
              (for [n (nodes g)
                    n' (-> g
                           (successor-subgraph [n])
                           (nodes)
                           (disj n))]
                [n n'])))

(defn transitive-reduction
  [g]
  (let [g-closed (transitive-closure g)
        g2 (add-edges* (new-dag)
                       (for [[n m] (edges g)
                             [n' m'] (edges g-closed)
                             :when (= m n')]
                         [n m']))]
    (add-edges* (new-dag)
                (for [e (edges g)
                      :when (not (has-edge*? g2 e))]
                  e))))

(defn topological-sort
  "Return topologically sorted sequence of nodes in g"
  [g]
  (let [no-incoming? (fn [g n] (zero? (count (predecessors g n))))
        start-nodes (filter (partial no-incoming? g) (nodes g))]
    (loop [g g l [] s (set start-nodes)]
      (if-let [n (first s)]
        (let [g' (remove-edges* g (out-edges g n))]
          (recur g'
                 (conj l n)
                 (into (disj s n) (filter (partial no-incoming? g') (successors g n)))))
        l))))

(defn empty?
  [g]
  (= 0 (count (nodes g))))

(defn all-paths
  "Return all paths from node n1 to n2, if any exist, otherwise nil"
  [g n1 n2]
  (loop [paths #{[n1]}
         found #{}]
    (if-let [p (first paths)]
      (if (= n2 (peek p))
        (recur (disj paths p)
               (conj found p))
        (let [succs (successors g (peek p))]
          (recur (into (disj paths p)
                       (map (partial conj p) succs))
                 found)))
      found)))

(defn- bfs-pred-graph
  "Return the predecessor graph of a BFS search from n1 to n2"
  [g start-node]
  (letfn [(add-pred [pred-g succs n depth]
            (reduce
              (fn [pred-g s]
                (-> pred-g
                    (add-edge s n)
                    (add-node-attr s :depth (inc depth))))
              (add-node-attr pred-g n :depth depth)
              succs))]
    (loop [queue [[start-node 0]] seen #{start-node} pred-g (new-dag)]
      (if-let [[n depth] (peek queue)]
        (let [succs (successors g n)]
          (recur (into (pop queue)
                       (map #(vector % (inc depth)))
                       (set/difference succs seen))
                 (into seen succs)
                 (add-pred pred-g succs n depth)))
        pred-g))))

(defn all-path-nodes
  "Return all nodes along all paths from node n1 to n2, if any exist, otherwise nil"
  [g n1 n2]
  (-> g
      (bfs-pred-graph n1)
      (bfs-pred-graph n2)
      (nodes)))

(defn shortest-paths
  "Return all shortest paths between n1 and n2"
  [g n1 n2]
  (let [g' (-> g
               (bfs-pred-graph n1)
               (bfs-pred-graph n2))
        extend-path (fn [[n :as path]]
                      (let [preds (predecessors g' n)]
                        (map #(conj path %) preds)))
        result (loop [paths [(list n2)]]
                 (when (seq paths)
                   (let [next-level-paths (mapcat extend-path paths)
                         frontier (into #{}
                                        (map first)
                                        next-level-paths)]
                     (if (contains? frontier n1)
                       ;; solution found
                       (filter #(= n1 (first %)) next-level-paths)
                       (recur next-level-paths)))))]
    #_(console/debug ::shortest-paths {:g g :g' g' :result result})
    result))

(defn node-depth-map
  "Return map from node to its depth according to BFS traversal"
  [g root]
  (into {}
        (map (fn [[n {d :depth}]]
               [n d]))
        (:node-attrs (bfs-pred-graph g root))))

(defn crop-to-depth
  "Remove all nodes beyond depth. Assumes g has a single root."
  [g depth]
  (let [-roots (roots g)
        depth-map (node-depth-map g (first -roots))
        frontier (for [[n d] depth-map
                       :when (= d depth)]
                   n)]
    (successor-subgraph g -roots frontier)))

(defn max-depth [g]
  "Returns maximum depth of g. Assumes g has a single root."
  (->> (node-depth-map g (first (roots g)))
       (vals)
       (apply max 0)))

;; XXX: this namespace is called parsimony.dag, but it has since grown to include the below functions for cyclic graphs...
;; This is unsafe since the above functions currently don't check for acyclicity before performing operations that
;; could loop infinitely on cyclic graphs!

(defn- detect-cycle
  [g n stack]
  (if (contains? (set stack) n)
    {:cycle? true :stack stack :n n}
    (let [res (map #(detect-cycle g % (conj stack n))
                   (successors g n))]
      (if-let [detected (first (filter :cycle? res))]
        detected
        {:cycle? false
         :seen (-> #{}
                   (into (mapcat :seen) res)
                   (conj n))}))))

(defn cyclic?
  "Return true iff the given graph is cyclic"
  [g]
  (loop [unseen (nodes g) seen #{}]
    (when-let [n (first unseen)]
      (let [{:keys [cycle?] :as res} (detect-cycle g n [])]
        (if cycle?
          true
          (recur (set/difference unseen (:seen res))
                 (into seen (:seen res))))))))

(defn sccs
  "Return SCCs using Kosaraju's algorithm"
  [g]
  (let [visited (atom #{})
        assignment (atom {})
        L (atom '())]
    (letfn [(visit [u]
              (when-not (contains? @visited u)
                (swap! visited conj u)
                (doseq [v (successors g u)]
                  (visit v))
                (swap! L conj u)))
            (assign [u root]
              (when-not (contains? @assignment u)
                (swap! assignment assoc u root)
                (doseq [v (predecessors g u)]
                  (assign v root))))]
      (doseq [u (nodes g)]
        (visit u))
      (doseq [u @L]
        (assign u u))
      (set (vals (reduce
                   (fn [acc [u root]]
                     (update acc root (fnil conj #{}) u))
                   {}
                   @assignment))))))

(defn induced-subgraph
  "Return the subgraph of g induced by the given nodes"
  [g vs]
  (reduce remove-node
          g
          (set/difference (set (nodes g))
                          (set vs))))

(defn johnson-all-cycles
  "Return a vector of cycles, where each cycle is a vector of nodes.
   This implementation uses Johnson's algorithm for enumerating simple cycles in a graph:

   Finding all the elementary circuits of a directed graph.
   D. B. Johnson, SIAM Journal on Computing 4, no. 1, 77-84, 1975.
   http://dx.doi.org/10.1137/0204007

   Note that this function does not detect unit cycles (edges from and to the same node)"
  [g]
  ;; useful Java implementation for learning how this works: https://github.com/josch/cycles_johnson_meyer
  (let [node->index (into {} (map-indexed #(vector %2 %1)) (nodes g))
        index->node (set/map-invert node->index)
        cycles (atom [])
        blocked (atom #{})
        B (atom {})
        stack (atom [])]
    (letfn [(min-scc [s]
              (let [subgraph-nodes (for [i (range s (count (nodes g)))]
                                     (index->node i))
                    g (induced-subgraph g subgraph-nodes)
                    components (sccs g)
                    indexed-components (for [c (sccs g)
                                             :when (> (count c) 1)]
                                         (let [min-index (first (sort (map node->index c)))]
                                           [min-index c]))]
                (first (sort-by first indexed-components))))
            (unblock [v]
              (swap! blocked disj v)
              (let [b-list (get @B v)]
                (doseq [w b-list]
                  (swap! B update v (comp vec next))
                  (when (contains? @blocked w)
                    (unblock w)))))
            (find-cycles [v s g]
              (let [found? (atom nil)]
                (swap! stack conj v)
                (swap! blocked conj v)
                (doseq [w (map node->index (successors g (index->node v)))]
                  (cond
                    ;;
                    (= w s)
                    (do (swap! cycles
                               conj
                               (into []
                                     (map index->node)
                                     @stack))
                        (reset! found? true))
                    ;;
                    (and (not (contains? @blocked w))
                         (find-cycles w s g))
                    (reset! found? true)
                    ;;
                    :else nil))
                (if @found?
                  (unblock v)
                  (doseq [w (map node->index (successors g (index->node v)))]
                    (when-not (find-first #{v} (get @B w))
                      (swap! B update w (fnil conj []) v))))
                (swap! stack #(vec (remove #{v} %)))
                @found?))]
      (loop [s 0]
        (if-let [[s scc] (min-scc s)]
          (let [g (induced-subgraph g scc)]
            (doseq [v (nodes g)
                    :when (seq (successors g v))]
              (swap! blocked disj (node->index v)))
            (find-cycles s s g)
            (recur (inc s)))
          @cycles)))))

(defn unit-cycles
  "Return a vector of unit cycles (cycles with an edge from and to the same
   node), where each unit cycle is represented as a vector of a single node"
  [g]
  (vec (for [[from to] (edges g)
             :when (= from to)]
         [from])))

(defn all-cycles
  "Return a vector of cycles as with johnson-all-cycles, with one difference:
   this function also returns unit cycles"
  [g]
  (into (unit-cycles g)
        (johnson-all-cycles g)))
