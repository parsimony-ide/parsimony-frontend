(ns parsimony.dag-test
  (:require [cljs.pprint :refer [pprint]]
            [cljs.test :refer-macros [deftest is testing]]
            [parsimony.dag :as dag]
            [parsimony.util :refer [pprint-str] :refer-macros [inspect inspect-pp with-inspection without-inspection]]))

(deftest dag-1
  ;; g is a tree
  (let [g (dag/add-edges (dag/new-dag) [:A :B] [:A :C])]
    (is g)
    (is (dag/has-edge? g :A :B))
    (is (dag/has-edge? g :A :C))
    (is (dag/has-node? g :A))
    (is (dag/has-node? g :B))
    (is (= (dag/successors g :A) #{:B :C}))
    (is (= (dag/successors (dag/remove-edge g :A :B) :A)
           #{:C}))
    (is (= (dag/successors (dag/remove-edge g :A :C) :A)
           #{:B}))
    (is (not (dag/has-edge? (dag/remove-node g :A) :A :B)))
    (is (not (dag/has-edge? (dag/remove-node g :A) :A :C)))
    (is (= (dag/predecessors g :B) #{:A}))
    (is (= (dag/predecessors g :C) #{:A}))
    (is (= (set (dag/edges g)) #{[:A :B] [:A :C]}))
    (is (= (dag/nodes g) #{:A :B :C}))
    (is (= (dag/nodes (dag/successor-subgraph g [:A]))
           #{:A :B :C}))
    (is (= (dag/nodes (dag/successor-subgraph g [:B]))
           #{:B}))
    (is (= (dag/nodes (dag/successor-subgraph g [:C]))
           #{:C}))
    ))

(deftest dag-2
  ;; g is not a tree
  (let [g (dag/add-edges (dag/new-dag) [:A :C] [:B :C])]
    (is g)
    (is (dag/has-edge? g :A :C))
    (is (dag/has-edge? g :B :C))
    (is (dag/has-node? g :A))
    (is (dag/has-node? g :B))
    (is (dag/has-node? g :C))
    (is (= (dag/successors g :A) #{:C}))
    (is (= (dag/successors g :B) #{:C}))
    (is (= (dag/predecessors g :C) #{:A :B}))
    (is (= (dag/nodes (dag/successor-subgraph g [:A]))
           #{:A :C}))
    (is (= (dag/nodes (dag/successor-subgraph g [:B]))
           #{:B :C}))
    (is (= (set (dag/edges (dag/successor-subgraph g [:A])))
           #{[:A :C]}))
    (is (= (set (dag/edges (dag/successor-subgraph g [:B])))
           #{[:B :C]}))
    (is (= (dag/roots g)
           #{:A :B}))
    (is (= (dag/leaves g)
           #{:C}))))

(deftest compose-dags-1
  (let [g1 (dag/new-dag)
        g2 (dag/add-edges g1 [:A :B] [:A :C])
        g3 (dag/add-edges g1 [:B :C])
        g4 (dag/compose-dags g1 g2 g3)]
    (is (= 0 (count (dag/nodes (dag/compose-dags (dag/new-dag) (dag/new-dag))))))
    (is (= #{:A :B :C} (dag/nodes g4)))
    (is (= #{[:A :B] [:A :C] [:B :C]} (set (dag/edges g4))))))

(deftest remove-subgraph-1
  (let [g1 (dag/new-dag)
        g2 (dag/add-edge g1 :A :B)
        g3 (dag/add-edge g2 :A :C)]
    (is (= g2 (dag/remove-subgraph g2 g1)))
    (is (= g1 (dag/remove-subgraph g2 g2)))
    (is (= (dag/add-edge g1 :A :C)
           (dag/remove-subgraph g3 g2)))))

(deftest remove-subgraph-2
  (let [g (-> (dag/new-dag)
              (dag/add-edges [:P :N]
                             [:P :M]
                             [:N :x]
                             [:M :y]
                             [:M :z]))
        g1 (-> g
               (dag/predecessor-subgraph #{:x :y})
               (dag/remove-node :P))]
    #_(inspect-pp g)
    #_(inspect-pp g1)
    #_(inspect-pp (dag/remove-subgraph g g1))))

(deftest replace-nodes-1
  (let [g (dag/new-dag)
        g1 (dag/add-edges g [:A :B] [:A :C])]
    (is (= g (dag/replace-nodes g {})))
    (is (= #{:X :B :C} (dag/nodes (dag/replace-nodes g1 {:A :X}))))
    (is (= #{[:X :B] [:X :C]}
           (set (dag/edges (dag/replace-nodes g1 {:A :X})))))))

(deftest transitive-closure-1
  (let [g1 (-> (dag/new-dag)
               (dag/add-edges [:A :B] [:B :C]))
        g2 (-> g1
               (dag/add-edges [:C :D] [:C :E]))]
    (is (dag/has-edge? (dag/transitive-closure g1) :A :C))
    (is (dag/has-edge? (dag/transitive-closure g2) :A :E))
    (is (= 0 (count (dag/nodes (dag/transitive-closure (dag/new-dag))))))))

(deftest topological-sort-1
  (let [g1 (-> (dag/new-dag)
               (dag/add-edges [:A :B] [:B :C]))
        g2 (-> g1
               (dag/add-edge :E :B)
               (dag/add-edge :B :D))]
    (is (= [] (dag/topological-sort (dag/new-dag))))
    (is (= [:A :B :C] (dag/topological-sort g1)))
    (let [index-map (into {} (map-indexed #(vector %2 %1)) (dag/topological-sort g2))]
      (is (> (:B index-map) (:E index-map)))
      (is (> (:C index-map) (:E index-map)))
      (is (> (:D index-map) (:B index-map)))
      (is (> (:D index-map) (:A index-map))))))

(deftest attributes-1
  (let [g (-> (dag/new-dag)
              (dag/add-edge :A :B)
              (dag/add-edge-attr :A :B :foo :bar))]
    (is (= :bar (dag/edge-attr g :A :B :foo)))
    (is (not (dag/edge-attr (dag/remove-edge g :A :B) :A :B :foo))))
  (let [g (-> (dag/new-dag)
              (dag/add-edge :A :B)
              (dag/add-node-attr :A :m :n)
              (dag/add-node-attr :B :p :q))]
    (is (= :n (dag/node-attr g :A :m)))
    (is (= :q (dag/node-attr g :B :p)))
    (is (= :n (dag/node-attr (dag/remove-edge g :A :B) :A :m)))
    (is (not (dag/node-attr (dag/remove-node g :A) :A :m))))
  (let [g1 (-> (dag/new-dag)
               (dag/add-edge :A :B)
               (dag/add-edge :A :C)
               (dag/add-node-attr :A :x :y))
        g2 (-> (dag/new-dag)
               (dag/add-edge :A :B)
               (dag/add-edge-attr :A :B :e :f))
        g3 (dag/add-subgraph g1 g2)]
    (is (= :f (dag/edge-attr g3 :A :B :e)))
    (is (= :y (dag/node-attr g3 :A :x)))))

(deftest attributes-2
  (let [g (-> (dag/new-dag)
              (dag/add-edge :A :B)
              (dag/add-node-attr :B :z :w)
              (dag/add-edge-attr :A :B :x :y)
              (dag/replace-nodes {:A :E
                                  :B :F}))]
    (is (= :y (dag/edge-attr g :E :F :x)))
    (is (= :w (dag/node-attr g :F :z)))))

(deftest attributes-3
  (let [g (-> (dag/new-dag)
              (dag/add-edge :A :B)
              (dag/add-edge :B :C)
              (dag/add-edge :B :D)
              (dag/add-edge :A :E)
              (dag/add-node-attr :B :w 0)
              (dag/add-node-attr :B :x 1)
              (dag/add-edge-attr :B :D :y 2)
              (dag/add-edge-attr :B :D :z 3)
              (dag/add-node-attr :C :a 100))
        g1 (dag/successor-subgraph g [:B])
        g2 (dag/predecessor-subgraph g [:C :D])]
    ;; successor-subgraph
    (is (= 0 (dag/node-attr g1 :B :w)))
    (is (= 1 (dag/node-attr g1 :B :x)))
    (is (= 2 (dag/edge-attr g1 :B :D :y)))
    (is (= 3 (dag/edge-attr g1 :B :D :z)))
    (is (= 100 (dag/node-attr g1 :C :a)))
    ;; predecessor-subgraph
    (is (= 0 (dag/node-attr g2 :B :w)))
    (is (= 1 (dag/node-attr g2 :B :x)))
    (is (= 2 (dag/edge-attr g2 :B :D :y)))
    (is (= 3 (dag/edge-attr g2 :B :D :z)))
    (is (= 4 (dag/edge-attr (dag/update-edge-attr g2 :B :D :z + 1)
                            :B :D :z)))
    (is (= 100 (dag/node-attr g2 :C :a)))))

(deftest all-paths-1
  (without-inspection
    (let [g (-> (dag/new-dag)
                (dag/add-edge :A :B)
                (dag/add-edge :B :C)
                (dag/add-edge :C :D)
                (dag/add-edge :B :D)
                (dag/add-edge :A :D))]
      (is (= (dag/all-paths g :A :D)
             #{[:A :B :C :D]
               [:A :B :D]
               [:A :D]})))))

(deftest all-path-nodes-1
  (let [g1 (-> (dag/new-dag)
               (dag/add-edge :A :B)
               (dag/add-edge :A :C)
               (dag/add-edge :B :D))
        g2 (-> (dag/new-dag)
               (dag/add-edge :A :B)
               (dag/add-edge :A :C)
               (dag/add-edge :B :C)
               (dag/add-edge :C :D))
        g3 (-> g2
               (dag/add-edge :B :E))
        g4 (-> g3
               (dag/add-edge :C :F)
               (dag/add-edge :F :A)
               (dag/add-edge :F :G))
        g5 (-> g4
               (dag/add-edge :E :A))]
    (is (= (dag/all-path-nodes g1 :A :D)
           #{:A :B :D}))
    (is (= (dag/all-path-nodes g2 :A :D)
           #{:A :B :C :D}))
    (is (= (dag/all-path-nodes g3 :A :D)
           #{:A :B :C :D}))
    (is (= (dag/all-path-nodes g4 :A :D)
           #{:A :B :C :D :F}))
    (is (= (dag/all-path-nodes g5 :A :D)
           #{:A :B :C :D :E :F}))))

(deftest shortest-paths-1
  (let [g1 (-> (dag/new-dag)
               (dag/add-edge :A :B)
               (dag/add-edge :A :C)
               (dag/add-edge :B :D)
               (dag/add-edge :C :D))
        g2 (-> (dag/new-dag)
               (dag/add-edge :A :D))
        g3 (-> (dag/new-dag)
               (dag/add-edge :A :A)
               (dag/add-edge :A :D)
               (dag/add-edge :D :D))
        g4 (-> (dag/new-dag)
               (dag/add-edge :A :B)
               (dag/add-edge :B :C)
               (dag/add-edge :C :D)
               (dag/add-edge :B :E)
               (dag/add-edge :C :F)
               (dag/add-edge :F :A)
               (dag/add-edge :F :G)
               (dag/add-edge :E :A)
               (dag/add-edge :E :D))]
    (is (= (set (dag/shortest-paths g1 :A :D))
           #{'(:A :B :D)
             '(:A :C :D)}))
    (is (= (set (dag/shortest-paths g2 :A :D))
           #{'(:A :D)}))
    (is (= (set (dag/shortest-paths g3 :A :D))
           #{'(:A :D)}))
    (is (= (set (dag/shortest-paths g4 :A :D))
           #{'(:A :B :C :D)
             '(:A :B :E :D)}))))

(deftest cyclic-1
  (without-inspection
    (is (not (dag/cyclic? (dag/new-dag))))
    (is (dag/cyclic? (-> (dag/new-dag)
                         (dag/add-edge :A :B)
                         (dag/add-edge :B :A))))
    (is (dag/cyclic? (-> (dag/new-dag)
                         (dag/add-edge :A :A))))
    (is (dag/cyclic? (-> (dag/new-dag)
                         (dag/add-edge :A :B)
                         (dag/add-edge :C :D)
                         (dag/add-edge :D :E)
                         (dag/add-edge :E :C))))
    (is (dag/cyclic? (-> (dag/new-dag)
                         (dag/add-edge :A :B)
                         (dag/add-edge :B :C)
                         (dag/add-edge :C :B))))
    (is (not (dag/cyclic? (-> (dag/new-dag)
                              (dag/add-edge :A :B)
                              (dag/add-edge :A :C)
                              (dag/add-edge :B :D)
                              (dag/add-edge :C :D)))))))

(deftest sccs-1
  (without-inspection
    (is (= #{}
           (dag/sccs (dag/new-dag))))
    (is (= #{#{:A}}
           (dag/sccs (-> (dag/new-dag)
                         (dag/add-node :A)))))
    (is (= #{#{:A} #{:B}}
           (dag/sccs (-> (dag/new-dag)
                         (dag/add-edge :A :B)))))
    (is (= #{#{:A :B}}
           (dag/sccs (-> (dag/new-dag)
                         (dag/add-edge :A :B)
                         (dag/add-edge :B :A)))))
    (is (= #{#{:A :B} #{:C}}
           (dag/sccs (-> (dag/new-dag)
                         (dag/add-edge :A :B)
                         (dag/add-edge :B :A)
                         (dag/add-node :C)))))
    (is (= #{#{:A :B} #{:C} #{:D :E}}
           (dag/sccs (-> (dag/new-dag)
                         (dag/add-edge :A :B)
                         (dag/add-edge :B :A)
                         (dag/add-edge :C :D)
                         (dag/add-edge :D :E)
                         (dag/add-edge :E :D)))))))

(deftest unit-cycles-1
  (without-inspection
    (is (= #{[0] [1]}
           (set (dag/unit-cycles
                  (-> (dag/new-dag)
                      (dag/add-edge 0 0)
                      (dag/add-edge 0 1)
                      (dag/add-edge 1 1))))))))

(deftest all-cycles-1
  (without-inspection
    (is (= #{[0]
             [0 1]
             [0 1 2]
             [0 2 1]
             [0 2]
             [1 2]}
           (set (dag/all-cycles
                  (-> (dag/new-dag)
                      (dag/add-edge 0 0)
                      (dag/add-edge 0 1)
                      (dag/add-edge 1 0)
                      (dag/add-edge 1 2)
                      (dag/add-edge 2 1)
                      (dag/add-edge 0 2)
                      (dag/add-edge 2 0))))))))

(deftest successor-subgraph-with-blocking-1
  (let [es #{[:A :B]
             [:A :C]
             [:A :D]
             [:B :E]
             [:C :E]
             [:D :E]}
        g (-> (dag/new-dag)
              (dag/add-edges* es))]
    (is (= (dag/edges (dag/successor-subgraph g [:A] [:B]))
           (disj es [:B :E])))))

(deftest predecessor-subgraph-with-blocking-1
  (let [es #{[:A :B]
             [:A :C]
             [:A :D]
             [:B :E]
             [:C :E]
             [:D :E]}
        g (-> (dag/new-dag)
              (dag/add-edges* es))]
    (is (= (dag/edges (dag/predecessor-subgraph g [:E] [:B]))
           (disj es [:A :B])))))
