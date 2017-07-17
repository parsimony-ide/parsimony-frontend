(ns parsimony.asm-inference-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [clojure.set :as set]
            [parsimony.common-test :refer [xyz-lexer]]
            [parsimony.lexer :as lexer]
            [parsimony.parser :as parser]
            [parsimony.inference :as inference]
            [parsimony.asm.inference :as asm.inference]
            [parsimony.asm.parser :as asm.parser]
            [parsimony.console :as console]))

(def parser
  (parser/definition-parser
                 "E = x ;"
                 #{:x :y :z}))

(defn gen-input [string sample-id constraints]
  (let [tokens (lexer/lex xyz-lexer string)
        {:keys [cyk codec]} (inference/run-parser-unconstrained parser tokens)
        cljs-constraint-states
        (into []
              (map (partial inference/gen-one-constraint-state codec cyk tokens parser constraints sample-id))
              (:positive constraints))
        extended-codec (asm.inference/extend-codec cljs-constraint-states xyz-lexer codec)]
    (asm.parser/cpp-free cyk)
    {:lexer xyz-lexer
     :parser parser
     :string string
     :cyk cyk
     :codec codec
     :extended-codec extended-codec
     :constraints constraints
     :cljs-constraint-states cljs-constraint-states
     :cpp-constraint-states (into []
                                  (map #(asm.inference/cpp-init-constraint % extended-codec))
                                  cljs-constraint-states)}))

(def input-1 (gen-input "xyxyxzx" 0
                            {:positive #{[:E 0 7]
                                         [:E 0 5]
                                         [:E 0 3]
                                         [:E 0 1]}}))

(def input-2 (gen-input "xyxyx" 1
                            {:positive #{[:E 0 5]
                                         [:E 2 3]
                                         [:E 0 1]
                                         [:E 2 1]}}))

(deftest extend-codec-1
  (let [{:keys [extended-codec]} input-1]
    (console/debug ::extend-codec-1 {:extended-codec extended-codec})
    (is (= 3 (-> (set (keys (:encode extended-codec)))
                 (set/intersection #{:%x :%y :%z})
                 (count))))))

(deftest intersect-0
  (testing "basic intersection"
    (let [{:keys [extended-codec]} input-1

          ;; cpp solution
          cpp-ci (asm.inference/intersect-constraint-states
                   (get-in input-1 [:cpp-constraint-states 1])
                   (get-in input-1 [:cpp-constraint-states 3]))
          cpp-solution (asm.inference/solve-shortest-non-unit cpp-ci)
          decoded-cpp-solution (asm.inference/decode-solution-compressed-path cpp-solution extended-codec)

          ;; cljs solution
          cljs-ci (inference/intersect-constraint-states
                    (get-in input-1 [:cljs-constraint-states 1])
                    (get-in input-1 [:cljs-constraint-states 3]))
          cljs-solution (inference/solve-shortest-non-unit cljs-ci)]
      (asm.inference/cpp-free cpp-ci)
      (asm.inference/cpp-free cpp-solution)
      (console/debug ::test-0 {:cljs-solution cljs-solution :cpp-solution decoded-cpp-solution})
      (is (= decoded-cpp-solution (first (keys cljs-solution)))))))

(deftest cpp->cljs-constraint-1
  (let [{:keys [extended-codec]} input-1]
    (doseq [[cpp-c cljs-c] (map vector
                                (:cpp-constraint-states input-1)
                                (:cljs-constraint-states input-1))]
      (is (= cljs-c (asm.inference/cpp->cljs-constraint cpp-c extended-codec))))))

(deftest cpp-learn-partitions-1
  (let [{:keys [extended-codec]} input-1
        cpp-input-cs (into (:cpp-constraint-states input-1) (:cpp-constraint-states input-2))
        cljs-input-cs (into (:cljs-constraint-states input-1) (:cljs-constraint-states input-2))
        cpp-cs (asm.inference/cpp-learn-partitions cpp-input-cs extended-codec)
        cljs-cs (inference/learn-partitions cljs-input-cs)]
    (is (= (count cpp-cs) (count cljs-cs)))
    (let [cpp-partition-map
          (into {}
                (map
                  (fn [c]
                    (let [soln (asm.inference/solve-shortest-non-unit c)
                          decoded (asm.inference/decode-solution-compressed-path soln extended-codec)]
                      (asm.inference/cpp-free soln)
                      (vector (asm.inference/provenance c extended-codec)
                              decoded))))
                cpp-cs)
          cljs-partition-map
          (into {}
                (map
                  (fn [c]
                    (let [soln (inference/solve-shortest-non-unit c)]
                      (vector (:provenance c) soln))))
                cljs-cs)]
      (console/debug ::cpp-learn-partitions-1 {:cpp cpp-partition-map :cljs cljs-partition-map})
      (is (= (set (keys cpp-partition-map)) (set (keys cljs-partition-map))))
      (doseq [provenance (keys cpp-partition-map)]
        (is (= (get cpp-partition-map provenance)
               (first (keys (get cljs-partition-map provenance))))))
      (doseq [c cpp-cs]
        (try
          (asm.inference/cpp-free c)
          (catch js/Error _
            (console/warn ::cpp-free "already freed")))))))

(deftest learn-and-solve-partitions-1
  (let [cs (into (:cljs-constraint-states input-1)
                 (:cljs-constraint-states input-2))
        result (asm.inference/learn-and-solve-partitions cs xyz-lexer (:codec input-1))
        result-map (into {}
                         (map (fn [{:keys [provenance path]}]
                                (vector provenance path)))
                         result)]
    (console/debug ::learn-and-solve-partitions-1 {:result-map result-map})
    (is (= {[[0 [:E 0 7]]] [#{:E} #{:%z} #{:E :%x}],
            [[1 [:E 0 1]] [0 [:E 0 1]] [1 [:E 2 1]]] [#{:%x}],
            [[1 [:E 0 5]] [1 [:E 2 3]] [0 [:E 0 5]] [0 [:E 0 3]]]
            [#{:E} #{:%y} #{:E}]}
           result-map))))


