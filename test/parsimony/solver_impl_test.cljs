(ns parsimony.solver-impl-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [parsimony.common-test :refer [aab-lexer csharp-lexer]]
            [parsimony.parser :as parser]
            [parsimony.models.parse-dashboard :as parse-dashboard]
            [parsimony.solver-impl :as solver-impl]
            [parsimony.console :as console]))

(defn- dummy-sample [id s]
  (parse-dashboard/sample id id (str "dummy-" id) s []))

(defn- add-label [{:keys [labels] :as sample} nt char-from char-to type]
  (let [next-label-id ((fnil inc 0) (last (sort (map :label-id labels))))
        label {:nt nt
               :char-from char-from
               :char-to char-to
               :type type
               :label-id next-label-id}]
    (update sample :labels conj label)))

(defn setup-state [lexer grammar-str samples]
  (let [token-keys (into [] (map first) lexer)
        parser (parser/definition-parser grammar-str token-keys)
        parse-dashboard (reduce
                          #(parse-dashboard/add-sample %1 %2)
                          parse-dashboard/default-model
                          samples)
        state (solver-impl/init solver-impl/default-model
                                lexer
                                parser
                                parse-dashboard)]
    (solver-impl/populate-sample-cache state)))

(deftest test-0
  (let [sample-1 (-> (dummy-sample 1 "a b")
                     (add-label :S 0 3 :positive))
        sample-2 (-> (dummy-sample 2 "x y")
                     (add-label :S 0 3 :positive))
        sample-3 (-> (dummy-sample 3 "a,b")
                     (add-label :S 0 3 :positive))
        sample-4 (-> (dummy-sample 4 "[a,b,c,d,e,f]")
                     (add-label :S 0 13 :positive))
        sample-5 (-> (dummy-sample 5 "[x y]")
                     (add-label :S 0 5 :positive))
        sample-6 (-> (dummy-sample 6 "[x]")
                     (add-label :S 0 3 :positive))
        sample-7 (-> (dummy-sample 7 "[x,y] [a,b,c]")
                     (add-label :S 0 13 :positive))
        state (-> (setup-state
                    csharp-lexer
                    "S = ident ;"
                    [sample-1 sample-2 sample-3 sample-4 sample-5 sample-6 sample-7])
                  (solver-impl/gen-constraint-states)
                  (solver-impl/partition-constraint-states)
                  (solver-impl/run-heuristics))]
    (console/debug ::test-0 (-> (dissoc state :parser :lexer)
                                (update :unpartitioned-constraint-states
                                        (partial map #(dissoc % :table)))
                                (update :partitioned-constraint-states
                                        (partial map #(dissoc % :table)))
                                (update :heuristics (partial sort-by :provenance))))
    (solver-impl/reset-sample-cache state)))

(deftest test-1
  ;; This regression test deals with the "statement-list" bug: when an
  ;; undelimited-list heuristic infers production "statement-list = statement"
  ;; due to nested labels for "statement", the subsequent solutions for
  ;; "statement" become "statement = statement-list", leading to a production
  ;; cycle that cannot be broken.
  ;;
  ;; This test encodes a distillation of that broken mechanism.
  (let [sample-1 (-> (dummy-sample 1 "abba")
                     (add-label :elem-list 0 4 :positive)
                     (add-label :elem 0 2 :positive)
                     (add-label :elem 2 4 :positive))
        state (-> (setup-state
                    aab-lexer
                    "ab-elem = a b ;
                     ba-elem = b a ;
                     elem-list = elem ;
                     elem-list = elem elem-list ;
                     elem = w ;"
                    [sample-1])
                  (solver-impl/gen-constraint-states)
                  (solver-impl/solve-partitions)
                  (solver-impl/recompile-multiparser)
                  (solver-impl/recompute-forests)
                  (solver-impl/gen-constraint-states)
                  (solver-impl/solve-partitions))
        solutions (:solutions state)]
    #_(console/debug ::test-1 {:solutions solutions})
    (is (= 3 (count solutions)))
    (let [{:keys [candidates] :as elem-list-0-2}
          (first (filter #(= [[1 [:elem 0 2]]] (:provenance %)) solutions))]
      (is (= 2 (count candidates)))
      (is (= 1 (count (:path (first candidates)))))
      (is (= [#{:ab-elem}] (:path (first candidates))))
      (is (= 2 (count (:path (second candidates)))))
      (is (= [#{:%a} #{:%b}] (:path (second candidates)))))
    (let [{:keys [candidates] :as elem-list-2-2}
          (first (filter #(= [[1 [:elem 2 2]]] (:provenance %)) solutions))]
      (is (= 2 (count candidates)))
      (is (= 1 (count (:path (first candidates)))))
      (is (= [#{:ba-elem}] (:path (first candidates))))
      (is (= 2 (count (:path (second candidates)))))
      (is (= [#{:%b} #{:%a}] (:path (second candidates)))))))
