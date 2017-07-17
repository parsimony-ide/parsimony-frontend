(ns parsimony.refactor-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [cljs.pprint :refer [pprint]]
            [clojure.set :as set]
            [parsimony.parser :as parser]
            [parsimony.refactor.parser :as refactor.parser]
            [parsimony.union-find :as uf]))

(def grammar-1
  "e = e plus e {left} ;
   e = e minus e ;
   e = e times e ;
   e = e exp e ;

   left {
     e = e plus e ;
     e = e minus e ;
   }

   right {
     e = e exp e ;
   }

   priorities {
     e = e times e > e = e minus e ;
     e = e exp e > e = e times e ;
   }")

(def grammar-2
  "e = e plus e ;
   e = e minus e ;
   e = e times e ;
   e = e exp e ;")

(def tokens [:plus :minus :times :div :exp])

(def p-div [:e [:e :%div :e]])
(def p-exp [:e [:e :%exp :e]])
(def p-plus [:e [:e :%plus :e]])
(def p-minus [:e [:e :%minus :e]])
(def p-times [:e [:e :%times :e]])

(defn orig-productions [parser]
  (get-in parser [:orig :productions]))

(defn orig-attributes [parser]
  (get-in parser [:orig :attributes]))

(defn orig-priorities [parser]
  (get-in parser [:orig :priorities]))

(defn orig-associativities [parser]
  (get-in parser [:orig :associativities]))

(deftest refactor-parser-1
  (let [parser (parser/definition-parser grammar-1 tokens)]
    (testing "add-productions"
      (let [parser' (refactor.parser/add-productions parser [p-div p-exp])]
        (is (= 5 (count (orig-productions parser'))))
        (is (set/subset? (set (orig-productions parser))
                         (set (orig-productions parser'))))
        (is (set/subset? #{p-div p-exp} (set (orig-productions parser'))))))
    (testing "remove-productions"
      (let [parser' (refactor.parser/remove-productions parser [p-plus])]
        (is (= 3 (count (orig-productions parser'))))
        (is (set/subset? (set (orig-productions parser'))
                         (set (orig-productions parser))))
        (is (not (set/subset? #{p-plus} (set (orig-productions parser')))))))
    (testing "add-attributes"
      (let [parser' (refactor.parser/add-attributes parser p-minus [:prefer])]
        (is (:prefer (get (orig-attributes parser') p-minus)))))
    (testing "remove-attributes"
      (let [parser' (refactor.parser/remove-attributes parser [p-plus])]
        (is (not (contains? (orig-attributes parser') p-plus)))))
    (testing "remove-priorities-1"
      (let [parser' (refactor.parser/remove-priorities parser [p-minus])]
        (is (not (contains? (set (orig-priorities parser'))
                            {:high p-times :low p-minus})))))
    (testing "remove-priorities-2"
      (let [parser' (refactor.parser/remove-priorities parser [p-times])]
        (is (zero? (count (orig-priorities parser'))))))
    (testing "remove-block-associativities"
      (let [parser' (refactor.parser/remove-block-associativities parser [p-plus])]
        (is (= (orig-associativities parser')
               [[:left [p-minus]] [:right [p-exp]]])))
      (let [parser' (refactor.parser/remove-block-associativities parser [p-exp])]
        (is (= (orig-associativities parser')
               [[:left [p-plus p-minus]]])))
      (let [parser' (refactor.parser/remove-block-associativities parser [p-plus p-minus p-exp])]
        (is (= (orig-associativities parser')
               []))))))

(deftest refactor-parser-2
  (let [parser (parser/definition-parser grammar-2 tokens)]
    (testing "add-block-associativity"
      (let [parser' (refactor.parser/add-block-associativity parser [p-plus p-minus] :left)]
        (is (= (orig-associativities parser')
               [[:left [p-plus p-minus]]]))))))

(deftest emit-pragma-1
  (let [g1 "{-# noop-pragma #-}"
        s1 (refactor.parser/reformat (parser/definition-parser g1))
        g2 "{-# noop-pragma start=E #-}"
        s2 (refactor.parser/reformat (parser/definition-parser g2 nil :skip-checks true))]
    (is (= g1 s1))
    (is (= g2 s2))))
