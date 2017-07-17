(ns parsimony.union-find-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [parsimony.union-find :as uf]))

(deftest test-1
  (let [u1 (uf/add (uf/new-union-find) :a)
        u2 (uf/add u1 :b)
        u3 (uf/union u2 :a :b)
        u4 (uf/add u3 :c)
        u5 (uf/union u4 :a :c)
        u6 (uf/add-equivalence-class (uf/new-union-find) :a :b :c :d)
        u7 (uf/add-equivalence-class u2 :c :d)
        u8 (uf/add-equivalence-class u4 :c :d)
        u9 (uf/add-equivalence-class u5 :c :d)]
    (is (uf/find u1 :a))
    (is (= (uf/find u1 :a) (uf/find u1 :a)))
    (is (not= (uf/find u2 :a) (uf/find u2 :b)))
    (is (= (uf/find u3 :a) (uf/find u3 :b)))
    (is (not= (uf/find u4 :a) (uf/find u4 :c)))
    (is (= (uf/find u5 :a) (uf/find u5 :c)))
    (is (= (uf/find u5 :b) (uf/find u5 :c)))

    (is (= #{#{:a}} (uf/->sets u1)))
    (is (= #{#{:a} #{:b}} (uf/->sets u2)))
    (is (= #{#{:a :b}} (uf/->sets u3)))
    (is (= #{#{:a :b} #{:c}} (uf/->sets u4)))
    (is (= #{#{:a :b :c}} (uf/->sets u5)))
    (is (= #{#{:a :b :c :d}} (uf/->sets u6)))
    (is (= #{#{:a} #{:b} #{:c :d}} (uf/->sets u7)))
    (is (= #{#{:a :b} #{:c :d}} (uf/->sets u8)))
    (is (= #{#{:a :b :c :d}} (uf/->sets u9)))

    (is (= #{#{:a}} (uf/->sets (uf/combine u1 u1))))
    (is (= #{#{:a} #{:b}} (uf/->sets (uf/combine u1 u2))))
    (is (= #{#{:a :b}} (uf/->sets (uf/combine u2 u3))))
    (is (= #{#{:a :b} #{:c}} (uf/->sets (uf/combine u3 u4))))
    (is (= #{#{:a :b :c}} (uf/->sets (uf/combine u4 u5))))
    (is (= #{#{:a :b} #{:c :d}} (uf/->sets (uf/combine u1 u8))))

    (is (= #{:a} (uf/nodes u1)))
    (is (= #{:a :b}
           (uf/nodes u2)
           (uf/nodes u3)))
    (is (= #{:a :b :c}
           (uf/nodes u4)
           (uf/nodes u5)))
    (is (= #{:a :b :c :d}
           (uf/nodes u6)
           (uf/nodes u7)
           (uf/nodes u8)
           (uf/nodes u9)))))
