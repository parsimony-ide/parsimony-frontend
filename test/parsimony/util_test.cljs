(ns parsimony.util-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [cljs.pprint :refer [pprint]]
            [parsimony.util :refer [deep-merge deep-select-keys]]))

(def m {:a {:b 1
            :c {:d [4]}
            :f {:d nil}}
        :d {:e #{:foo}}})

(def m' {:a {:b 2
             :e 3}})

(deftest deep-merge-1
  (is (= (-> m
             (assoc-in [:a :b] 2)
             (assoc-in [:a :e] 3))
         (deep-merge m m')))
  (is (= m (deep-merge nil m))))

(deftest deep-select-keys-1
  (is (= {:a {:b 1}
          :d {:e #{:foo}}}
         (deep-select-keys m [[:a :b]
                              [:d]])))
  (is (= {:d {:e #{:foo}}}
         (deep-select-keys m [[:d :e]])))
  (is (= {:a {:c {:d [4]}
              :f {:d nil}}}
         (deep-select-keys m [[:a :* :d]])))
  (is (= {} (deep-select-keys m nil)))
  (is (= {} (deep-select-keys m []))))
