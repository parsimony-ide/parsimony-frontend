(ns parsimony.parser-performance-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [goog.string]
            [parsimony.common-test :refer [forests]]
            [parsimony.util :refer-macros [inspect inspect-pp with-inspection without-inspection]]
            [parsimony.parser :as parser]))

(deftest performance-bug-1
  (testing "associativity performance"
    (let [token-vec [:x :a
                     :x :a
                     :x :a
                     :x :a
                     :x :a
                     :x :a
                     :x]
          parser-left (parser/definition-parser
                        "E = E a E {left} ;
                         E = x ;"
                        #{:x :a})
          parser-right (parser/definition-parser
                         "E = E a E {right} ;
                          E = x ;"
                         #{:x :a}) ]
      (simple-benchmark [parser parser-left] (forests parser token-vec :E 0 13) 1)
      (simple-benchmark [parser parser-right] (forests parser token-vec :E 0 13) 1)))

  (testing "priority performance"
    (let [token-vec [:x :a
                     :x :b
                     :x :a
                     :x :b
                     :x :a
                     :x :b
                     :x]
          parser (parser/definition-parser
                   "E = E a E ;
                    E = E b E ;
                    E = x ;
                    priorities {
                    E = E a E > E = E b E ;
                    }"
                   #{:x :a :b})]
      (simple-benchmark [parser parser] (forests parser token-vec :E 0 13) 1))))
