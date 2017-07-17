(ns parsimony.overlay-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [parsimony.views.neo-overlay :refer [rowindices char-ranges->regions]]))

(deftest rowindices-1
  (is (= [] (rowindices "")))
  (is (= [[0 1]] (rowindices "a")))
  (is (= [[0 2] [2 3]] (rowindices "a\nb"))))

(deftest char-ranges->regions-1
  (is (= [] (char-ranges->regions [] [])))
  (let [s "hello\nworld"
        rs (rowindices s)]
    (is (= [[0 6] [6 11]] rs))
    (is (= [[[[0 0] [0 3]]]] (char-ranges->regions rs [[0 3 nil]])))
    (is (= [[[[0 0] [0 4]]]] (char-ranges->regions rs [[0 4 nil]])))
    (is (= [[[[0 0] [0 6]]]] (char-ranges->regions rs [[0 6 nil]])))
    (is (= [[[[0 0] [0 6]]
             [[1 0] [1 5]]]] (char-ranges->regions rs [[0 11 nil]])))
    (is (= [[[[0 0] [0 6]]]
            [[[1 0] [1 5]]]] (char-ranges->regions rs [[0 6 nil]
                                                       [6 11 nil]])))
    (is (= [[[[0 0] [0 3]]]
            [[[0 3] [0 6]]
             [[1 0] [1 2]]]
            [[[1 2] [1 5]]]] (char-ranges->regions rs [[0 3 nil]
                                                       [3 8 nil]
                                                       [8 11 nil]])))))

