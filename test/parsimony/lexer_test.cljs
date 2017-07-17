(ns parsimony.lexer-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [cljs.pprint :refer [pprint]]
            [parsimony.lexer :as lexer]))

(def lex-infos
  [[:ws
    {:source-str "ws = [ ] ;",
     :regex-str "[ ]",
     :js-automaton
     {:states
      {0
       {:transitions [{:min " ", :max " ", :to 1}],
        :id 0,
        :accept false},
       1 {:transitions [], :id 1, :accept true}},
      :initial 0}}]
   [:abc
    {:source-str "abc = (a | b | c)+ ;",
     :regex-str "(a|b|c)+",
     :js-automaton
     {:states
      {0
       {:transitions [{:min "a", :max "c", :to 1}],
        :id 0,
        :accept false},
       1
       {:transitions [{:min "a", :max "c", :to 1}],
        :id 1,
        :accept true}},
      :initial 0}}]])

(deftest abc-1
  (is (= '(:abc) (map :label (lexer/lex lex-infos "abc"))))
  (is (= '(:abc) (map :label (lexer/lex lex-infos "abc "))))
  (is (= '(:abc :ws) (map :label (lexer/lex lex-infos "abc " nil))))
  (is (= {:reason :no-matching-token
          :fail-idx 4}
         (last (lexer/lex lex-infos "abc d")))))

(deftest discard-whitespace-1
  (let [tokens (lexer/lex lex-infos "a b c")]
    (is (= 3 (count tokens)))
    (is (not (contains? (set (map :label tokens)) :ws))))
  (let [tokens-with-ws (lexer/lex lex-infos "a b c" nil)]
    (is (= 5 (count tokens-with-ws)))
    (is (contains? (set (map :label tokens-with-ws)) :ws))))

(deftest char-range->token-range-1
  (let [tokens (lexer/lex lex-infos "abc  abc  " #{:ws})]
    (doseq [start (range 0 10)
            end (range (inc start) 11)]
      (let [expected (case [start end]
                       [0 3] [0 1]
                       [5 8] [1 1]
                       [0 8] [0 2]
                       nil)]
        (is (= expected (lexer/char-range->token-range tokens start end)))))))

(deftest inexact-char-range->token-range-1
  (let [tokens (lexer/lex lex-infos "abc  abc  " #{:ws})]
    (doseq [start (range 0 10)
            end (range (inc start) 11)]
      (let [expected (case [start end]
                       ([0 1] [0 2] [0 3] [0 4] [0 5]
                        [1 2] [1 3] [1 4] [1 5]
                        [2 3] [2 4] [2 5])
                       [0 1]

                       ([0 6] [0 7] [0 8] [0 9] [0 10]
                        [1 6] [1 7] [1 8] [1 9] [1 10]
                        [2 6] [2 7] [2 8] [2 9] [2 10])
                       [0 2]

                       ([3 6] [3 7] [3 8] [3 9] [3 10]
                        [4 6] [4 7] [4 8] [4 9] [4 10]
                        [5 6] [5 7] [5 8] [5 9] [5 10]
                        [6 7] [6 8] [6 9] [6 10]
                        [7 8] [7 9] [7 10])
                       [1 1]

                       ;; default
                       [0 2])]
        (is (= expected (lexer/inexact-char-range->token-range tokens start end))
            (str [start end]))))))

(deftest token-range->char-range-1
  (let [tokens (lexer/lex lex-infos "abc  abc  " #{:ws})]
    (is (= [0 3] (lexer/token-range->char-range tokens 0 1)))
    (is (= [0 8] (lexer/token-range->char-range tokens 0 2)))
    (is (= [5 8] (lexer/token-range->char-range tokens 1 1)))
    (is (= nil   (lexer/token-range->char-range tokens 0 3)))
    (is (= nil   (lexer/token-range->char-range tokens 1 2)))))

(deftest all-syms-1
  (is (= [:abc :ws] (lexer/all-syms lex-infos))))
