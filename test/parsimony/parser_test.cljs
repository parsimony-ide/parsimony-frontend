(ns parsimony.parser-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [cljs.pprint :refer [pprint]]
            [clojure.set :as set]
            [clojure.string :as str]
            [goog.string :as gstring]
            [goog.string.format]
            [parsimony.common-test :refer [forests xyz-lexer]]
            [parsimony.dag :as dag]
            [parsimony.union-find :as uf]
            [parsimony.util :refer [pprint-str] :refer-macros [inspect inspect-pp with-inspection without-inspection]]
            [parsimony.parser :as parser]
            [parsimony.parser-recovery :as parser-recovery]
            [parsimony.lexer :as lexer]
            [parsimony.console :as console]))

(def basic-definition
  "
  ;; this is comment
  A = B C D ;
  A = C D ;
  A = X Y Z ;
  B = A? ;
  ident-with-dash = Foo Bar ;
  ;; aoeu
  ")

(def elim-1
  "
  A = B? ;
  C = A ;
  D = A E ;
  ")

(def numbers-1
  (parser/definition-parser
    "
    Number = Integer ;
    Number = Real ;
    Integer = digit ;
    Integer = Integer digit ;
    Real = Integer Fraction Scale? ;
    Fraction = dot Integer ;
    Scale = e sign Integer ;
    "
    #{:digit :dot :e :sign}))

(def numbers-2
  (parser/definition-parser
    "
    Number = Integer ;
    Integer = digit ;
    Integer = Integer digit ;
    "
    #{:digit}))

(def numbers-3
  (parser/definition-parser
    "
    Number = Integer ;
    Number = Real ;
    Integer = digit ;
    Integer = Integer digit ;
    Real = Integer dot Integer ;
    "
    #{:digit :dot}))

(def numbers-lex-infos
[[:digit {:source-str "digit = [0-9];", :regex-str "[0-9]", :js-automaton {:states {0 {:transitions [], :id 0, :accept true}, 1 {:transitions [{:min "0", :max "9", :to 0}], :id 1, :accept false}}, :initial 1}}] [:e {:source-str "\ne = e;", :regex-str "e", :js-automaton {:states {0 {:transitions [], :id 0, :accept true}, 1 {:transitions [{:min "e", :max "e", :to 0}], :id 1, :accept false}}, :initial 1}}] [:dot {:source-str "\ndot = \\.;", :regex-str "\\.", :js-automaton {:states {0 {:transitions [{:min ".", :max ".", :to 1}], :id 0, :accept false}, 1 {:transitions [], :id 1, :accept true}}, :initial 0}}] [:sign {:source-str "\nsign = \\-;", :regex-str "\\-", :js-automaton {:states {0 {:transitions [{:min "-", :max "-", :to 1}], :id 0, :accept false}, 1 {:transitions [], :id 1, :accept true}}, :initial 0}}] [:ws {:source-str "\nws = [ \\n\\r\\t]+;", :regex-str "[ \n\r\t]+", :js-automaton {:states {0 {:transitions [{:min "\t", :max "\n", :to 1} {:min " ", :max " ", :to 1} {:min "\r", :max "\r", :to 1}], :id 0, :accept false}, 1 {:transitions [{:min "\t", :max "\n", :to 1} {:min " ", :max " ", :to 1} {:min "\r", :max "\r", :to 1}], :id 1, :accept true}}, :initial 0}}]])

(def cyclic-1
  (parser/definition-parser "A = A ;" nil :skip-checks true))

(def cyclic-2
  (parser/definition-parser "A = B ; B = A ;" nil :skip-checks true))

(def cyclic-3
  (parser/definition-parser "A = B ; B = A ; C = foo ;" #{:foo} :skip-checks true))

(def abc-1
  (parser/definition-parser "S = a b c ;" #{:a :b :c}))

(def epsilon-1
  (parser/definition-parser "S = a? ;" #{:a}))

(def epsilon-2
  (parser/definition-parser "S = A ; A = a? ;" #{:a}))

(def ambig-1
  (parser/definition-parser "S = A B ; A = a ; A = a a ; B = b ; B = a b ;" #{:a :b}))

(def ambig-2
  (parser/definition-parser "S = A B ; T = A B ; U = A B ; A = a ; A = a a ; B = b ; B = a b ;" #{:a :b}))

(defn verbose-emit [ps message]
  (println "================================================================================")
  (println message)
  (println "================================================================================")
  (println (parser/emit (parser/sort-by-nt ps)))
  #_(println "raw =" ps)
  ps)

(defn verbose-pipeline [ps]
  (-> ps
      (verbose-emit "orig")
      (parser/expand-optionals)
      (verbose-emit "expand-optionals")
      (parser/elim-epsilon-rules)
      (verbose-emit "elim-epsilon-rules")
      (parser/elim-unit-rules)
      (verbose-emit "elim-unit-rules")
      (parser/add-terminal-rules)
      (verbose-emit "add-terminal-rules")
      (parser/binarize-rules)
      (verbose-emit "binarize-rules")))

(deftest basic-1
  (let [asts (:productions (parser/definition-parser basic-definition nil :skip-checks true))
        elim-1-ps (:productions (parser/definition-parser elim-1 nil :skip-checks true))]
    (is (= 5 (count asts)))
    (is (= :A (parser/lhs (first asts))))
    (is (= 3 (count (parser/productions-with-lhs asts :A))))
    (is (= 1 (count (parser/productions-with-lhs asts :ident-with-dash))))
    (is (= 2 (count (parser/productions-with-rhs asts :D))))
    (is (= 1 (count (parser/productions-with-rhs asts :Z))))
    (is (= 0 (count (parser/productions-with-rhs asts :missing))))
    (is (-> asts
            (parser/productions-with-lhs :B)
            (first)
            (parser/rhs)
            (first)
            (parser/optional?)))

    (is (= [:A :B :C] (parser/rhs-nts [[:X [:A]]
                                       [:Y [:B :C]]])))

    (is (parser/optional? :A?))
    (is (not (parser/optional? :A)))
    (is (parser/terminal? :%A))
    (is (parser/binarized? :A:2))
    (is (= :A (parser/->required :A?)))
    (is (= :A (parser/->required :A)))
    (is (= :A (parser/->nonterminal :%A)))
    (is (= :A? (parser/->nonterminal :%A?)))
    (is (= :*a (parser/->pseudo-terminal :%a)))
    (is (= :A (parser/->pseudo-terminal :A)))
    (is (= :A (parser/->required (parser/->nonterminal :%A?))))
    (is (parser/prime? :A'))
    (is (not (parser/prime? :A)))
    (is (= :A' (parser/->prime :A)))
    (is (= :A'' (parser/->prime :A')))
    (is (= :A (parser/->unprime :A')))
    (is (= :A (parser/->unprime :A')))
    (is (= :A' (parser/->unprime :A'')))
    (is (= :A (parser/->unprime :A)))

    (is (= 2 (count (parser/expand-optional-production [:A [:B?]]))))
    (is (= 2 (count (parser/expand-optional-production [:A [:B? :C]]))))
    (is (= 4 (count (parser/expand-optional-production [:A [:B? :C?]]))))
    (is (= 4 (count (parser/expand-optional-production [:A [:B? :C? :D :E]]))))
    (is (contains?
         (set (parser/expand-optional-production [:A [:B? :C? :D :E]]))
         [:A [:C :D :E]]))

    (is (= 6 (count (parser/expand-optionals asts))))

    (is (= [[:A []]] (parser/epsilon-rules [[:A []]
                                            [:B [:B]]])))

    (is (= [[:B [:B]]] (parser/non-epsilon-rules [[:A []]
                                                  [:B [:B]]])))

    (is (parser/unit-self-cyclic-rule? [:A [:A]]))
    (is (not (parser/unit-self-cyclic-rule? [:A [:B]])))
    (is (not (parser/unit-self-cyclic-rule? [:A [:A :B]])))


    (is (= [[:A [:A]]
            [:B [:A]]] (parser/unit-rules [[:A [:A]]
                                           [:A []]
                                           [:B [:A]]
                                           [:B [:A :B :C]]])))

    (is (parser/has-terminal? [:A [:%B]]))
    (is (parser/has-terminal? [:A [:%B :C :D]]))
    (is (parser/has-terminal? [:A [:B :%C :D]]))
    (is (not (parser/has-terminal? [:A [:B]])))
    (is (not (parser/has-terminal? [:A []])))

    (is (= [:A [:C]] (parser/delete-nts-from-production [:A [:B :C :B]] #{:B})))

    (is (= [[:A [:C]]] (parser/remove-production [[:A [:B]]
                                                  [:A [:C]]]
                                                 [:A [:B]])))

    (is (= [[:A [:C]]] (parser/remove-productions [[:A [:B]]
                                                   [:A [:C]]
                                                   [:A [:D]]]
                                                  [[:A [:B]]
                                                   [:A [:D]]])))

    (is (= [[:C []]] (parser/remove-productions-by-lhs [[:A [:A]]
                                                        [:A [:B]]
                                                        [:C []]] :A)))

    (= "a " (parser/str-pad "a" 2))
    (= "a  " (parser/str-pad "a" 3))
    (= "abcd" (parser/str-pad "abcd" 1))

    (is (= 2 (count (parser/elim-rules [:A [:B]] :B))))
    (is (= 1 (count (parser/elim-rules [:A [:B :C :D :C]] :missing))))
    (is (= 4 (count (parser/elim-rules [:A [:B :C :D :C]] :C))))

    ;; elim

    (is (= [[:A []]] (parser/elim-epsilon-rules-3 [[:A []]
                                                   [:B [:A']]])))


    #_(let [expand-optionals (parser/expand-optionals elim-1-ps)]
        (println "expand-optionals =")
        (pprint expand-optionals)
        (let [elim-epsilon-rules-1 (parser/elim-epsilon-rules-1 expand-optionals)]
          (println "elim-epsilon-rules-1 =")
          (pprint elim-epsilon-rules-1)
          (let [elim-epsilon-rules-2 (parser/elim-epsilon-rules-2 elim-epsilon-rules-1)]
            (println "elim-epsilon-rules-2 =")
            (pprint elim-epsilon-rules-2)
            (let [elim-epsilon-rules-3 (parser/elim-epsilon-rules-3 elim-epsilon-rules-2)]
              (println "elim-epsilon-rules-3 =")
              (pprint elim-epsilon-rules-3)
              (let [elim-epsilon-rules-4 (parser/elim-epsilon-rules-4 elim-epsilon-rules-3)]
                (println "elim-epsilon-rules-4 =")
                (pprint elim-epsilon-rules-4)
                (let [elim-epsilon-rules-5 (parser/elim-epsilon-rules-5 elim-epsilon-rules-4)]
                  (println "elim-epsilon-rules-5 =")
                  (pprint elim-epsilon-rules-5)))))))

    (is (= [] (parser/elim-unit-rules [[:A [:B]]
                                       [:B [:C]]])))
    (is (= #{[:A [:C :D]]
             [:B [:C :D]]} (set (parser/elim-unit-rules [[:A [:B]]
                                                         [:B [:C :D]]]))))
    (is (= #{[:A [:*T :B]]
             [:*T [:%T]]}
           (set (parser/add-terminal-rules [[:A [:%T :B]]]))))
    (is (= [[:A [:%T]]]
           (parser/add-terminal-rules [[:A [:%T]]]))) ;; if already a unit rule, don't do anything to it

    (is (= #{[:A [:A:1 :D]]
             [:A:1 [:B :C]]}
           (set (parser/binarize-rules [[:A [:B :C :D]]]))))

    (is (= #{[:A [:A:2 :E]]
             [:A:2 [:A:1 :D]]
             [:A:1 [:B :C]]}
           (set (parser/binarize-rules [[:A [:B :C :D :E]]]))))

    (is (= [] (parser/->cnf (:productions cyclic-1))))
    (is (= [] (parser/->cnf (:productions cyclic-2))))
    (is (= [[:C [:%foo]]] (parser/->cnf (:productions cyclic-3))))
    (is (= #{[:S [:S:1 :*c]]
             [:S:1 [:*a :*b]]
             [:*a [:%a]]
             [:*b [:%b]]
             [:*c [:%c]]}
           (set (parser/->cnf (:productions abc-1)))))

    (is (= [[:S [:%a]]] (parser/->cnf (:productions epsilon-1))))

    #_(println (parser/emit (parser/->cnf numbers-1)))))

(deftest cyk-and-coloring-1
  (let [s "1.2e-3 42"
        parser numbers-1
        raw-tokens (lexer/lex numbers-lex-infos s nil)
        token-vec (into [] (map :label) raw-tokens)
        cyk-table (parser/cyk token-vec parser)
        coloring-table (parser/coloring cyk-table)
        forest (parser/reconstruct cyk-table token-vec parser :Number 0 6)
        max-coloring (parser/lookup-table coloring-table 0 (count s))
        min-coloring (parser/succinct-coloring cyk-table token-vec parser max-coloring)]
    #_(do
        (println (parser/emit (:productions parser)))
        (println (parser/pprint-cyk cyk-table s raw-tokens))
        (println (parser/pprint-cyk coloring-table s raw-tokens)))
    (without-inspection
      (inspect-pp forest)
      (inspect-pp max-coloring)
      (inspect-pp min-coloring))
    (is (= forest
           #{[[:Number 0 6] [[:Real 0 6]]]
             [[:Real 0 6] [[:Integer 0 1] [:Fraction 1 2] [:Scale 3 3]]]
             [[:Scale 3 3] [[:%e 3 1] [:%sign 4 1] [:Integer 5 1]]]
             [[:Integer 5 1] [[:%digit 5 1]]]
             [[:Fraction 1 2] [[:%dot 1 1] [:Integer 2 1]]]
             [[:Integer 2 1] [[:%digit 2 1]]]
             [[:Integer 0 1] [[:%digit 0 1]]]}))
    (is (= max-coloring #{[:Number 0 6] [:Real 0 6] [:Integer 7 2] [:Number 7 2]}))
    (is (set/subset? min-coloring max-coloring))
    (is (= min-coloring #{[:Number 0 6] [:Number 7 2]}))))

(deftest cyk-and-coloring-2
  (let [s "aab aab"
        parser ambig-2
        raw-tokens [{:label :a :start 0 :end 1}
                    {:label :a :start 1 :end 2}
                    {:label :b :start 2 :end 3}
                    {:label :w :start 3 :end 4}
                    {:label :a :start 4 :end 5}
                    {:label :a :start 5 :end 6}
                    {:label :b :start 6 :end 7}]
        token-vec (into [] (map :label) raw-tokens)
        cyk-table (parser/cyk token-vec parser)
        coloring-table (parser/coloring cyk-table)
        forest (parser/reconstruct cyk-table token-vec parser :S 0 3)
        max-coloring (parser/lookup-table coloring-table 0 (count s))
        min-coloring (parser/succinct-coloring cyk-table token-vec parser max-coloring)]
    #_(do
        (println (parser/emit (:productions parser)))
        (parser/pprint-cyk cyk-table s raw-tokens)
        (parser/pprint-cyk coloring-table s raw-tokens))
    (without-inspection
      (inspect-pp forest)
      (inspect-pp max-coloring)
      (inspect-pp min-coloring))
    (is (= forest
           #{[[:S 0 3] [[:A 0 1] [:B 1 2]]]
             [[:S 0 3] [[:A 0 2] [:B 2 1]]]
             [[:B 2 1] [[:%b 2 1]]]
             [[:A 0 2] [[:%a 0 1] [:%a 1 1]]]
             [[:B 1 2] [[:%a 1 1] [:%b 2 1]]]
             [[:A 0 1] [[:%a 0 1]]]}))
    (is (= max-coloring #{[:S 0 3] [:T 0 3] [:U 0 3] [:S 4 3] [:T 4 3] [:U 4 3]}))
    (is (= min-coloring #{[:S 0 3] [:T 0 3] [:U 0 3] [:S 4 3] [:T 4 3] [:U 4 3]}))))

(deftest cyk-and-coloring-3
  (let [s "a  a"
        raw-tokens [{:label :a :start 0 :end 1}
                    {:label :w :start 1 :end 3}
                    {:label :a :start 3 :end 4}]
        token-vec (into [] (map :label) raw-tokens)
        parser (parser/definition-parser "A = a ;" #{:a})
        cyk-table (parser/cyk token-vec parser)
        coloring-table (parser/coloring cyk-table)
        forest-1 (parser/reconstruct cyk-table token-vec parser :A 0 1)
        forest-2 (parser/reconstruct cyk-table token-vec parser :A 2 1)
        max-coloring (parser/lookup-table coloring-table 0 3)
        min-coloring (parser/succinct-coloring cyk-table token-vec parser max-coloring)]
    #_(do
        (println (parser/emit (:productions parser)))
        (parser/pprint-cyk cyk-table s raw-tokens)
        (parser/pprint-cyk coloring-table s raw-tokens) )
    (without-inspection
      (inspect-pp forest-1)
      (inspect-pp forest-2)
      (inspect-pp max-coloring)
      (inspect-pp min-coloring))
    (is (= forest-1 #{[[:A 0 1] [[:%a 0 1]]]}))
    (is (= forest-2 #{[[:A 2 1] [[:%a 2 1]]]}))
    (is (= max-coloring #{[:A 0 1] [:A 2 1]}))
    (is (= min-coloring #{[:A 0 1] [:A 2 1]}))))

(deftest coloring-bug-1
  ;; simplified version of a bug found during testing
  (let [token-vec [:using :ident :semi
                   :using :ident :dot :ident :dot :ident :semi]
        parser (parser/definition-parser
                 "Qual-ident = Qual-ident dot ident ;
                  Qual-ident = ident ;
                  Using = using Qual-ident ;
                  Expr = Qual-ident ;
                  Arg-list = Expr ;"
                 (set token-vec))
        cyk-table (parser/cyk token-vec parser)
        coloring-table (parser/coloring cyk-table)
        max-coloring (parser/lookup-table coloring-table 0 (count token-vec))
        min-coloring (parser/succinct-coloring cyk-table token-vec parser max-coloring)]
    (is (= #{[:Using 0 2]
             [:Using 3 6]}
           max-coloring
           min-coloring))))

(deftest overlapped-colorings-1
  ;; This is an example of a max-coloring in which two candidates have the same
  ;; score.  In this case, the coloring algorithm (see
  ;; parsimony.parser/color-fn) merges both candidates, which leads to
  ;; overlapped regions.
  (let [token-vec [:a :a :a]
        parser (parser/definition-parser
                 "Q = a ;
                  Q = a a ;"
                 #{:a})
        cyk-table (parser/cyk token-vec parser)
        coloring-table (parser/coloring cyk-table)
        max-coloring (parser/lookup-table coloring-table 0 (count token-vec))
        min-coloring (parser/succinct-coloring cyk-table token-vec parser max-coloring)]
    (console/debug ::coloring-bug-2
                   {:coloring-table coloring-table
                    :cyk-table cyk-table
                    :max-coloring max-coloring
                    :min-coloring min-coloring})))

(deftest source-map-1
  (without-inspection
    (let [{:keys [source-map] :as parser}
          (parser/definition-parser
            "E = E a E ;
             E = E b E ;
             E = E c E ;
             D = D a D {right} ;
             E = x ;
             left {
               E = E a E ;
             }
             priorities {
               E = E a E > E = E b E ;
               E = E b E > E = E c E ;
             }
             {-# noop-pragma #-}"
            #{:a :b :c :x}
            :skip-checks true
            :keep-source-map true)]

      (inspect-pp parser)

      (is (contains? (set (:ident source-map)) [:E {:span [0 1]}]))
      (is (contains? (set (:body source-map)) [:E {:span [4 9]}]))
      (is (contains? (set (:production source-map)) [[:E [:E :%a :E]] {:span [0 11]}]))
      (is (contains? (set (:attr source-map)) [:right {:span [86 91]}]))
      (is (contains? (set (:attr-block source-map)) [:right {:span [85 92]}]))

      (is (contains? (set (parser/associativity-declarations parser [:E [:E :%a :E]] :left))
                     [[:E [:E :%a :E]] {:span [151 160]}]))
      (is (contains? (set (parser/associativity-declarations parser [:D [:D :%a :D]] :right))
                     [:right {:span [86 91]}]))

      (is (contains? (set (parser/priority-declarations parser [:E [:E :%a :E]]))
                     [[:E [:E :%a :E]] {:span [219 242]}]))
      (is (contains? (set (:pragma-block source-map))
                     [:noop {:span [310 329]}]))
      (is (contains? (set (:pragma source-map))
                     [:noop {:span [314 325]}])))))

(deftest check-undefined-1
  (testing "undefined symbol checks"
    (let [f (fn [] (parser/definition-parser "S = A ;"))]
      (is (thrown-with-msg? js/Error
                            #"Symbol without definition"
                            (f)))
      (try (f)
           (catch js/Error e
             (let [{:keys [symbols instances]} (ex-data e)]
               (is (= [:A] symbols))
               (is (= [[:A {:span [4 5]}]] instances))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse Forest Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest parse-forest-utilities-1
  (let [forest #{[[:E 0 5] [[:E 0 3] [:%plus 3 1] [:E 4 1]]]
                 [[:E 4 1] [[:%number 4 1]]]
                 [[:E 0 1] [[:%number 0 1]]]
                 [[:E 2 3] [[:E 2 1] [:%plus 3 1] [:E 4 1]]]
                 [[:E 2 1] [[:%number 2 1]]]
                 [[:E 0 3] [[:E 0 1] [:%plus 1 1] [:E 2 1]]]
                 [[:E 0 5] [[:E 0 1] [:%plus 1 1] [:E 2 3]]]}]
    (is (= (parser/descendant-nodes forest [:E 0 5])
           (disj (parser/all-nodes forest) [:E 0 5])))
    (is (= (parser/descendant-nodes forest [:E 0 3])
           #{[:E 0 1]
             [:E 2 1]
             [:%plus 1 1]
             [:%number 0 1]
             [:%number 2 1]}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disambiguation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest check-assoc-1
  (testing "incompatibility associativity checks"
    (without-inspection
      (let [f (fn []
                (parser/definition-parser
                  "S = A ;
                   A = A a A {left} ;
                   A = a ;
                   right { A = A a A ; }"
                  #{:a}))
            f' (fn []
                 (parser/definition-parser
                   "S = A ;
                    A = A a A ;
                    B = B b B {left} ;
                    left { A = A a A ; B = B b B ; }
                    right { B = B b B ; }"
                   #{:a :b}))]
        (is (thrown-with-msg? js/Error #"Incompatible associativity" (f)))
        (try (f)
             (catch js/Error e
               (is (= [:A [:A :%a :A]] (:production (ex-data e))))
               (is (= 2 (count (:declarations (ex-data e)))))))

        (is (thrown-with-msg? js/Error #"Incompatible associativity" (f')))
        (try (f')
             (catch js/Error e
               (is (= [:B [:B :%b :B]] (:production (ex-data e))))
               (is (= 3 (count (:declarations (ex-data e)))))))))))

(deftest check-assoc-2
  (testing "undefined associativity production checks"
    (without-inspection
      (let [f (fn []
                (parser/definition-parser
                  "A = a ;
                   B = b ;
                   left { A = A a A ; }"
                  #{:a :b}))]
        (is (thrown-with-msg? js/Error #"Associativity declaration references previously undefined production" (f)))
        (try (f)
             (catch js/Error e
               (is (= 1 (count (:declarations (ex-data e)))))))))))

(deftest check-priority-1
  (testing "priority cycle checks"
    (without-inspection
      (let [f (fn []
                (parser/definition-parser
                  "E = E a E ;
                   E = E b E ;
                   E = x ;
                   priorities {
                   E = E a E > E = E b E ;
                   E = E b E > E = E a E ;
                   }"
                  #{:a :b :x}))]
        (is (thrown-with-msg? js/Error
                              #"Priority definition contains cycle"
                              (f)))
        (try (f)
             (catch js/Error e
               (let [{:keys [production declarations]} (ex-data e)]
                 (is (= [:E [:E :%a :E]] production))
                 (is (= 2 (count declarations))))))))))

(deftest check-priority-2
  (testing "undefined priority production checks"
    (without-inspection
      (let [f (fn []
                (parser/definition-parser
                  "A = a ;
                   priorities { A = A a A > A = A b A ; }"
                  #{:a :b}))]
        (is (thrown-with-msg? js/Error #"Priority declaration references previously undefined production" (f)))
        (try (f)
             (catch js/Error e
               (is (= 2 (count (:declarations (ex-data e)))))))))))

(deftest helpers-1
  (without-inspection
    (let [{:keys [productions cnf attributes] :as parser}
          (parser/definition-parser
            "S = A ;
             S = B ;
             S = C ;
             A = a ;
             B = a ;
             C = a ;"
            #{:a})
          {:keys [forest]} (forests parser [:a] :S 0 1)
          g (parser/forest->dag forest)]
      (is (parser/dummy-parent? g [:S 0 1]))
      (is (= #{[:S [:A]]
               [:S [:B]]
               [:S [:C]]}
             (parser/node-productions g [:S 0 1])))
      (is (= #{[:A [:%a]]}
             (parser/node-productions g [:A 0 1])))
      (is (= #{[:B [:%a]]}
             (parser/node-productions g [:B 0 1])))
      (is (= #{[:C [:%a]]}
             (parser/node-productions g [:C 0 1]))))))

(deftest helpers-2
  (without-inspection
    (let [{:keys [productions cnf attributes] :as parser}
          (parser/definition-parser
            "S = A ;
             S = B ;
             A = X ;
             A = Y ;
             B = a ;
             X = a ;
             Y = a ;"
            #{:a})
          {:keys [forest]} (forests parser [:a] :S 0 1)
          g (parser/forest->dag forest)]
      (inspect-pp "helpers-2" {:g g
                               :forest forest})
      (is (parser/dummy-parent? g [:S 0 1]))
      (is (parser/dummy-parent? g [:A 0 1]))
      (is (= #{[:S [:A]]
               [:S [:B]]}
             (parser/node-productions g [:S 0 1])))
      (is (= #{[:A [:X]]
               [:A [:Y]]}
             (parser/node-productions g [:A 0 1])))
      (is (= #{[:B [:%a]]}
             (parser/node-productions g [:B 0 1])))
      )))

(deftest helpers-3
  (without-inspection
    (let [{:keys [productions cnf attributes] :as parser}
          (parser/definition-parser
            "S = A ;
             S = B ;
             A = a ;
             B = a ;"
            #{:a})
          {:keys [forest]} (forests parser [:a] :S 0 1)
          g (parser/forest->dag forest)
          g' (dag/remove-edge g [:S 0 1] [:S 0 1 0])
          g'' (parser/remove-orphan-nodes g' (dag/roots g))
          g''' (parser/remove-redundant-dummies g'')]
      (inspect-pp "helpers-3" {:g g :g' g' :g'' g'' :g''' g'''
                               :forest forest
                               :forest' (parser/dag->forest g')
                               :forest'' (parser/dag->forest g'')
                               :forest''' (parser/dag->forest g''')
                               })
      (is (parser/dummy-parent? g [:S 0 1]))
      (is (= 2 (count (dag/successors g [:S 0 1]))))
      (is (= 1 (count (dag/successors g' [:S 0 1]))))
      (is (= #{[:B 0 1]} (dag/successors g''' [:S 0 1]))))))

(deftest resolve-prefer-1
  (without-inspection
    (let [{:keys [productions cnf attributes] :as parser}
          (parser/definition-parser
            "S = A {prefer} ;
             S = B ;
             A = a ;
             B = a ;"
            #{:a})]
      (is (= (get attributes [:S [:A]]) #{:prefer}))

      (let [{:keys [forest forest']} (forests parser [:a] :S 0 1)
            g (parser/forest->dag forest)
            g' (parser/forest->dag forest')]
        (is (parser/dummy-parent? g [:S 0 1]))
        (is (not (parser/dummy-parent? g' [:S 0 1])))
        (is (contains? forest [[:S 0 1] [[:A 0 1]]]))
        (is (contains? forest [[:S 0 1] [[:B 0 1]]]))
        (is (contains? forest' [[:S 0 1] [[:A 0 1]]]))
        (is (not (contains? forest' [[:S 0 1] [[:B 0 1]]])))))))

(deftest resolve-priority-1
  (let [grammar-str
        "E = E a E ; E = E b E ; E = E c E ; E = E d E ; E = x ;
         "
        token-set #{:a :b :c :d :x}]

    (testing "basic priority syntax and compilation"
      (without-inspection
        (let [p0 (parser/definition-parser grammar-str token-set)
              p1 (parser/definition-parser
                   (str grammar-str "priorities { E = E a E > E = E b E ; }")
                   token-set)
              p2 (parser/definition-parser
                   (str grammar-str "priorities {
                                     E = E a E > E = E b E ;
                                     E = E b E > E = E c E ;
                                     }")
                   token-set)
              p3 (parser/definition-parser
                   (str grammar-str "priorities {
                                     E = E a E > E = E b E ;
                                     E = E b E > E = E c E ;
                                     E = E a E > E = E d E ;
                                     }")
                   token-set)]
          (is (zero? (count (dag/nodes (get-in p0 [:priorities :dag])))))
          (is (= 2 (count (dag/nodes (get-in p1 [:priorities :dag])))))
          (is (= 1 (count (dag/edges (get-in p1 [:priorities :dag])))))
          (is (= 3 (count (dag/nodes (get-in p2 [:priorities :dag])))))
          (is (= 2 (count (dag/edges (get-in p2 [:priorities :dag])))))
          (is (= 4 (count (dag/nodes (get-in p3 [:priorities :dag])))))
          (is (= 3 (count (dag/edges (get-in p3 [:priorities :dag]))))))))

    (testing "basic priority"
      (let [parser (parser/definition-parser
                     (str/join \newline
                               [grammar-str
                                "priorities { E = E a E > E = E b E ; }"])
                     token-set)]
        (without-inspection
          (let [{:keys [forest forest']} (forests parser [:x :a :x :b :x] :E 0 5)]
            (is (contains? forest [[:E 0 5] [[:E 0 3]
                                             [:%b 3 1]
                                             [:E 4 1]]]))
            (is (contains? forest [[:E 0 5] [[:E 0 1]
                                             [:%a 1 1]
                                             [:E 2 3]]]))
            (is (not (contains? forest' [[:E 0 5] [[:E 0 1]
                                                   [:%a 1 1]
                                                   [:E 2 3]]])))
            (is (contains? forest' [[:E 0 5] [[:E 0 3]
                                              [:%b 3 1]
                                              [:E 4 1]]]))))
        (without-inspection
          (let [{:keys [forest forest'] :as r} (forests parser [:x :a :x :a :x :b :x] :E 0 7)]
            (inspect-pp r)
            (is (contains? forest [[:E 0 7] [[:E 0 5]
                                             [:%b 5 1]
                                             [:E 6 1]]]))
            (is (contains? forest [[:E 0 7] [[:E 0 3]
                                             [:%a 3 1]
                                             [:E 4 3]]]))

            (is (contains? forest' [[:E 0 7] [[:E 0 5]
                                              [:%b 5 1]
                                              [:E 6 1]]]))
            (is (not (contains? forest' [[:E 0 7] [[:E 0 3]
                                                   [:%a 3 1]
                                                   [:E 4 3]]])))
            (is (not (contains? forest' [[:E 0 7] [[:E 0 1]
                                                   [:%a 1 1]
                                                   [:E 2 5]]])))))))

    (testing "priority and associativity interaction"
      (without-inspection
        (let [parser (parser/definition-parser
                       (str/join \newline
                                 [grammar-str
                                  "left { E = E a E ; E = E c E ; }"
                                  "priorities { E = E a E > E = E b E ; }"])
                       token-set)
              {:keys [forest forest']} (forests parser [:x :c :x :b :x] :E 0 5)]
          (is (contains? forest [[:E 0 5] [[:E 0 3]
                                           [:%b 3 1]
                                           [:E 4 1]]]))
          (is (contains? forest [[:E 0 5] [[:E 0 1]
                                           [:%c 1 1]
                                           [:E 2 3]]]))
          (is (contains? forest' [[:E 0 5] [[:E 0 3]
                                            [:%b 3 1]
                                            [:E 4 1]]]))
          (is (not (contains? forest' [[:E 0 5] [[:E 0 1]
                                                 [:%c 1 1]
                                                 [:E 2 3]]]))))))))

(deftest resolve-priority-2
  (let [grammar-str "E = E a E ;
                     E = E b E ;
                     E = x ;"
        token-set #{:a :b :x}
        p0 (parser/definition-parser
             (str/join \newline
                       [grammar-str
                        "priorities {
                         E = E a E <0> > E = E b E ;
                         }"])
             token-set)
        p1 (parser/definition-parser
             (str/join \newline
                       [grammar-str
                        "priorities {
                         E = E a E <0,2> > E = E b E ;
                         }"])
             token-set)
        p2 (parser/definition-parser
             (str/join \newline
                       [grammar-str
                        "priorities {
                         E = E a E <2,0> > E = E b E ;
                         }"])
             token-set)
        p3 (parser/definition-parser
             (str/join \newline
                       [grammar-str
                        "priorities {
                         E = E a E > E = E b E ;
                         }"])
             token-set)]

    (testing "argument-specific priority syntax and compilation"
      (is (= #{0}
             (dag/edge-attr (get-in p0 [:priorities :dag])
                            [:E [:E :%a :E]]
                            [:E [:E :%b :E]]
                            :only)))
      (is (= #{0 2}
             (dag/edge-attr (get-in p1 [:priorities :dag])
                            [:E [:E :%a :E]]
                            [:E [:E :%b :E]]
                            :only)))
      (is (= #{0 2}
             (dag/edge-attr (get-in p2 [:priorities :dag])
                            [:E [:E :%a :E]]
                            [:E [:E :%b :E]]
                            :only)))
      (is (not
            (dag/edge-attr (get-in p3 [:priorities :dag])
                           [:E [:E :%a :E]]
                           [:E [:E :%b :E]]
                           :only))))

    (testing "argument-specific priority resolution"
      (without-inspection
        (let [{:keys [forest forest']} (forests p0 [:x :a :x :b :x] :E 0 5)]
          (is (contains? forest [[:E 0 5] [[:E 0 1]
                                           [:%a 1 1]
                                           [:E 2 3]]]))
          (is (contains? forest [[:E 0 5] [[:E 0 3]
                                           [:%b 3 1]
                                           [:E 4 1]]]))
          ;; nothing is filtered, since nothing's wrong in the 0th position of E = E a E
          (is (contains? forest' [[:E 0 5] [[:E 0 1]
                                            [:%a 1 1]
                                            [:E 2 3]]]))
          (is (contains? forest' [[:E 0 5] [[:E 0 3]
                                            [:%b 3 1]
                                            [:E 4 1]]]))))
      (without-inspection
        (let [{:keys [forest forest']} (forests p0 [:x :b :x :a :x] :E 0 5)]
          (is (contains? forest [[:E 0 5] [[:E 0 1]
                                           [:%b 1 1]
                                           [:E 2 3]]]))
          (is (contains? forest [[:E 0 5] [[:E 0 3]
                                           [:%a 3 1]
                                           [:E 4 1]]]))
          ;; the 0th position of E = E a E is filled by an instance of E = E b E, which
          ;; violates the argument-specific priority
          (is (contains? forest' [[:E 0 5] [[:E 0 1]
                                            [:%b 1 1]
                                            [:E 2 3]]]))
          (is (not (contains? forest' [[:E 0 5] [[:E 0 3]
                                                 [:%a 3 1]
                                                 [:E 4 1]]])))))
      (without-inspection
        ;; p1 is equivalent to a rule with no argument-specification, since it
        ;; covers both E slots at indices 0 and 2
        (let [{:keys [forest forest']} (forests p1 [:x :a :x :b :x] :E 0 5)]
          (is (contains? forest [[:E 0 5] [[:E 0 1]
                                           [:%a 1 1]
                                           [:E 2 3]]]))
          (is (contains? forest [[:E 0 5] [[:E 0 3]
                                           [:%b 3 1]
                                           [:E 4 1]]]))
          (is (not (contains? forest' [[:E 0 5] [[:E 0 1]
                                                 [:%a 1 1]
                                                 [:E 2 3]]])))
          (is (contains? forest' [[:E 0 5] [[:E 0 3]
                                            [:%b 3 1]
                                            [:E 4 1]]])))))))

(deftest resolve-assoc-1
  (let [grammar-str
        "E = E a E ; E = E b E ; E = x ;"
        token-set #{:a :b :x}]

    (testing "left associativity"
      (without-inspection
        (let [parser (parser/definition-parser
                       (str/join \newline
                                 [grammar-str
                                  "left { E = E a E ; }"])
                       token-set)
              {:keys [forest forest']} (forests parser [:x :a :x :a :x] :E 0 5)]
          (inspect-pp "left associativity"
                      {:forest forest
                       :forest' forest'})
          (is (contains? forest [[:E 0 3] [[:E 0 1]
                                           [:%a 1 1]
                                           [:E 2 1]]]))
          (is (contains? forest [[:E 2 3] [[:E 2 1]
                                           [:%a 3 1]
                                           [:E 4 1]]]))
          (is (contains? forest' [[:E 0 3] [[:E 0 1]
                                           [:%a 1 1]
                                           [:E 2 1]]]))
          (is (not (contains? forest' [[:E 2 3] [[:E 2 1]
                                                 [:%a 3 1]
                                                 [:E 4 1]]]))))))

    (testing "right associativity"
      (without-inspection
        (let [parser (parser/definition-parser
                       (str/join \newline
                                 [grammar-str
                                  "right { E = E a E ; }"])
                       token-set)
              {:keys [forest forest']} (forests parser [:x :a :x :a :x] :E 0 5)]
          (inspect-pp "right associativity"
                      {:forest forest
                       :forest' forest'})
          (is (contains? forest [[:E 0 3] [[:E 0 1]
                                           [:%a 1 1]
                                           [:E 2 1]]]))
          (is (contains? forest [[:E 2 3] [[:E 2 1]
                                           [:%a 3 1]
                                           [:E 4 1]]]))
          (is (not (contains? forest' [[:E 0 3] [[:E 0 1]
                                                 [:%a 1 1]
                                                 [:E 2 1]]])))
          (is (contains? forest' [[:E 2 3] [[:E 2 1]
                                            [:%a 3 1]
                                            [:E 4 1]]]))))))

  (testing "associativity syntax"
    (without-inspection
      (let [{:keys [associativities] :as parser}
            (parser/definition-parser
              "E = E a E {left} ;"
              #{:a})]
        (is (uf/find (:left associativities) [:E [:E :%a :E]]))
        (is (uf/empty? (:right associativities))))

      (let [{:keys [associativities] :as parser}
            (parser/definition-parser
              "E = E a E {left} ;
               E = E b E {right} ;"
              #{:a :b})]
        (is (uf/find (:left associativities) [:E [:E :%a :E]]))
        (is (uf/find (:right associativities) [:E [:E :%b :E]])))

      (let [{:keys [associativities] :as parser}
            (parser/definition-parser
              "E = E a E ;
               E = E b E {right} ;
               left {
                 E = E a E ;
               }"
              #{:a :b})]
        (is (uf/find (:left associativities) [:E [:E :%a :E]]))
        (is (uf/find (:right associativities) [:E [:E :%b :E]]))))))

(deftest associativity-with-whitespace-1
  (let [tokens #{:x :plus :times :exp :ws}]
    (testing "inline left associativity"
      (let [parser (parser/definition-parser
                     "E = x ;
                      E = E ws? plus ws? E {left} ;"
                     tokens)
            {:keys [forest forest']} (forests parser [:x :ws :plus :ws :x :ws :plus :ws :x] :E 0 9)]
        (is (seq (parser/ambiguous-nodes forest)))
        (is (not (seq (parser/ambiguous-nodes forest'))))
        (is (contains? forest'
                       [[:E 0 9] [[:E 0 5]
                                  [:%ws 5 1]
                                  [:%plus 6 1]
                                  [:%ws 7 1]
                                  [:E 8 1]]]))))
    (testing "group left associativity"
      (let [parser (parser/definition-parser
                     "E = x ;
                      E = E ws? plus ws? E ;
                      left {
                        E = E plus E ;
                      }"
                     tokens)
            {:keys [forest forest']} (forests parser [:x :ws :plus :ws :x :ws :plus :ws :x] :E 0 9)]
        (is (seq (parser/ambiguous-nodes forest)))
        (is (not (seq (parser/ambiguous-nodes forest'))))
        (is (contains? forest'
                       [[:E 0 9] [[:E 0 5]
                                  [:%ws 5 1]
                                  [:%plus 6 1]
                                  [:%ws 7 1]
                                  [:E 8 1]]]))))
    (testing "inline right associativity"
      (let [parser (parser/definition-parser
                     "E = x ;
                      E = E ws? plus ws? E {right} ;"
                     tokens)
            {:keys [forest forest']} (forests parser [:x :ws :plus :ws :x :ws :plus :ws :x] :E 0 9)]
        (is (seq (parser/ambiguous-nodes forest)))
        (is (not (seq (parser/ambiguous-nodes forest'))))
        (is (contains? forest'
                       [[:E 0 9] [[:E 0 1]
                                  [:%ws 1 1]
                                  [:%plus 2 1]
                                  [:%ws 3 1]
                                  [:E 4 5]]]))))
    (testing "group right associativity"
      (let [parser (parser/definition-parser
                     "E = x ;
                      E = E ws? plus ws? E ;
                      right {
                        E = E plus E ;
                      }"
                     tokens)
            {:keys [forest forest']} (forests parser [:x :ws :plus :ws :x :ws :plus :ws :x] :E 0 9)]
        (is (seq (parser/ambiguous-nodes forest)))
        (is (not (seq (parser/ambiguous-nodes forest'))))
        (is (contains? forest'
                       [[:E 0 9] [[:E 0 1]
                                  [:%ws 1 1]
                                  [:%plus 2 1]
                                  [:%ws 3 1]
                                  [:E 4 5]]]))))))

(deftest priority-with-whitespace-1
  (let [tokens #{:x :plus :times :exp :ws}
        parser (parser/definition-parser
                 "E = x ;
                  E = E ws? plus ws? E ;
                  E = E ws? times ws? E ;
                  priorities {
                     E = E times E > E = E plus E ;
                  }"
                 tokens)
        {:keys [forest forest']} (forests parser [:x :ws :times :ws :x :ws :plus :ws :x] :E 0 9)]
    (pprint {:forest forest
             :forest' forest'})
    (is (seq (parser/ambiguous-nodes forest)))
    (is (not (seq (parser/ambiguous-nodes forest'))))
    (is (contains? forest'
                   [[:E 0 9][[:E 0 5]
                             [:%ws 5 1]
                             [:%plus 6 1]
                             [:%ws 7 1]
                             [:E 8 1]]])))

  (let [tokens #{:x :plus :minus :times :exp :ws}
        parser (parser/definition-parser
                 "E = x ;
                  E = E ws? plus ws? E ;
                  E = E ws? minus ws? E ;
                  E = E ws? times ws? E ;
                  left {
                    E = E plus E ;
                    E = E minus E ;
                  }
                  priorities {
                    E = E times E > E = E plus E ;
                  }"
                 tokens)
        {:keys [forest forest']} (forests parser [:x :ws :times :ws :x :ws :minus :ws :x] :E 0 9)]
    (pprint {:forest forest
             :forest' forest'})
    (is (seq (parser/ambiguous-nodes forest)))
    (is (not (seq (parser/ambiguous-nodes forest'))))
    (is (contains? forest'
                   [[:E 0 9][[:E 0 5]
                             [:%ws 5 1]
                             [:%minus 6 1]
                             [:%ws 7 1]
                             [:E 8 1]]]))))

(deftest priority-bug-1
  ;; transitive cycle bug
  (let [tokens #{:number :plus :minus :times}
        f (fn [] (parser/definition-parser
                   "e = number ;
                    e = e minus e ;
                    e = e times e ;
                    e = e plus number ;

                    priorities {
                      e = e times e > e = e minus e ;
                      e = e minus e > e = e plus number ;
                      e = e plus number > e = e times e ;
                    }"
                   tokens))]
    (is (thrown-with-msg? js/Error
                          #"Priority definition contains cycle"
                          (f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error Detection and Recovery
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run-recovery
  [string parser lexer]
  (let [raw-tokens (lexer/lex lexer string)
        token-vec (into [] (map :label) raw-tokens)
        cyk-table (parser/cyk token-vec parser)
        prefixes (parser-recovery/compute-longest-correct-prefixes parser cyk-table token-vec)
        hiccup (parser-recovery/prefixes->hiccup-msg string raw-tokens prefixes)]
    #_(console/debug ::run-recovery {:token-vec token-vec :prefixes prefixes})
    {:prefixes prefixes
     :hiccup hiccup}))

(deftest parser-recovery-1
  (let [parser-1 (parser/definition-parser
                   "S = x ;"
                   (lexer/all-syms xyz-lexer))
        parser-2 (parser/definition-parser
                   "S = x y ;"
                   (lexer/all-syms xyz-lexer))
        parser-3 (parser/definition-parser
                   "S = x x ;
                    S = A B ;
                    A = x x ;
                    B = y ;
                    B = C ;
                    C = z ;"
                   (lexer/all-syms xyz-lexer))]
    (let [{:keys [prefixes hiccup]} (run-recovery "xx" parser-1 xyz-lexer)]
      (is (= 1 (count prefixes)))
      (let [prefix (first prefixes)]
        (is (= 3 (count prefix)))
        (is (= [:-end-sentinel 1 0 nil] (peek prefix)))))
    (let [{:keys [prefixes hiccup]} (run-recovery "xx" parser-2 xyz-lexer)]
      (is (= 1 (count prefixes)))
      (let [prefix (first prefixes)]
        (is (= 3 (count prefix)))
        (is (= [:-end-sentinel 1 0 :%y] (peek prefix)))))
    (let [{:keys [prefixes hiccup]} (run-recovery "xxxyz" parser-3 xyz-lexer)]
      (is (= 3 (count prefixes)))
      (let [expected (into #{}
                           (map (comp peek peek))
                           prefixes)]
        (is (= expected #{nil :%y :%z}))))))

(deftest parser-recovery-2
  (let [parser-1 (parser/definition-parser
                   "S = S x ;"
                   (lexer/all-syms xyz-lexer))
        {:keys [prefixes]} (run-recovery "x" parser-1 xyz-lexer)]
    #_(console/debug ::parser-recovery-2 {:prefixes prefixes})
    (is (= 0 (count prefixes)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pragmas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest pragmas-1
  (let [{:keys [source-map] :as parser}
        (parser/definition-parser
          "{-# noop-pragma start=E #-}
           E = a ;
           "
          #{:a}
          :keep-source-map true)]
    (is (= (:pragma source-map) [[:noop {:span [4 15]}] [:start {:span [16 23]}]]))
    (is (= :E (get-in parser [:options :start])))
    (is (= :E (parser/start-symbol parser)))))

(deftest pragmas-2
  (is (thrown-with-msg? js/Error
                        #"Start pragma refers to undefined nonterminal"
                        (parser/definition-parser
                          "{-# noop-pragma start=F #-}
                           E = a ;
                           "
                          #{:a}))))
