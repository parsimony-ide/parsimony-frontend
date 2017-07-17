(ns parsimony.asm-parser-test
  (:require [cljs.pprint :refer [pprint]]
            [cljs.test :refer-macros [deftest is testing]]
            [clojure.set :as set]
            [parsimony.common-test :refer [xyz-lexer]]
            [parsimony.lexer :as lexer]
            [parsimony.parser :as parser]
            [parsimony.parser-recovery :as parser-recovery]
            [parsimony.parser-test :as parser-test]
            [parsimony.asm.parser :as asm.parser]
            [parsimony.asm.parser-recovery :as asm.parser-recovery]
            [parsimony.console :as console]))

(def csharp-tokens
  (vector
   {:string "using", :label :using, :start 0, :end 5}
   {:string " ", :label :ws, :start 5, :end 6}
   {:string "System", :label :ident, :start 6, :end 12}
   {:string ";", :label :semi, :start 12, :end 13}
   {:string "\n", :label :ws, :start 13, :end 14}
   {:string "using", :label :using, :start 14, :end 19}
   {:string " ", :label :ws, :start 19, :end 20}
   {:string "System", :label :ident, :start 20, :end 26}
   {:string ".", :label :dot, :start 26, :end 27}
   {:string "Collections", :label :ident, :start 27, :end 38}
   {:string ".", :label :dot, :start 38, :end 39}
   {:string "Generic", :label :ident, :start 39, :end 46}
   {:string ";", :label :semi, :start 46, :end 47}
   {:string "\n", :label :ws, :start 47, :end 48}
   {:string "using", :label :using, :start 48, :end 53}
   {:string " ", :label :ws, :start 53, :end 54}
   {:string "System", :label :ident, :start 54, :end 60}
   {:string ".", :label :dot, :start 60, :end 61}
   {:string "Linq", :label :ident, :start 61, :end 65}
   {:string ";", :label :semi, :start 65, :end 66}
   {:string "\n", :label :ws, :start 66, :end 67}
   {:string "using", :label :using, :start 67, :end 72}
   {:string " ", :label :ws, :start 72, :end 73}
   {:string "System", :label :ident, :start 73, :end 79}
   {:string ".", :label :dot, :start 79, :end 80}
   {:string "Text", :label :ident, :start 80, :end 84}
   {:string ";", :label :semi, :start 84, :end 85}
   {:string "\n \n", :label :ws, :start 85, :end 88}
   {:string "namespace", :label :namespace, :start 88, :end 97}
   {:string " ", :label :ws, :start 97, :end 98}
   {:string "HelloWorld", :label :ident, :start 98, :end 108}
   {:string "\n", :label :ws, :start 108, :end 109}
   {:string "{", :label :lbrace, :start 109, :end 110}
   {:string "\n    ", :label :ws, :start 110, :end 115}
   {:string "class", :label :class, :start 115, :end 120}
   {:string " ", :label :ws, :start 120, :end 121}
   {:string "Program", :label :ident, :start 121, :end 128}
   {:string "\n    ", :label :ws, :start 128, :end 133}
   {:string "{", :label :lbrace, :start 133, :end 134}
   {:string "\n        ", :label :ws, :start 134, :end 143}
   {:string "static", :label :static, :start 143, :end 149}
   {:string " ", :label :ws, :start 149, :end 150}
   {:string "void", :label :void, :start 150, :end 154}
   {:string " ", :label :ws, :start 154, :end 155}
   {:string "Main", :label :ident, :start 155, :end 159}
   {:string "(", :label :lparen, :start 159, :end 160}
   {:string "string", :label :ident, :start 160, :end 166}
   {:string "[", :label :lsquare, :start 166, :end 167}
   {:string "]", :label :rsquare, :start 167, :end 168}
   {:string " ", :label :ws, :start 168, :end 169}
   {:string "args", :label :ident, :start 169, :end 173}
   {:string ")", :label :rparen, :start 173, :end 174}
   {:string "\n        ", :label :ws, :start 174, :end 183}
   {:string "{", :label :lbrace, :start 183, :end 184}
   {:string "\n            ", :label :ws, :start 184, :end 197}
   {:string "Console", :label :ident, :start 197, :end 204}
   {:string ".", :label :dot, :start 204, :end 205}
   {:string "WriteLine", :label :ident, :start 205, :end 214}
   {:string "(", :label :lparen, :start 214, :end 215}
   {:string "\"Hello World!\"", :label :string, :start 215, :end 229}
   {:string ")", :label :rparen, :start 229, :end 230}
   {:string ";", :label :semi, :start 230, :end 231}
   {:string "\n            ", :label :ws, :start 231, :end 244}
   {:string "Console", :label :ident, :start 244, :end 251}
   {:string ".", :label :dot, :start 251, :end 252}
   {:string "ReadKey", :label :ident, :start 252, :end 259}
   {:string "(", :label :lparen, :start 259, :end 260}
   {:string ")", :label :rparen, :start 260, :end 261}
   {:string ";", :label :semi, :start 261, :end 262}
   {:string "\n        ", :label :ws, :start 262, :end 271}
   {:string "}", :label :rbrace, :start 271, :end 272}
   {:string "\n    ", :label :ws, :start 272, :end 277}
   {:string "}", :label :rbrace, :start 277, :end 278}
   {:string "\n", :label :ws, :start 278, :end 279}
   {:string "}", :label :rbrace, :start 279, :end 280}
   {:string "\n", :label :ws, :start 280, :end 281}))

(def csharp-grammar-str
  "
Qual-ident     = Qual-ident dot ident ;
Qual-ident     = ident ;
Using          = using ws Qual-ident ws? ;
Namespace-decl = namespace ws ident ws? lbrace ws? Class ws? rbrace ;

Expr           = Qual-ident;
Expr           = string ;

Type           = void ;
Type           = ident ;
Type           = Type ws? lsquare ws? rsquare ;

Arg-list       = Expr ;
Arg-list       = Arg-list ws? comma ws? Expr ;

Param          = Type ws ident ;
Param-list     = Param ;
Param-list     = Param-list ws? comma ws? Param ;

Function       = static ws void ws ident ws? lparen ws? Param-list ws? rparen ws? lbrace ws? Stmt-list ws? rbrace ;

Stmt           = Qual-ident ws? lparen ws? Arg-list? ws? rparen ;
Stmt-list      = Stmt ws? semi ;
Stmt-list      = Stmt-list ws? Stmt ws? semi ;

Class          = class ws ident ws? lbrace ws? Function ws? rbrace ;
")

(def csharp-token-kws
  #{:using
    :namespace
    :class
    :static
    :void
    :ident
    :dot
    :semi
    :lbrace
    :rbrace
    :lparen
    :rparen
    :lsquare
    :rsquare
    :string
    :ws
    :comma
    :comment})

(defn run-asm-parser [parser token-vec & options]
  (let [options (set options)
        {:keys [cyk codec]} (asm.parser/cpp-init-cyk token-vec parser)]
    (asm.parser/cpp-run-cyk cyk)
    (asm.parser/cpp-run-color cyk)
    (let [max-coloring (asm.parser/get-colors codec cyk 0 (count token-vec))
          min-coloring (asm.parser/succinct-coloring codec cyk token-vec parser max-coloring)]
      (asm.parser/cpp-free cyk)
      (when (:verbose options)
        (pprint {:i 0
                 :l (count token-vec)
                 :max-coloring max-coloring
                 :min-coloring min-coloring}))
      {:max-coloring max-coloring
       :min-coloring min-coloring})))

(deftest small-asm-parser-1
  (let [grammar-str "A = x ; A = x y ; B = x y ;"
        token-vec [:x :y :x]
        parser (parser/definition-parser grammar-str #{:x :y})
        {:keys [max-coloring min-coloring]} (run-asm-parser parser token-vec :verbose)]
    (is (= max-coloring min-coloring #{[:A 0 2] [:B 0 2] [:A 2 1]}))))

(deftest small-asm-parser-2
  (let [grammar-str "A = x ; A = y ;"
        token-vec [:x :y :x]
        parser (parser/definition-parser grammar-str #{:x :y})
        {:keys [max-coloring min-coloring]} (run-asm-parser parser token-vec :verbose)]
    (is (= max-coloring min-coloring #{[:A 0 1] [:A 1 1] [:A 2 1]}))))

#_(deftest big-asm-parser-1
  (let [token-vec (into [] (map :label) csharp-tokens)
        parser (parser/definition-parser csharp-grammar-str csharp-token-kws)]
    (run-asm-parser parser token-vec :verbose)))

(deftest empty-token-stream-1
  (let [parser (parser/definition-parser csharp-grammar-str csharp-token-kws)]
    (is (thrown-with-msg? js/Error
                          #"Empty token stream"
                          (run-asm-parser parser [])))))

(deftest cyk-and-coloring-1
  (let [s "1.2e-3 42"
        parser parser-test/numbers-1
        raw-tokens (lexer/lex parser-test/numbers-lex-infos s nil)
        token-vec (into [] (map :label) raw-tokens)
        {:keys [max-coloring min-coloring]} (run-asm-parser parser token-vec)]
    (is (= max-coloring #{[:Number 0 6] [:Real 0 6] [:Integer 7 2] [:Number 7 2]}))
    (is (set/subset? min-coloring max-coloring))
    (is (= min-coloring #{[:Number 0 6] [:Number 7 2]}))))

(deftest cyk-and-coloring-2
  (let [s "aab aab"
        parser parser-test/ambig-2
        raw-tokens [{:label :a :start 0 :end 1}
                    {:label :a :start 1 :end 2}
                    {:label :b :start 2 :end 3}
                    {:label :w :start 3 :end 4}
                    {:label :a :start 4 :end 5}
                    {:label :a :start 5 :end 6}
                    {:label :b :start 6 :end 7}]
        token-vec (into [] (map :label) raw-tokens)
        {:keys [max-coloring min-coloring]} (run-asm-parser parser token-vec)]
    (is (= min-coloring max-coloring #{[:S 0 3] [:T 0 3] [:U 0 3] [:S 4 3] [:T 4 3] [:U 4 3]}))))

(deftest cyk-and-coloring-3
  (let [s "a  a"
        parser (parser/definition-parser "A = a ;" #{:a})
        raw-tokens [{:label :a :start 0 :end 1}
                    {:label :w :start 1 :end 3}
                    {:label :a :start 3 :end 4}]
        token-vec (into [] (map :label) raw-tokens)
        {:keys [max-coloring min-coloring]} (run-asm-parser parser token-vec)]
    (is (= min-coloring max-coloring #{[:A 0 1] [:A 2 1]}))))

(deftest coloring-bug-1
  (let [token-vec [:using :ident :semi
                   :using :ident :dot :ident :dot :ident :semi]
        grammar-str "Qual-ident = Qual-ident dot ident ;
                     Qual-ident = ident ;
                     Using = using Qual-ident ;
                     Expr = Qual-ident ;
                     Arg-list = Expr ;"
        parser (parser/definition-parser grammar-str csharp-token-kws)
        {:keys [min-coloring max-coloring]} (run-asm-parser parser token-vec)]
    (is (= #{[:Using 0 2]
             [:Using 3 6]}
           max-coloring
           min-coloring))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error Detection and Recovery
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run-recovery
  [string parser lexer]
  (let [raw-tokens (lexer/lex lexer string)
        token-vec (into [] (map :label) raw-tokens)
        {:keys [codec cyk]} (asm.parser/cpp-init-cyk token-vec parser)]
    (asm.parser/cpp-run-cyk cyk)
    (let [prefixes (asm.parser-recovery/compute-longest-correct-prefixes parser codec cyk token-vec)
          hiccup (parser-recovery/prefixes->hiccup-msg string raw-tokens prefixes)]
      (asm.parser/cpp-free cyk)
      #_(console/debug ::run-recovery {:token-vec token-vec :prefixes prefixes})
      {:prefixes prefixes
       :hiccup hiccup})))

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
