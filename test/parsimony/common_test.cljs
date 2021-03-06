(ns parsimony.common-test
  (:require [parsimony.parser :as parser]))

(def csharp-lexer
  [[:using
    {:source-str "using = using ;",
     :regex-str "using",
     :js-automaton
     {:states
      {0
       {:transitions [{:min "n", :max "n", :to 2}],
        :id 0,
        :accept false},
       1
       {:transitions [{:min "i", :max "i", :to 0}],
        :id 1,
        :accept false},
       2
       {:transitions [{:min "g", :max "g", :to 3}],
        :id 2,
        :accept false},
       3 {:transitions [], :id 3, :accept true},
       4
       {:transitions [{:min "u", :max "u", :to 5}],
        :id 4,
        :accept false},
       5
       {:transitions [{:min "s", :max "s", :to 1}],
        :id 5,
        :accept false}},
      :initial 4}}]
   [:namespace
    {:source-str "\nnamespace = namespace ;",
     :regex-str "namespace",
     :js-automaton
     {:states
      {0
       {:transitions [{:min "c", :max "c", :to 6}],
        :id 0,
        :accept false},
       1
       {:transitions [{:min "m", :max "m", :to 9}],
        :id 1,
        :accept false},
       2
       {:transitions [{:min "a", :max "a", :to 1}],
        :id 2,
        :accept false},
       3
       {:transitions [{:min "a", :max "a", :to 0}],
        :id 3,
        :accept false},
       4
       {:transitions [{:min "n", :max "n", :to 2}],
        :id 4,
        :accept false},
       5
       {:transitions [{:min "s", :max "s", :to 7}],
        :id 5,
        :accept false},
       6
       {:transitions [{:min "e", :max "e", :to 8}],
        :id 6,
        :accept false},
       7
       {:transitions [{:min "p", :max "p", :to 3}],
        :id 7,
        :accept false},
       8 {:transitions [], :id 8, :accept true},
       9
       {:transitions [{:min "e", :max "e", :to 5}],
        :id 9,
        :accept false}},
      :initial 4}}]
   [:class
    {:source-str "\nclass = class ;",
     :regex-str "class",
     :js-automaton
     {:states
      {0
       {:transitions [{:min "a", :max "a", :to 1}],
        :id 0,
        :accept false},
       1
       {:transitions [{:min "s", :max "s", :to 3}],
        :id 1,
        :accept false},
       2
       {:transitions [{:min "c", :max "c", :to 5}],
        :id 2,
        :accept false},
       3
       {:transitions [{:min "s", :max "s", :to 4}],
        :id 3,
        :accept false},
       4 {:transitions [], :id 4, :accept true},
       5
       {:transitions [{:min "l", :max "l", :to 0}],
        :id 5,
        :accept false}},
      :initial 2}}]
   [:static
    {:source-str "\nstatic = static ;",
     :regex-str "static",
     :js-automaton
     {:states
      {0
       {:transitions [{:min "t", :max "t", :to 3}],
        :id 0,
        :accept false},
       1
       {:transitions [{:min "t", :max "t", :to 6}],
        :id 1,
        :accept false},
       2
       {:transitions [{:min "s", :max "s", :to 1}],
        :id 2,
        :accept false},
       3
       {:transitions [{:min "i", :max "i", :to 4}],
        :id 3,
        :accept false},
       4
       {:transitions [{:min "c", :max "c", :to 5}],
        :id 4,
        :accept false},
       5 {:transitions [], :id 5, :accept true},
       6
       {:transitions [{:min "a", :max "a", :to 0}],
        :id 6,
        :accept false}},
      :initial 2}}]
   [:void
    {:source-str "\nvoid = void ;",
     :regex-str "void",
     :js-automaton
     {:states
      {0
       {:transitions [{:min "v", :max "v", :to 1}],
        :id 0,
        :accept false},
       1
       {:transitions [{:min "o", :max "o", :to 2}],
        :id 1,
        :accept false},
       2
       {:transitions [{:min "i", :max "i", :to 4}],
        :id 2,
        :accept false},
       3 {:transitions [], :id 3, :accept true},
       4
       {:transitions [{:min "d", :max "d", :to 3}],
        :id 4,
        :accept false}},
      :initial 0}}]
   [:ident
    {:source-str "\n\nident = [a-zA-Z0-9]+ ;",
     :regex-str "[a-zA-Z0-9]+",
     :js-automaton
     {:states
      {0
       {:transitions
        [{:min "A", :max "Z", :to 1}
         {:min "a", :max "z", :to 1}
         {:min "0", :max "9", :to 1}],
        :id 0,
        :accept false},
       1
       {:transitions
        [{:min "A", :max "Z", :to 1}
         {:min "a", :max "z", :to 1}
         {:min "0", :max "9", :to 1}],
        :id 1,
        :accept true}},
      :initial 0}}]
   [:dot
    {:source-str "\ndot = \\. ;",
     :regex-str "\\.",
     :js-automaton
     {:states
      {0
       {:transitions [{:min ".", :max ".", :to 1}],
        :id 0,
        :accept false},
       1 {:transitions [], :id 1, :accept true}},
      :initial 0}}]
   [:semi
    {:source-str "\nsemi = \\; ;",
     :regex-str "\\;",
     :js-automaton
     {:states
      {0 {:transitions [], :id 0, :accept true},
       1
       {:transitions [{:min ";", :max ";", :to 0}],
        :id 1,
        :accept false}},
      :initial 1}}]
   [:lbrace
    {:source-str "\nlbrace = \\{ ;",
     :regex-str "\\{",
     :js-automaton
     {:states
      {0
       {:transitions [{:min "{", :max "{", :to 1}],
        :id 0,
        :accept false},
       1 {:transitions [], :id 1, :accept true}},
      :initial 0}}]
   [:rbrace
    {:source-str "\nrbrace = \\} ;",
     :regex-str "\\}",
     :js-automaton
     {:states
      {0 {:transitions [], :id 0, :accept true},
       1
       {:transitions [{:min "}", :max "}", :to 0}],
        :id 1,
        :accept false}},
      :initial 1}}]
   [:lparen
    {:source-str "\nlparen = \\( ;",
     :regex-str "\\(",
     :js-automaton
     {:states
      {0 {:transitions [], :id 0, :accept true},
       1
       {:transitions [{:min "(", :max "(", :to 0}],
        :id 1,
        :accept false}},
      :initial 1}}]
   [:rparen
    {:source-str "\nrparen = \\) ;",
     :regex-str "\\)",
     :js-automaton
     {:states
      {0
       {:transitions [{:min ")", :max ")", :to 1}],
        :id 0,
        :accept false},
       1 {:transitions [], :id 1, :accept true}},
      :initial 0}}]
   [:lsquare
    {:source-str "\nlsquare = \\[ ;",
     :regex-str "\\[",
     :js-automaton
     {:states
      {0
       {:transitions [{:min "[", :max "[", :to 1}],
        :id 0,
        :accept false},
       1 {:transitions [], :id 1, :accept true}},
      :initial 0}}]
   [:rsquare
    {:source-str "\nrsquare = \\] ;",
     :regex-str "\\]",
     :js-automaton
     {:states
      {0 {:transitions [], :id 0, :accept true},
       1
       {:transitions [{:min "]", :max "]", :to 0}],
        :id 1,
        :accept false}},
      :initial 1}}]
   [:string
    {:source-str "string = \\\" [^\"]+ \\\" ;",
     :regex-str "\\\"[^\"]+\\\"",
     :js-automaton
     {:states
      {0
       {:transitions [{:min "\"", :max "\"", :to 3}],
        :id 0,
        :accept false},
       1 {:transitions [], :id 1, :accept true},
       2
       {:transitions
        [{:min "#", :max "￿", :to 2}
         {:min " ", :max "!", :to 2}
         {:min "\"", :max "\"", :to 1}],
        :id 2,
        :accept false},
       3
       {:transitions
        [{:min "#", :max "￿", :to 2} {:min " ", :max "!", :to 2}],
        :id 3,
        :accept false}},
      :initial 0}}]
   [:ws
    {:source-str "\nws = [ \\n\\r\\t]+ ;",
     :regex-str "[ \n\r\t]+",
     :js-automaton
     {:states
      {0
       {:transitions
        [{:min "\t", :max "\n", :to 0}
         {:min " ", :max " ", :to 0}
         {:min "\r", :max "\r", :to 0}],
        :id 0,
        :accept true},
       1
       {:transitions
        [{:min "\t", :max "\n", :to 0}
         {:min " ", :max " ", :to 0}
         {:min "\r", :max "\r", :to 0}],
        :id 1,
        :accept false}},
      :initial 1}}]
   [:comma
    {:source-str "\ncomma = , ;",
     :regex-str ",",
     :js-automaton
     {:states
      {0 {:transitions [], :id 0, :accept true},
       1
       {:transitions [{:min ",", :max ",", :to 0}],
        :id 1,
        :accept false}},
      :initial 1}}]
   [:comment
    {:source-str "\ncomment = /\\* .* \\*/ !& /\\* .* \\*/ .* \\*/ ;",
     :regex-str "(/\\*.*\\*/&~(/\\*.*\\*/.*\\*/))",
     :js-automaton
     {:states
      {0
       {:transitions [{:min "/", :max "/", :to 4}],
        :id 0,
        :accept false},
       1
       {:transitions
        [{:min "+", :max ".", :to 2}
         {:min "*", :max "*", :to 1}
         {:min " ", :max ")", :to 2}
         {:min "/", :max "/", :to 3}
         {:min "0", :max "￿", :to 2}],
        :id 1,
        :accept false},
       2
       {:transitions
        [{:min "+", :max "￿", :to 2}
         {:min "*", :max "*", :to 1}
         {:min " ", :max ")", :to 2}],
        :id 2,
        :accept false},
       3 {:transitions [], :id 3, :accept true},
       4
       {:transitions [{:min "*", :max "*", :to 2}],
        :id 4,
        :accept false}},
      :initial 0}}]
   [:mod
    {:source-str "mod = \\% ;",
     :regex-str "\\%",
     :js-automaton
     {:states
      {0
       {:transitions [{:min "%", :max "%", :to 1}],
        :id 0,
        :accept false},
       1 {:transitions [], :id 1, :accept true}},
      :initial 0}}]
   [:div
    {:source-str "div = \\/ ;",
     :regex-str "\\/",
     :js-automaton
     {:states
      {0
       {:transitions [{:min "/", :max "/", :to 1}],
        :id 0,
        :accept false},
       1 {:transitions [], :id 1, :accept true}},
      :initial 0}}]
   [:exp
    {:source-str "exp = \\^ ;",
     :regex-str "\\^",
     :js-automaton
     {:states
      {0 {:transitions [], :id 0, :accept true},
       1
       {:transitions [{:min "^", :max "^", :to 0}],
        :id 1,
        :accept false}},
      :initial 1}}]
   [:times
    {:source-str "times = \\* ;",
     :regex-str "\\*",
     :js-automaton
     {:states
      {0 {:transitions [], :id 0, :accept true},
       1
       {:transitions [{:min "*", :max "*", :to 0}],
        :id 1,
        :accept false}},
      :initial 1}}]
   [:minus
    {:source-str "minus = \\- ;",
     :regex-str "\\-",
     :js-automaton
     {:states
      {0 {:transitions [], :id 0, :accept true},
       1
       {:transitions [{:min "-", :max "-", :to 0}],
        :id 1,
        :accept false}},
      :initial 1}}]
   [:plus
    {:source-str "plus = \\+ ;",
     :regex-str "\\+",
     :js-automaton
     {:states
      {0
       {:transitions [{:min "+", :max "+", :to 1}],
        :id 0,
        :accept false},
       1 {:transitions [], :id 1, :accept true}},
      :initial 0}}]])

(def csharp-cfg-str
  "
Qual-ident = Qual-ident dot ident ;
Qual-ident = ident ;
Using = using ws Qual-ident ws? ;
Namespace-decl = namespace ws ident ws?
         lbrace ws? Class ws? rbrace ;

Expr = Qual-ident;
Expr = string ;

Type = void ;
Type = ident ;
Type = Type ws? lsquare ws? rsquare ;

Arg-list = Expr ;
Arg-list = Arg-list ws? comma ws? Expr ;

Param = Type ws ident ;
Param-list = Param ;
Param-list = Param-list ws? comma ws? Param ;

Function = static ws void ws ident ws? lparen ws? Param-list ws? rparen ws? lbrace ws? Stmt-list ws? rbrace ;

Stmt = Qual-ident ws? lparen ws? Arg-list? ws? rparen ;
Stmt-list = Stmt ws? semi ;
Stmt-list = Stmt-list ws? Stmt ws? semi ;


Class = class ws ident ws? lbrace ws? Function ws? rbrace ;
  ")

(def xyz-lexer
  [[:x
    {:source-str "x = x ;",
     :regex-str "x",
     :js-automaton
     {:states
      {0 {:transitions [], :id 0, :accept true},
       1
       {:transitions [{:min "x", :max "x", :to 0}],
        :id 1,
        :accept false}},
      :initial 1}}]
   [:y
    {:source-str "\ny = y ;",
     :regex-str "y",
     :js-automaton
     {:states
      {0
       {:transitions [{:min "y", :max "y", :to 1}],
        :id 0,
        :accept false},
       1 {:transitions [], :id 1, :accept true}},
      :initial 0}}]
   [:z
    {:source-str "\nz = z ;",
     :regex-str "z",
     :js-automaton
     {:states
      {0
       {:transitions [{:min "z", :max "z", :to 1}],
        :id 0,
        :accept false},
       1 {:transitions [], :id 1, :accept true}},
      :initial 0}}]])

(def aab-lexer
  [[:a
    {:source-str "a = a ;",
     :regex-str "a",
     :js-automaton
     {:states
      {0
       {:transitions [{:min "a", :max "a", :to 1}],
        :id 0,
        :accept false},
       1 {:transitions [], :id 1, :accept true}},
      :initial 0}}]
   [:b
    {:source-str "\nb = b ;",
     :regex-str "b",
     :js-automaton
     {:states
      {0 {:transitions [], :id 0, :accept true},
       1
       {:transitions [{:min "b", :max "b", :to 0}],
        :id 1,
        :accept false}},
      :initial 1}}]
   [:w
    {:source-str "\nw = [ \\n\\t\\r]+ ;",
     :regex-str "[ \n\t\r]+",
     :js-automaton
     {:states
      {0
       {:transitions
        [{:min "\t", :max "\n", :to 0}
         {:min " ", :max " ", :to 0}
         {:min "\r", :max "\r", :to 0}],
        :id 0,
        :accept true},
       1
       {:transitions
        [{:min "\t", :max "\n", :to 0}
         {:min " ", :max " ", :to 0}
         {:min "\r", :max "\r", :to 0}],
        :id 1,
        :accept false}},
      :initial 1}}]])

(def aab-cfg-str
  "S = A B ;
   A = a ;
   A = a a ;
   B = b ;
   B = a b ;")

(def aab-parser (parser/definition-parser aab-cfg-str (map first aab-lexer)))

(def if-else-lexer
  [[:if
    {:source-str "if = if ;",
     :regex-str "if",
     :js-automaton
     {:states
      {0 {:transitions [], :id 0, :accept true},
       1
       {:transitions [{:min "i", :max "i", :to 2}],
        :id 1,
        :accept false},
       2
       {:transitions [{:min "f", :max "f", :to 0}],
        :id 2,
        :accept false}},
      :initial 1}}]
   [:else
    {:source-str "\nelse = else ;",
     :regex-str "else",
     :js-automaton
     {:states
      {0
       {:transitions [{:min "e", :max "e", :to 2}],
        :id 0,
        :accept false},
       1 {:transitions [], :id 1, :accept true},
       2
       {:transitions [{:min "l", :max "l", :to 4}],
        :id 2,
        :accept false},
       3
       {:transitions [{:min "e", :max "e", :to 1}],
        :id 3,
        :accept false},
       4
       {:transitions [{:min "s", :max "s", :to 3}],
        :id 4,
        :accept false}},
      :initial 0}}]
   [:star
    {:source-str "\nstar = \\* ;",
     :regex-str "\\*",
     :js-automaton
     {:states
      {0 {:transitions [], :id 0, :accept true},
       1
       {:transitions [{:min "*", :max "*", :to 0}],
        :id 1,
        :accept false}},
      :initial 1}}]
   [:ws
    {:source-str "\nws = [ \\n\\r\\t]+ ;",
     :regex-str "[ \n\r\t]+",
     :js-automaton
     {:states
      {0
       {:transitions
        [{:min "\t", :max "\n", :to 0}
         {:min " ", :max " ", :to 0}
         {:min "\r", :max "\r", :to 0}],
        :id 0,
        :accept true},
       1
       {:transitions
        [{:min "\t", :max "\n", :to 0}
         {:min " ", :max " ", :to 0}
         {:min "\r", :max "\r", :to 0}],
        :id 1,
        :accept false}},
      :initial 1}}]])

(def if-else-cfg-str
  "if-else = if ws star ws body ;
   if-else = if ws star ws body ws else ws body ;
   body = star ;
   body = if-else ;")

(def if-else-parser
  (parser/definition-parser if-else-cfg-str (map first if-else-lexer)))

(def if-else-sample
  "if * if * * else *")

(defn forests
  "Return a map {:forest ... :forest' ...} with the parse forests before and
   after disambiguation"
  [parser token-vec nt i l]
  (let [cyk-table (parser/cyk token-vec parser)
        coloring-table (parser/coloring cyk-table)
        forest (parser/reconstruct cyk-table token-vec parser nt i l)]
    {:forest forest :forest' (parser/disambiguate forest parser)}))
