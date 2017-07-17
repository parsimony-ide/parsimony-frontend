(ns parsimony.views.parsing-lesson
  (:require [cljs.pprint :refer [*print-right-margin*]]
            [clojure.string :as str]
            [parsimony.com.buttons :refer [button]]
            [parsimony.parser :as parser]
            [parsimony.util :refer [pprint-str]]
            [parsimony.views.common :refer [icon]]
            [parsimony.views.modal :refer [IModal]]
            [parsimony.views.parse-forest :as parse-forest]
            [parsimony.workers.batch-lexer :as batch-lexer]
            [parsimony.workers.batch-parser :as batch-parser]
            [re-com.core :refer [h-box v-box scroller line gap] :refer-macros [handler-fn] :as re-com]
            [re-frame.core :refer [dispatch]]
            [reagent.core :as reagent]))

(enable-console-print!)

(defn p [& args]
  (into [re-com/p {:style {:text-align "justify"
                           :width "490px"
                           :min-width "490px"}}]
        args))

(defn pre [s]
  [:pre {:style {:width "490px"
                 :font-size "12px"}}
   s])

(defn emph [s]
  [:span {:style {:font-style "italic"
                  :text-decoration "underline"}}
   s])

(defn title-string [page-number s]
  (str (inc page-number) ". " s))

(defn title [page-number s]
  [:div {:style {:font-size "25px"
                 :font-weight "lighter"}}
   (title-string page-number s)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def string
  (str/join \newline
            ["void helloworld() {"
             "    printf(\"Hello, world!\");"
             "}"]))

(def lexer-definition
"STRING = \\\" ![\"\\r\\n]* \\\" ;
VOID   = void ;
IDENT  = [a-z]+;
LPAREN = \\( ;
RPAREN = \\) ;
LBRACE = \\{ ;
RBRACE = \\} ;
SEMI   = \\; ;")

(def tokens
  [{:string "void", :label :VOID, :start 0, :end 4}
   {:string "helloworld", :label :IDENT, :start 5, :end 15}
   {:string "(", :label :LPAREN, :start 15, :end 16}
   {:string ")", :label :RPAREN, :start 16, :end 17}
   {:string "{", :label :LBRACE, :start 18, :end 19}
   {:string "printf", :label :IDENT, :start 24, :end 30}
   {:string "(", :label :LPAREN, :start 30, :end 31}
   {:string "\"Hello, world!\"", :label :STRING, :start 31, :end 46}
   {:string ")", :label :RPAREN, :start 46, :end 47}
   {:string ";", :label :SEMI, :start 47, :end 48}
   {:string "}", :label :RBRACE, :start 49, :end 50}])

(def token-vec
  (into []
        (map :label)
        tokens))

(def grammar
"function  = type IDENT arglist body ;
type      = VOID ;
arglist   = LPAREN RPAREN ;
body      = LBRACE statement RBRACE;
statement = call ;
call      = IDENT LPAREN expr RPAREN SEMI ;
expr      = STRING ;")

(def parser
  (parser/definition-parser grammar (set token-vec)))

(def cyk
  (parser/cyk token-vec parser))

(def forest
  (parser/reconstruct cyk token-vec parser :function 0 (count token-vec)))

(def rendered-forest
  (binding [*print-right-margin* 50]
    (batch-parser/format-forest (batch-parser/string-index-map string)
                                tokens
                                (parser/ambiguous-nodes forest)
                                forest)))

(def decoration-map
  {:parse
   (into {}
         (comp
           (map parser/->nonterminal)
           (map-indexed #(vector (name %2) %1)))
         (parser/all-syms (:productions parser)))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; View
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare pages)

(defonce ^:private current-page (reagent/atom 0))

(defn pinned [f n x]
  (if (= n x)
    n
    (f x)))

(defn next-button []
  []
  [button
   :attr {:on-mouse-down (handler-fn (.preventDefault event))}
   :label "Next"
   :disabled? (= (dec (count pages)) @current-page)
   :on-click #(swap! current-page (partial pinned inc (dec (count pages))))])

(defn prev-button []
  [button
   :attr {:on-mouse-down (handler-fn (.preventDefault event))}
   :label "Previous"
   :disabled? (= 0 @current-page)
   :on-click #(swap! current-page (partial pinned dec 0))])

(defn footer []
  [v-box
   :children
   [[line]
    [gap :size "15px"]
    [h-box
     :style {:color "#bbb"}
     :align :center
     :justify :between
     :children
     (-> []
         (conj [prev-button])
         (into
           (map (fn [i]
                  [icon {:style {:font-size "16px"
                                 :cursor "pointer"}
                         :md-icon-name
                         (if (= @current-page i)
                           "zmdi-circle"
                           "zmdi-circle-o")
                         :on-click #(reset! current-page i)
                         :tooltip [:span {:style {:font-size "14px"}}
                                   (title-string i (:title (get pages i)))]}]))
           (range (count pages)))
         (conj [next-button]))]]])

(defn page-0 []
  [v-box
   :children
   [[p "Parsing is the task of taking a string (a sequence of characters)
        and deriving the underlying structure encoded by that string."]
    [p "One of the most obvious uses of parsing is to transform the text of a
        program into its corresponding syntax tree according to the syntax
        rules of the programming language."]
    [p "For example, take the following familiar C program:"]
    [pre string]
    [p "Visually, it is easy for us to see that this is a function, the body
        of which contains a single " [:code "printf"]
     "statement. Our human brains allow us to easily see the hierarchical
      structure in the text, almost without conscious thought.  But how do we
      write a program that figures this out programmatically?"]]])

(defn page-1 []
  (let [box-css {:border "2px solid black"
                 :background-color "white"
                 :padding "5px"}]
    [v-box
     :children
     [[p "One of the most well-known approaches to this problem is to use a
          pipeline consisting of two programs run back to back: a " (emph "lexer")
       " and a " (emph "parser") "."]
      [v-box
       :align :center
       :margin "0px 0px 10px 0px"
       :children
       [[h-box
         :align :center
         :gap "5px"
         :style {:background-color "#efefef"
                 :padding "10px"}
         :children [[:div {:style {:font-style "italic"}} "String"]
                    [icon {:md-icon-name "zmdi-arrow-right"}]
                    [:div {:style box-css} "Lexer"]
                    [icon {:md-icon-name "zmdi-arrow-right"}]
                    [:div {:style {:font-style "italic"}} "Token Stream"]
                    [icon {:md-icon-name "zmdi-arrow-right"}]
                    [:div {:style box-css} "Parser"]
                    [icon {:md-icon-name "zmdi-arrow-right"}]
                    [:div {:style {:font-style "italic"}} "Syntax Tree"]]]]]
      [p "The first phase, the lexer, takes an input string and produces a
          sequence of tokens called a " (emph "token stream") "."
       " A token is an indivisible element of the language being parsed (for
        example, identifiers, mathematical operators, and keywords are common
        tokens)."]
      [p "The second phase, the parser, takes the sequence of tokens and
          produces from it a " (emph "syntax tree") "."]
      [p "This division of labor is practical: the algorithms used to compute
          tokens and to compute syntax trees are quite different, so it makes
          sense to put them into different programs."]
      [p "In practice, both lexers and parsers are constructed from declarative
          specifications that describe the form of tokens and syntax trees,
          respectively."]
      [p "Go to the next page to learn how to write lexer specifications."]]]))

(defn page-2 []
  [v-box
   :children
   [[p "A token is a simple thing -- it's just a string with an associated
        label called a " (emph "token name") "."
     " Here's the token stream for the Hello World program:"]
    [:div.log-view {:style {:width "490px"
                            :font-size "13px"
                            :border "0px"}}
     (into [:div.token-list]
           (map batch-lexer/token->hiccup)
           tokens)]
    [gap :size "10px"]
    [p "How do we specify a lexer that will produce such a token stream? We
        provide rules describing the form of strings that should be given
        a particular token name.  For example, " [:code "VOID = void ;"]
     " is a rule that says tokens labeled " [:code "VOID"]
     " must be exactly the substring \"void\"."]
    [p "A more complex rule " [:code "IDENT = [a-z]+ ;"]
     " states that tokens labeled " [:code "IDENT"]
     " are sequences of lower-case alphabetical characters. Thus, you see that
      lexer rules in fact use regular expressions to describe the form of
      strings."]
    [p "Here is the complete specification that I used to produce the above
        token stream:"]
    [pre lexer-definition]
    [p "An important note is that the order of lexer rules matters.  When run
        on an input string, the lexer will start with the first rule and try
        each rule in turn until it finds a match, repeatedly doing this until
        the entire input string has been consumed."]
    [p "If ever a substring is
        encountered that does not match any rule, the lexer will report an
        error and halt."]]])

(defn page-3 []
  [v-box
   :style {:margin-right "15px"}
   :children
   [[p "With tokens in hand, our goal is to produce a tree that associates a
        label with every syntactic construct:"]
    [:div.parse-forest-view
     {:style {:background-color "#efefef"
              :padding "0px 20px 20px 0px"}}
     [parse-forest/forest-pane
      {:forest forest
       :string string
       :tokens tokens
       :decoration-map decoration-map
       :disabled-overlays nil
       :peek nil}]]
    [gap :size "10px"]
    [p "The above is just a stylized, graphical depiction of the actual tree
        data structure below, which uses nested vectors to make the hierarchy
        explicit. You'll notice that the structure also records string indices
        to associate tree nodes with the substrings they cover (e.g., 2:29
        means \"line 2, column 29\")."]
    [pre rendered-forest]
    [p "How do we specify a parser that will generate such a syntax tree?  We
        provide rules, called " (emph "productions")
     ", that describe each level of hierarchy in the tree."]
    [p "For example, " [:code "type = VOID ;"]
     " is a production that says a " [:code "VOID"]
     " token can be the child of of a node labeled " [:code "type"] "."]
    [p "The production " [:code "arg-list = LPAREN RPAREN ;"]
     " says that an " [:code "arg-list"]
     " node has two children, an " [:code "LPAREN"]
     " token followed by an " [:code "RPAREN"] " token."]
    [p "A complete parser specification must contain productions for every such
        parent-child relationship in its input. If a rule is missing, then the
        parser will be unable to produce a syntax tree for that input."]
    [p "Here is the complete specification that I used to produce the above
        tree:"]
    [pre grammar]
    [p "It's important to note that the rule for " [:code "function"]
     " is at the top of the specification since " [:code "function"]
     " is the root of the syntax tree. By convention, the left-hand side of the
      topmost production is called the " (emph "start symbol")
     " and must always correspond to the syntax tree root. Note, however, that
      the order of the remaining productions does not matter."]]])

(defn page-4 []
  [v-box
   :children
   [[p "We've just gone over the basics of creating lexers and parsers via
        declarative specifications.  So far, we have avoided going into any
        specifics about the particular tool you would use to write these
        specifications."]
    [p "To learn how to use Parsimony specifically,
        Select "
        [:span
         {:on-click (handler-fn (dispatch [:exec-command-from-menu :install-example-projects]))
          :style {:font-style "italic"
                  :text-decoration "underline"
                  :cursor "pointer"}}
         "Help \u2192 Install Example Projects"]
        " in the menu bar to install the example projects. Then follow along
         with the video available at "
        [:a {:href "https://parsimony-ide.github.io" :target "_blank"}
         "https://parsimony-ide.github.io"] "."]]])

(def pages
  [{:title "Introduction to Parsing"
    :page [page-0]}
   {:title "Two-Phase Approach"
    :page [page-1]}
   {:title "Lexer"
    :page [page-2]}
   {:title "Parser"
    :page [page-3]}
   {:title "Conclusion"
    :page [page-4]}])

(defn parsing-lesson-pane []
  (let [{-title :title page :page} (get pages @current-page (first pages))]
    [v-box
     :size "auto"
     :children
     [^{:key (str @current-page)}
      [scroller
       :child
       [v-box
        :children [[title @current-page -title]
                   [line]
                   [gap :size "10px"]
                   page]]]
      [footer]]]))

(defrecord ParsingLessonModal []
  IModal
  (show-modal [this]
    [v-box
     :size "auto"
     :class "parsing-lesson-modal"
     :height "600px"
     :width "520px"
     :children [[parsing-lesson-pane]]])

  (backdrop-closeable? [this]
    true))

(cljs.reader/register-tag-parser! "parsimony.views.parsing-lesson.ParsingLessonModal" map->ParsingLessonModal)

(defn parsing-lesson-modal []
  (ParsingLessonModal.))
