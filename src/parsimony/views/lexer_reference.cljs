(ns parsimony.views.lexer-reference
  (:require [parsimony.views.modal :refer [IModal]]
            [re-com.core :refer [h-box v-box scroller line title gap] :refer-macros [handler-fn] :as re-com]))

(defn pre [s]
  [:pre {:style {:width "700px"
                 :font-size "12px"}}
   s])

(defn page []
  [pre
"A lexer definition is a sequence of lexer rules, with one lexer rule per line.

A lexer rule takes the form

    <token-name> = <regex> ;

That is, a lexer rule is
    - a token name, then
    - an equals sign, then
    - a regular expression, then
    - a semicolon.

<token-name> can be any alphanumeric string without whitespace.

<regex> describe the set of strings that should be recognized as instances of
<token-name>.

<regex> takes the following basic forms:
    - alphanumeric character (e.g., x),
    - escaped non-alphanumeric character: '\\' <char> (e.g., \\+, \\-)
    - character class: '[' ... ']', (e.g., [abc], [a-z], [0-9], [\\+\\-\\?]),

Additionally a <regex> can be built recursively from other <regex>es:
    - concatenation: <regex><regex>
    - grouping: '(' <regex> ')'
    - alternation: <regex> '|' <regex>
    - repetition of 1 or more (Kleene plus): <regex> '+'
    - repetition of 0 or more (Kleene star): <regex> '*'
    - single-character negation '!' <regex>
    - optional <regex> '?'

The order of lexer rules matters.  If two or more regular expressions match a
given token, the one highest up in the file will be the one that wins.

The following are all examples of lexer rules:

A = a ;                      // matches just the string 'a'
HEXCHAR = [a-f] ;            // matches 'a', 'b', 'c', 'd', 'e', or 'f'
OCTAL = [0-7]+ ;             // matches any sequence of 1 or more digits between 0 and 7
HELLOWORLD = hello | world ; // matches either 'hello' or 'world'
NOTX = !x ;                  // matches any single character that is not 'x'
NOTALPHA = ![a-zA-Z]+ ;      // matches any sequence of 1 or more non-alphabetical characters

// This one has a complex <regex> that has all the constructs:
COMPLEX = ([ab]+ | (x | y | 1)* 9)? foo !a* ;"])

(defn lexer-reference-pane []
  [scroller
   :size "auto"
   :child
   [v-box
    :children [[:div {:style {:font-size "25px"
                              :font-weight "lighter"}}
                "Lexer Reference"]
               [line]
               [gap :size "10px"]
               [page]]]])

(defrecord LexerReferenceModal []
  IModal
  (show-modal [this]
    [v-box
     :size "auto"
     :class "lexer-reference-modal"
     :height "800px"
     :width "730px"
     :children [[lexer-reference-pane]]])

  (backdrop-closeable? [this]
    true))

(cljs.reader/register-tag-parser! "parsimony.views.lexer-reference.LexerReferenceModal" map->LexerReferenceModal)

(defn lexer-reference-modal []
  (LexerReferenceModal.))


