(ns parsimony.views.parser-reference
  (:require [parsimony.views.modal :refer [IModal]]
            [re-com.core :refer [h-box v-box scroller line title gap] :refer-macros [handler-fn] :as re-com]))

(defn pre [s]
  [:pre {:style {:width "700px"
                 :font-size "12px"}}
   s])

(defn page []
  [pre
"A parser definition (equivalently, grammar) is a sequence of productions, with
one production per line.

1. Productions
==============

A production takes the form

    <nonterminal> = (<token-name> | <nonterminal>)+ ;

That is, a production is
    - a nonterminal, then
    - an equals sign, then
    - a sequence of 1 or more symbols, where a symbol can either be a token name
      or a nonterminal, then
    - a semicolon.

<nonterminal> can be any non-empty sequence of alphabetical, numerical, or dash
characters.

<token-name> must be one of the token names defined in your lexer definition.

Note that there are no epsilon (empty) productions in this format. All
productions must have a non-empty body.

Also note that there are no alternations in this format. To create an
alternative, simply specify another production with the same left hand side
nonterminal.

2. Left Associativity
=====================

To specify that a production is left-associative, add '{left}' immediately before
the terminating semicolon:

    <nonterminal> = ... {left} ;

To specify that a group of productions are mutually left-associative, group them
like so:

left {
  <production>
  <production>
  ...
  <production>
}

All productions in <list of productions> must already be defined as described
in Section 1.

The productions inside an associativity group are treated with equal priority.

3. Right Associativity
======================

Same as Section 2, but use 'right' instead of 'left'.

4. Priority
===========

Place all priority declarations inside a block like so:

priorities {
  <production> > <production> ;
  <production> > <production> ;
  ...
  <production> > <production> ;
}

At most one priority block may be defined.  All productions mentioned in a
priority block must already be defined as described in Section 1.

Example:

priorities {
  foo = foo X foo > foo = foo Y foo ;
}

Priorities defined on a member of an associativity group automatically extend
to the other productions of that associativity group.

5. Start Symbol
===============

By default, the left-hand side of the first production is treated as the start
symbol.  To override this, you may add an annotation to the first line of the
grammar:

    {-# start=<symbol> #-}

where <symbol> is the name of the start symbol you wish to use."])

(defn parser-reference-pane []
  [scroller
   :size "auto"
   :child
   [v-box
    :children [[:div {:style {:font-size "25px"
                              :font-weight "lighter"}}
                "Parser Reference"]
               [line]
               [gap :size "10px"]
               [page]]]])

(defrecord ParserReferenceModal []
  IModal
  (show-modal [this]
    [v-box
     :size "auto"
     :class "parser-reference-modal"
     :height "800px"
     :width "730px"
     :children [[parser-reference-pane]]])

  (backdrop-closeable? [this]
    true))

(cljs.reader/register-tag-parser! "parsimony.views.parser-reference.ParserReferenceModal" map->ParserReferenceModal)

(defn parser-reference-modal []
  (ParserReferenceModal.))

