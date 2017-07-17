(ns parsimony.inference-test
  (:require [cljs.pprint :refer [pprint]]
            [cljs.test :refer-macros [deftest is testing]]
            [clojure.set :as set]
            [parsimony.common-test :refer [csharp-lexer
                                           xyz-lexer
                                           aab-lexer aab-parser
                                           if-else-lexer if-else-parser if-else-sample]]
            [parsimony.dag :as dag]
            [parsimony.inference :as infer]
            [parsimony.lexer :as lexer]
            [parsimony.asm.parser :as asm.parser]
            [parsimony.parser :as parser]
            [parsimony.util :refer [*inspect* pprint-str]
             :refer-macros [inspect inspect-pp noinspect with-inspection without-inspection]]))

(def csharp-cfg-str-1
  "Qual-ident = ident ;")

(def csharp-parser-1
  (parser/definition-parser csharp-cfg-str-1 (map first csharp-lexer)))

(def csharp-cfg-str-2
  "
  Qual-ident = ident ;
  Qual-ident = Qual-ident dot ident ;
  ")

(def csharp-parser-2
  (parser/definition-parser csharp-cfg-str-2 (map first csharp-lexer)))

(def csharp-cfg-str-3
  "
  Qual-ident = ident ;
  Qual-ident = ident dot Qual-ident ;
  ")

(def csharp-parser-3
  (parser/definition-parser csharp-cfg-str-3 (map first csharp-lexer)))

;; no nesting
(def samples-1
  [{:string
    "using System;\nusing System.Collections.Generic;\nusing System.Linq;",
    :labels
    [{:nt :Qual-ident :char-from 6  :char-to 12 :type :positive :label-id 1} ;; System on line 1
     {:nt :Qual-ident :char-from 20 :char-to 38 :type :positive :label-id 2} ;; System.Collections on line 2
     {:nt :Qual-ident :char-from 20 :char-to 46 :type :positive :label-id 3} ;; System.Collections.Generic on line 2
     {:nt :Qual-ident :char-from 27 :char-to 46 :type :negative :label-id 4} ;; Collections.Generic on line 2
     ]}])

;; one instance of nesting
(def samples-3
  [{:string
    "using System;\nusing System.Collections.Generic;\nusing System.Linq;",
    :labels
    [{:nt :Qual-ident :char-from 20 :char-to 46 :type :positive :label-id 3}
     {:nt :Qual-ident :char-from 20 :char-to 38 :type :positive :label-id 4}
     {:nt :Qual-ident :char-from 54 :char-to 65 :type :positive :label-id 5}]}])

(defn run-parser
  "Return map with structure {:cyk :codec :tokens}"
  [lexer parser string]
  (let [tokens (lexer/lex lexer string)]
    (assoc (infer/run-parser-unconstrained parser tokens)
           :tokens tokens)))

(defn check-sample [lexer parser sample]
  (let [tokens (lexer/lex lexer (:string sample))]
    (into {}
          (map (fn [[k v]]
                 [k (select-keys v [:top-down :bottom-up])]))
          (infer/check-sample {:parser parser
                               :tokens tokens
                               :parse  (infer/run-parser-unconstrained parser tokens)}
                              sample))))

(deftest check-sample-1
  (is (= {1 {:top-down true :bottom-up true}
          2 {:top-down false :bottom-up false}
          3 {:top-down false :bottom-up false}
          4 {:top-down false :bottom-up true}}
         (check-sample csharp-lexer csharp-parser-1 (first samples-1))))
  (is (= {1 {:top-down true :bottom-up true}
          2 {:top-down true :bottom-up true}
          3 {:top-down true :bottom-up true}
          4 {:top-down false :bottom-up false}}
         (check-sample csharp-lexer csharp-parser-2 (first samples-1))))
  (is (= {1 {:top-down true :bottom-up true}
          2 {:top-down true :bottom-up true}
          3 {:top-down false :bottom-up true}
          4 {:top-down false :bottom-up false}}
         (check-sample csharp-lexer csharp-parser-3 (first samples-1)))))

(deftest sample->node-constraints-1
  (without-inspection
    (let [{:keys [codec cyk tokens]} (run-parser csharp-lexer csharp-parser-1 (:string (first samples-1)))
          constraints (infer/sample->node-constraints (first samples-1) tokens)]
      (inspect-pp constraints))))

(deftest solve-constraints-1
  (with-inspection
    (let [parser (parser/definition-parser
                   ";; E = E y E ;
                    ;; E = E z E ;
                    E = x ;"
                   #{:x :y :z})
          gen (fn [s constraints]
                (let [{:keys [codec cyk tokens]}
                      (run-parser xyz-lexer parser s)]
                  (into []
                        (map #(infer/gen-one-constraint-state codec cyk tokens parser [0 %] constraints %))
                        (:positive constraints))))
          cs1 (gen "xyxyxzx"
                  {:positive #{[:E 0 7]
                               [:E 0 5]
                               [:E 0 3]
                               [:E 0 1]}})
          cs2 (gen "xyxyx" {:positive #{[:E 0 5]
                                        [:E 2 3]
                                        [:E 0 1]
                                        [:E 2 1]
                                        }})
          cs (into cs1 cs2)
          cs3 (infer/learn-partitions cs)]

      (doseq [c cs3]
        (inspect-pp (:root c))
        (inspect-pp (infer/solve c))))))
