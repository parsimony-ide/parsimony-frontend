(ns parsimony.heuristic-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [parsimony.common-test :refer [csharp-lexer]]
            [parsimony.heuristic :as heuristic :refer-macros [with-cljs-query with-cpp-query]]
            [parsimony.inference :as inference]
            [parsimony.inference-test :refer [run-parser]]
            [parsimony.parser :as parser]
            [parsimony.asm.parser :as asm.parser]
            [parsimony.console :as console]))

(def token-keys (into [] (map first) csharp-lexer))

(defn gen-state [grammar-str s nt]
  (let [parser (parser/definition-parser grammar-str token-keys)
        {:keys [codec cyk tokens] :as result} (run-parser csharp-lexer parser s)
        constraints (inference/sample->node-constraints
                      {:string s
                       :labels [{:nt nt :char-from 0 :char-to (count s) :type :positive :label-id 1}]}
                      tokens)
        constraint-state (inference/gen-one-constraint-state
                           codec
                           cyk
                           tokens
                           parser
                           constraints
                           0
                           (first (:positive constraints)))]
    (asm.parser/cpp-free cyk)
    constraint-state))

(defn- canonicalize-one [{:keys [path] :as x}]
  (let [sorted-path (into []
                          (map (comp vec sort))
                          path)]
    (assoc x :path sorted-path)))

(defn- canonicalize [xs]
  (->> xs
       (map canonicalize-one)
       (sort-by :path)
       (vec)))

(defn differential [heuristic-fn & args]
  (let [cljs-result (with-cljs-query (apply heuristic-fn args))
        cpp-result (with-cpp-query (apply heuristic-fn args))]
    (is (= (canonicalize cljs-result) (canonicalize cpp-result)))
    cljs-result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delimited-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest dl-test-0
  (let [constraint-state (gen-state "S = ident ;" "a" :S)
        result (differential heuristic/-delimited-list constraint-state)]
    #_(console/debug ::dl-test-0
                     {:constraint-state constraint-state
                      :result result})
    (is (= 0 (count result)))))

(deftest dl-test-1
  (let [constraint-state (gen-state "S = ident ;" "a,b,c" :S)
        result (differential heuristic/-delimited-list constraint-state)
        {:keys [elem sep]} (:params (first result))]
    #_(console/debug ::dl-test-1
                     {:constraint-state constraint-state
                      :result result})
    (is (= 1 (count result)))
    (is (= #{:S :%ident} elem))
    (is (= #{:%comma} sep))))

(deftest dl-test-2
  (let [constraint-state (gen-state "S = ident ;" "a,b" :S)
        result (differential heuristic/-delimited-list constraint-state)
        {:keys [elem sep]} (:params (first result))]
    #_(console/debug ::dl-test-2
                     {:constraint-state constraint-state
                      :result result})
    (is (= 1 (count result)))
    (is (= #{:S :%ident} elem))
    (is (= #{:%comma} sep))))

(deftest dl-test-3
  (let [constraint-state (gen-state "S = ident ; S = void ;" "a,b,void" :S)
        result (differential heuristic/-delimited-list constraint-state)
        {:keys [elem sep]} (:params (first result))]
    #_(console/debug ::dl-test-3
                     {:constraint-state constraint-state
                      :result result})
    (is (= 1 (count result)))
    (is (= #{:S} elem))
    (is (= #{:%comma} sep))))

(deftest dl-test-4
  (let [constraint-state (gen-state "S = ident ; S = void ; T = ident ;" "a,b,void" :S)
        result (differential heuristic/-delimited-list constraint-state)
        {:keys [elem sep]} (:params (first result))]
    #_(console/debug ::dl-test-4
                     {:constraint-state constraint-state
                      :result result})
    (is (= 1 (count result)))
    (is (= #{:S} elem))
    (is (= #{:%comma} sep))))

(deftest dl-test-5
  (let [constraint-state (gen-state "S = ident ; S = void ; T = ident ; T = void ;" "a,b,void" :S)
        result (differential heuristic/-delimited-list constraint-state)
        {:keys [elem sep]} (:params (first result))]
    #_(console/debug ::dl-test-5
                     {:constraint-state constraint-state
                      :result result})
    (is (= 1 (count result)))
    (is (= #{:S :T} elem))
    (is (= #{:%comma} sep))))

(deftest dl-test-6
  (let [constraint-state (gen-state "S = ident ; S = ident comma ident ;" "a,b,c" :S)
        result (differential heuristic/-delimited-list constraint-state)]
    #_(console/debug ::dl-test-6
                     {:constraint-state constraint-state
                      :result result})
    (is (= 2 (count result)))
    (is (= #{:S} (get-in (first result) [:params :elem]) (get-in (second result) [:params :elem])))
    (is (= #{:%comma} (get-in (first result) [:params :sep]) (get-in (second result) [:params :sep])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undelimited-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest ul-test-0
  (let [constraint-state (gen-state "S = ident ;" "a" :S)
        result (differential heuristic/-undelimited-list constraint-state)]
    #_(console/debug ::ul-test-0
                     {:constraint-state constraint-state
                      :result result})
    (is (= 0 (count result)))))

(deftest ul-test-1
  (let [constraint-state (gen-state "S = ident ;" "a b c" :S)
        result (differential heuristic/-undelimited-list constraint-state)
        {:keys [elem]} (:params (first result))]
    #_(console/debug ::ul-test-1
                     {:constraint-state constraint-state
                      :result result})
    (is (= 1 (count result)))
    (is (= #{:S :%ident} elem))))

(deftest ul-test-2
  (let [constraint-state (gen-state "S = ident ;" "a b" :S)
        result (differential heuristic/-undelimited-list constraint-state)
        {:keys [elem]} (:params (first result))]
    #_(console/debug ::ul-test-2
                     {:constraint-state constraint-state
                      :result result})
    (is (= 1 (count result)))
    (is (= #{:S :%ident} elem))))

(deftest ul-test-3
  (let [constraint-state (gen-state "S = ident ; S = void ;" "a b void" :S)
        result (differential heuristic/-undelimited-list constraint-state)
        {:keys [elem]} (:params (first result))]
    #_(console/debug ::ul-test-3
                     {:constraint-state constraint-state
                      :result result})
    (is (= 1 (count result)))
    (is (= #{:S} elem))))

(deftest ul-test-4
  (let [constraint-state (gen-state "S = ident ; S = void ; T = ident ;" "a b void" :S)
        result (differential heuristic/-undelimited-list constraint-state)
        {:keys [elem]} (:params (first result))]
    #_(console/debug ::ul-test-4
                     {:constraint-state constraint-state
                      :result result})
    (is (= 1 (count result)))
    (is (= #{:S} elem))))

(deftest ul-test-5
  (let [constraint-state (gen-state "S = ident ; S = void ; T = ident ; T = void ;" "a b void" :S)
        result (differential heuristic/-undelimited-list constraint-state)
        {:keys [elem]} (:params (first result))]
    #_(console/debug ::ul-test-5
                     {:constraint-state constraint-state
                      :result result})
    (is (= 1 (count result)))
    (is (= #{:S :T} elem))))

(deftest ul-test-6
  (let [constraint-state (gen-state "S = ident ; S = ident ident ;" "a b c" :S)
        result (differential heuristic/-undelimited-list constraint-state)]
    #_(console/debug ::ul-test-6
                     {:constraint-state constraint-state
                      :result result})
    (is (= 2 (count result)))
    (is (= #{:S} (get-in (first result) [:params :elem]) (get-in (second result) [:params :elem])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enclosed-delimited-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest edl-test-0
  (let [constraint-state (gen-state "S = ident ;" "[a]" :S)
        result (differential heuristic/-enclosed-delimited-list constraint-state csharp-lexer)]
    #_(console/debug ::edl-test-0
                     {:constraint-state constraint-state
                      :result result})
    (is (= 0 (count result)))))

(deftest edl-test-1
  (let [constraint-state (gen-state "S = ident ;" "[a,b,c]" :S)
        result (differential heuristic/-enclosed-delimited-list constraint-state csharp-lexer)
        {:keys [elem sep encloser]} (:params (first result))]
    #_(console/debug ::edl-test-1
                     {:constraint-state constraint-state
                      :result result})
    (is (= 1 (count result)))
    (is (= #{:S :%ident} elem))
    (is (= #{:%comma} sep))
    (is (= #{[:%lsquare :%rsquare]} encloser))))

(deftest edl-test-2
  (let [constraint-state (gen-state "S = ident ;" "{a,b}" :S)
        result (differential heuristic/-enclosed-delimited-list constraint-state csharp-lexer)
        {:keys [elem sep encloser]} (:params (first result))]
    #_(console/debug ::edl-test-2
                     {:constraint-state constraint-state
                      :result result})
    (is (= 1 (count result)))
    (is (= #{:S :%ident} elem))
    (is (= #{:%comma} sep))
    (is (= #{[:%lbrace :%rbrace]} encloser))))

(deftest edl-test-3
  (let [constraint-state (gen-state "S = ident ; S = void ;" "(a;b;void)" :S)
        result (differential heuristic/-enclosed-delimited-list constraint-state csharp-lexer)
        {:keys [elem sep encloser]} (:params (first result))]
    #_(console/debug ::edl-test-3
                     {:constraint-state constraint-state
                      :result result})
    (is (= 1 (count result)))
    (is (= #{:S} elem))
    (is (= #{:%semi} sep))
    (is (= #{[:%lparen :%rparen]} encloser))))

(deftest edl-test-4
  (let [constraint-state (gen-state "S = ident ; S = void ; T = ident ;" "[a.b.void]" :S)
        result (differential heuristic/-enclosed-delimited-list constraint-state csharp-lexer)
        {:keys [elem sep encloser]} (:params (first result))]
    #_(console/debug ::edl-test-4
                     {:constraint-state constraint-state
                      :result result})
    (is (= 1 (count result)))
    (is (= #{:S} elem))
    (is (= #{:%dot} sep))
    (is (= #{[:%lsquare :%rsquare]} encloser))))

(deftest edl-test-5
  (let [constraint-state (gen-state "S = ident ; S = void ; T = ident ; T = void ;" "[a,b,void]" :S)
        result (differential heuristic/-enclosed-delimited-list constraint-state csharp-lexer)
        {:keys [elem sep encloser]} (:params (first result))]
    #_(console/debug ::edl-test-5
                     {:constraint-state constraint-state
                      :result result})
    (is (= 1 (count result)))
    (is (= #{:S :T} elem))
    (is (= #{:%comma} sep))
    (is (= #{[:%lsquare :%rsquare]} encloser))))

(deftest edl-test-6
  (let [constraint-state (gen-state "S = ident ; S = ident semi ident ;" "[a;b;c]" :S)
        result (differential heuristic/-enclosed-delimited-list constraint-state csharp-lexer)]
    #_(console/debug ::edl-test-6
                     {:constraint-state constraint-state
                      :result result})
    (is (= 2 (count result)))
    (is (= #{:S} (get-in (first result) [:params :elem]) (get-in (second result) [:params :elem])))
    (is (= #{:%semi} (get-in (first result) [:params :sep]) (get-in (second result) [:params :sep])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enclosed-undelimited-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: nested lists
;; TODO: list next to list

(deftest eudl-test-0
  (let [constraint-state (gen-state "S = ident ;" "[a]" :S)
        result (differential heuristic/-enclosed-undelimited-list constraint-state csharp-lexer)]
    #_(console/debug ::eudl-test-0
                     {:constraint-state constraint-state
                      :result result})
    (is (= 0 (count result)))))

(deftest eudl-test-1
  (let [constraint-state (gen-state "S = ident ;" "[a b c]" :S)
        result (differential heuristic/-enclosed-undelimited-list constraint-state csharp-lexer)
        {:keys [elem encloser]} (:params (first result))]
    #_(console/debug ::eudl-test-1
                     {:constraint-state constraint-state
                      :result result})
    (is (= 1 (count result)))
    (is (= #{:S :%ident} elem))
    (is (= #{[:%lsquare :%rsquare]} encloser))))

(deftest eudl-test-2
  (let [constraint-state (gen-state "S = ident ;" "{a b}" :S)
        result (differential heuristic/-enclosed-undelimited-list constraint-state csharp-lexer)
        {:keys [elem encloser]} (:params (first result))]
    #_(console/debug ::eudl-test-2
                     {:constraint-state constraint-state
                      :result result})
    (is (= 1 (count result)))
    (is (= #{:S :%ident} elem))
    (is (= #{[:%lbrace :%rbrace]} encloser))))

(deftest eudl-test-3
  (let [constraint-state (gen-state "S = ident ; S = void ;" "(a b void)" :S)
        result (differential heuristic/-enclosed-undelimited-list constraint-state csharp-lexer)
        {:keys [elem encloser]} (:params (first result))]
    #_(console/debug ::eudl-test-3
                     {:constraint-state constraint-state
                      :result result})
    (is (= 1 (count result)))
    (is (= #{:S} elem))
    (is (= #{[:%lparen :%rparen]} encloser))))

(deftest eudl-test-4
  (let [constraint-state (gen-state "S = ident ; S = void ; T = ident ;" "[a b void]" :S)
        result (differential heuristic/-enclosed-undelimited-list constraint-state csharp-lexer)
        {:keys [elem encloser]} (:params (first result))]
    #_(console/debug ::eudl-test-4
                     {:constraint-state constraint-state
                      :result result})
    (is (= 1 (count result)))
    (is (= #{:S} elem))
    (is (= #{[:%lsquare :%rsquare]} encloser))))

(deftest eudl-test-5
  (let [constraint-state (gen-state "S = ident ; S = void ; T = ident ; T = void ;" "[a b void]" :S)
        result (differential heuristic/-enclosed-undelimited-list constraint-state csharp-lexer)
        {:keys [elem encloser]} (:params (first result))]
    #_(console/debug ::eudl-test-5
                     {:constraint-state constraint-state
                      :result result})
    (is (= 1 (count result)))
    (is (= #{:S :T} elem))
    (is (= #{[:%lsquare :%rsquare]} encloser))))

(deftest eudl-test-6
  (let [constraint-state (gen-state "S = ident ; S = ident ident ;" "[a b c]" :S)
        result (differential heuristic/-enclosed-undelimited-list constraint-state csharp-lexer)]
    #_(console/debug ::eudl-test-6
                     {:constraint-state constraint-state
                      :result result})
    (is (= 2 (count result)))
    (is (= #{:S} (get-in (first result) [:params :elem]) (get-in (second result) [:params :elem])))
    (is (= #{[:%lsquare :%rsquare]} (get-in (first result) [:params :encloser]) (get-in (second result) [:params :encloser])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest expression-test-0
  (let [constraint-state (gen-state "S = ident ;" "a+a" :S)
        result (differential heuristic/-expression constraint-state csharp-lexer)
        {:keys [lhs-nt op paren]} (:params (first result))]
    #_(console/debug ::expression-test-0
                     {:constraint-state constraint-state
                      :result result})
    (is (= 1 (count result)))
    (is (= :S lhs-nt))
    (is (= #{:%plus} op))
    (is (= nil paren))))

(deftest expression-test-1
  (let [constraint-state (gen-state "S = ident ;" "(a+a)" :S)
        result (differential heuristic/-expression constraint-state csharp-lexer)
        {:keys [lhs-nt op paren]} (:params (first result))]
    #_(console/debug ::expression-test-1
                     {:constraint-state constraint-state
                      :result result})
    (is (= 1 (count result)))
    (is (= :S lhs-nt))
    (is (= #{:%plus} op))
    (is (= #{[:%lparen :%rparen]} paren))))

(deftest expression-test-2
  (let [constraint-state (gen-state "S = ident ;" "{a+a*a}" :S)
        result (differential heuristic/-expression constraint-state csharp-lexer)
        {:keys [lhs-nt op paren]} (:params (first result))]
    #_(console/debug ::expression-test-2
                     {:constraint-state constraint-state
                      :result result})
    (is (= 1 (count result)))
    (is (= :S lhs-nt))
    (is (= #{:%plus :%times} op))
    (is (= #{[:%lbrace :%rbrace]} paren))))

(deftest expression-test-3
  (let [constraint-state (gen-state "S = ident ;" "(a+a)-a" :S)
        result (differential heuristic/-expression constraint-state csharp-lexer)
        {:keys [lhs-nt op paren]} (:params (first result))]
    #_(console/debug ::expression-test-3
                     {:constraint-state constraint-state
                      :result result})
    (is (= 1 (count result)))
    (is (= :S lhs-nt))
    (is (= #{:%plus :%minus} op))
    (is (= #{[:%lparen :%rparen]} paren))))

(deftest expression-test-4
  (let [constraint-state (gen-state "S = ident ;" "(a+a)-[a/a]" :S)
        result (differential heuristic/-expression constraint-state csharp-lexer)
        {:keys [lhs-nt op paren]} (:params (first result))]
    #_(console/debug ::expression-test-4
                     {:constraint-state constraint-state
                      :result result})
    (is (= 1 (count result)))
    (is (= :S lhs-nt))
    (is (= #{:%plus :%minus :%div} op))
    (is (= #{[:%lparen :%rparen]
             [:%lsquare :%rsquare]}
           paren))))
