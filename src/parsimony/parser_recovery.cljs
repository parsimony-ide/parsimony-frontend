(ns parsimony.parser-recovery
  "Error recovery for parsimony.parser implementation"
  (:require [parsimony.parser :as parser]
            [parsimony.util :refer [string-idx->line-col]]
            [parsimony.console :as console]))

(declare greedy-prefix keep-longest-prefixes)

(defn- extend-prefix
  "Return sequence of all prefixes derived by extending prefix with nt"
  [ps cyk-table token-vec sym prefix seen]
  (let [[last-sym i l :as last-segment] (peek prefix)
        i' (+ i l)
        result
        (cond
          ;; end sentinel exists, cannot extend path any further
          (= last-sym :-end-sentinel)
          [prefix]
          ;; sym is nil, end of production found
          (not sym)
          [(conj prefix [:-end-sentinel i' 0 nil])]
          ;; otherwise
          :else
          (let [n (- (count token-vec) i')]
            (if-let [extensions
                     (seq (for [k (range 1 (inc n))
                                :when (parser/applicable? cyk-table token-vec sym i' k)]
                            [sym i' k]))]
              ;; extension(s) to prefix exist
              (into []
                    (map #(conj prefix %))
                    extensions)
              ;; no extensions exist
              (if-not (contains? seen sym)
                ;; have not yet recursed on sym at this location
                (if-let [sub-productions (seq (parser/productions-with-lhs ps sym))]
                  ;; sub-productions exists, recurse into each sub-production
                  (do #_(console/debug ::recurse {:sub-productions sub-productions})
                      (into []
                            (mapcat (fn [prod] (greedy-prefix ps cyk-table token-vec prod [prefix] (conj seen sym))))
                            sub-productions))
                  ;; sub-productions do not exist
                  [(conj prefix [:-end-sentinel i' 0 sym])])
                ;; have already recursed on sym at this location, cycle found so exit recursion
                (do #_(console/debug ::exit-recursion {:sym sym :seen seen})
                    nil)))))]
    #_(console/debug ::extend-prefix {:prefix prefix :sym sym :result result})
    result))

(defn- greedy-prefix [ps cyk-table token-vec prod prefixes seen]
  #_(console/debug ::greedy-prefix prod)
  (let [body (parser/rhs prod)]
    (reduce
      (fn [acc sym]
        (keep-longest-prefixes
          (into []
                (mapcat #(extend-prefix ps cyk-table token-vec sym % seen))
                acc)))
      prefixes
      (conj body nil))))

(defn- keep-longest-prefixes [prefixes]
  (let [n (reduce max
                  0
                  (for [prefix prefixes
                        :let [[sym i _] (peek prefix)]
                        :when (= :-end-sentinel sym)]
                    i))]
    #_(console/debug ::keep-longest-prefixes {:n n})
    (into []
          (remove (fn [prefix]
                    (let [[sym i _] (peek prefix)]
                      (and (= sym :-end-sentinel)
                           (not= i n)))))
          prefixes)))

(defn compute-longest-correct-prefixes
  [parser cyk-table token-vec]
  (let [ps (parser/expand-optionals (:productions parser))
        start-nt (parser/start-symbol parser)]
    (if (parser/applicable? cyk-table token-vec start-nt 0 (count token-vec))
      [[start-nt 0 (count token-vec)]]
      (let [start-productions (parser/productions-with-lhs ps start-nt)
            all-prefixes
            (into []
                  (mapcat (fn [prod] (greedy-prefix ps cyk-table token-vec prod [[[:-start-sentinel 0 0]]] #{})))
                  start-productions)]
        (keep-longest-prefixes all-prefixes)))))

(defn- instantiate-error-template [line col next-token expected]
  (if (some? next-token)
    [:div
     [:span (str "Parse failure at line " line ":" col ". Extraneous token "
                 "'" (:string next-token) "'" " with label ")
      [:code (name (:label next-token))]]
     expected]
    [:div
     [:span (str "Parse failure at line " line ":" col ". End of input found.")]
     expected]))

(defn- fail-token->html
  [sym]
  (if (nil? sym)
    [:span "end of file"]
    [:code (name (parser/->nonterminal sym))]))

(defn prefixes->hiccup-msg
  "Return a hiccup representation of the error message for prefixes. Return nil if no error."
  [string raw-tokens prefixes]
  (let [raw-tokens (vec raw-tokens)]
    (if-let [end-sentinels (seq (map peek prefixes))]
      ;; prefixes exist
      (let [fail-idx (second (first end-sentinels))
            fail-tokens (into #{}
                              (map peek)
                              end-sentinels)
            next-token (get raw-tokens fail-idx)
            {:keys [line col]} (string-idx->line-col string (:start next-token))]
        (console/debug ::prefixes->hiccup-msg {:prefixes prefixes
                                               :end-sentinels end-sentinels
                                               :fail-idx fail-idx
                                               :fail-tokens fail-tokens
                                               :next-token next-token
                                               :raw-tokens raw-tokens})
        (when (seq fail-tokens)
          (instantiate-error-template line col next-token
                                      (if (> (count fail-tokens) 1)
                                        [:div.parser-failure
                                         "Expected one of the following instead: "
                                         (into [:ul]
                                               (map (comp (partial vector :li)
                                                          fail-token->html))
                                               fail-tokens)]
                                        [:div.parser-failure
                                         "Expected " (fail-token->html (first fail-tokens)) " instead"]))))
      ;; no prefixes exist
      (let [first-token (first raw-tokens)
            {:keys [line col]} (string-idx->line-col string (:start first-token))]
        [:div
         [:span (str "Parse failure at line " line ":" col
                     ". All productions for the start symbol are useless: they cannot match anything.")]]))))
