(ns parsimony.asm.parser-recovery
  "Error recovery for parsimony.asm.parser implementation"
  (:require [parsimony.asm.parser :as asm.parser]
            [parsimony.parser :as parser]
            [parsimony.parser-recovery :refer [keep-longest-prefixes]]
            [parsimony.console :as console]))

(declare greedy-prefix)

(defn- extend-prefix
  [ps codec cyk token-vec sym prefix seen]
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
                                :when (asm.parser/applicable? codec cyk token-vec sym i' k)]
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
                            (mapcat (fn [prod] (greedy-prefix ps codec cyk token-vec prod [prefix] (conj seen sym))))
                            sub-productions))
                  ;; sub-productions do not exist
                  [(conj prefix [:-end-sentinel i' 0 sym])])
                ;; have already recursed on sym at this location, cycle found so exit recursion
                (do #_(console/debug ::exit-recursion {:sym sym :seen seen})
                    nil)))))]
    #_(console/debug ::extend-prefix {:prefix prefix :sym sym :result result})
    result))

(defn- greedy-prefix
  [ps codec cyk token-vec prod prefixes seen]
  #_(console/debug ::greedy-prefix prod)
  (let [body (parser/rhs prod)]
    (reduce
      (fn [acc sym]
        (keep-longest-prefixes
          (into []
                (mapcat #(extend-prefix ps codec cyk token-vec sym % seen))
                acc)))
      prefixes
      (conj body nil))))

(defn compute-longest-correct-prefixes
  [parser codec cyk token-vec]
  (let [ps (parser/expand-optionals (:productions parser))
        start-nt (parser/start-symbol parser)]
    (if (asm.parser/applicable? codec cyk token-vec start-nt 0 (count token-vec))
      [[start-nt 0 (count token-vec)]]
      (let [start-productions (parser/productions-with-lhs ps start-nt)
            all-prefixes
            (into []
                  (mapcat (fn [prod] (greedy-prefix ps codec cyk token-vec prod [[[:-start-sentinel 0 0]]] #{})))
                  start-productions)]
        #_(console/debug ::compute-longest-correct-prefixes
                         {:start-productions start-productions
                          :all-prefixes all-prefixes})
        (keep-longest-prefixes all-prefixes)))))
