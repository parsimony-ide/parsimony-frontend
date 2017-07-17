(ns parsimony.lexer
  (:require [clojure.string :as str]
            [parsimony.util :refer [pprint-str flast]]
            [parsimony.console :as console]))

(defn step
  [{:keys [transitions] :as state} c]
  (letfn [(matches? [{:keys [to min max] :as t}]
            #_(console/debug "matches?" to min c max)
            (<= min c max))]
    (when-let [t (first (filter matches? transitions))]
      (:to t))))

(defn run
  "Return the length of the longest accepted run of the given string starting at the given offset"
  [{:keys [initial states] :as automaton} string offset]
  (let [l (count string)]
    (loop [p (get states initial)
           r 0
           offset offset
           max -1]
      #_(console/debug (pprint-str p) r offset max l)
      (if (<= offset l)
        (let [max (if (:accept p) r max)]
          (if (= offset l)
            max
            (if-let [p (get states (step p (.charAt string offset)))]
              (recur p (inc r) (inc offset) max)
              max)))
        max))))

(defn lex-error
  [fail-idx]
  {:reason :no-matching-token
   :fail-idx fail-idx})

(defn lex-error?
  [x]
  (contains? x :fail-idx))

(defn next-token
  [lex-infos s idx]
  (reduce
    (fn [[max-m max-label :as acc]
         [label {:keys [js-automaton]}]]
      (let [m (run js-automaton s idx)]
        (if (> m max-m)
          [m label]
          acc)))
    [0 nil]
    lex-infos))

(defn lex-fn
  [lex-infos]
  (letfn [(token-seq [s idx]
            (when (< idx (count s))
              (let [[m label] (next-token lex-infos s idx)]
                (if (some? label)
                  (let [new-idx (+ idx m)
                        payload {:string (subs s idx new-idx)
                                 :label label
                                 :start idx
                                 :end   new-idx}]
                    (cons payload (lazy-seq (token-seq s new-idx))))
                  (list (lex-error idx))))))]
    (fn [s] (token-seq s 0))))

(defn lex
  ([lex-infos string]
   (lex lex-infos string #{:ws :comment :line-comment}))
  ([lex-infos string discards]
   #_(console/debug "lex-infos =" (pprint-str lex-infos))
   #_(console/debug "string =" (pr-str string))
   (let [f (lex-fn lex-infos)
         result (f string)]
     #_(console/debug (pprint-str result))
     (remove #(contains? discards (:label %))
             result))))

(defn char-range->token-range
  "Given a character range, return the corresponding token-indexed range [i l].
   Return a token-indexed range only if the character range is exact.
   Otherwise, return nil."
  [tokens char-from char-to]
  (let [indexed-tokens (map-indexed vector tokens)
        first-index (ffirst (filter (fn [[i t]] (= char-from (:start t)))
                                    indexed-tokens))
        last-index (ffirst (filter (fn [[i t]] (= char-to (:end t)))
                                   indexed-tokens))]
    (if (and (some? first-index)
             (some? last-index)
             (<= first-index last-index))
      [first-index
       (inc (- last-index first-index))]
      (do (console/error "Unable to compute token-range"
                         (pprint-str {:indexed-tokens indexed-tokens
                                      :char-from char-from
                                      :char-to char-to
                                      :first-index first-index
                                      :last-index last-index}))
          nil))))

(defn inexact-char-range->token-range
  "Given a character range, return the corresponding token-indexed range [i l].
   Return the nearest token-indexed range if the character range is inexact."
  [tokens char-from char-to]
  ;; If char-from intersects a token, then the position before that token is
  ;; the start. Otherwise, the start is the position before the first token
  ;; after char-from.
  ;;
  ;; If char-to intersects a token, then the position after that token is the
  ;; end. Otherwise, the end is the position after the first token before
  ;; char-to.
  (let [indexed-tokens (map-indexed vector tokens)
        from-intersect (ffirst (filter (fn [[_ {:keys [start end]}]]
                                         (and (<= start char-from)
                                              (< char-from end)))
                                       indexed-tokens))
        first-index (if (some? from-intersect)
                      from-intersect
                      (flast (filter (fn [[_ {:keys [start]}]]
                                        (< char-from start))
                                      (reverse indexed-tokens))))
        to-intersect (ffirst (filter (fn [[_ {:keys [start end]}]]
                                       (and (< start char-to)
                                            (<= char-to end)))
                                     indexed-tokens))
        last-index (if (some? to-intersect)
                     to-intersect
                     (flast (filter (fn [[_ {:keys [end]}]]
                                      (<= end char-to))
                                    indexed-tokens)))]
    (if (and (some? first-index)
             (some? last-index)
             (<= first-index last-index))
      [first-index
       (inc (- last-index first-index))]
      (do (console/error "Unable to compute token-range"
                         (pprint-str {:indexed-tokens indexed-tokens
                                      :char-from char-from
                                      :char-to char-to
                                      :from-intersect from-intersect
                                      :first-index first-index
                                      :to-intersect to-intersect
                                      :last-index last-index}))
          [0 (count tokens)]))))

(defn token-range->char-range
  [tokens i l]
  (let [tokens (vec tokens)
        first-token (get tokens i)
        last-token (get tokens (dec (+ i l)))]
    (if (and (some? first-token)
             (some? last-token))
      [(:start first-token)
       (:end last-token)]
      (do (console/error "Unable to compute char-range"
                         (pprint-str {:tokens tokens
                                      :i i
                                      :l l
                                      :first-token first-token
                                      :last-token last-token}))
          nil))))

(defn all-syms [lex-infos]
  (vec (into (sorted-set)
             (map first)
             lex-infos)))
