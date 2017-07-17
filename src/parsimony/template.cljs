(ns parsimony.template
  (:require [clojure.set :as set]
            [parsimony.util :refer [pprint-str] :refer-macros [inspect-pp]]))

(defn drop-n-right [n [lhs rhs]]
  (when (> (count rhs) n)
    (let [new-rhs (into []
                        (take (- (count rhs) n))
                        rhs)]
      (when (not= [lhs] new-rhs)
        [[lhs new-rhs]]))))

(defn drop-n-left [n [lhs rhs]]
  (when (> (count rhs) n)
    (let [new-rhs (into []
                        (drop n)
                        rhs)]
      (when (not= [lhs] new-rhs)
        [[lhs new-rhs]]))))

(defn factor-inner [recursive? [lhs rhs]]
  (when (>= (count rhs) 3)
    (let [inner (into []
                      (comp (take (dec (count rhs)))
                            (drop 1)) rhs)
          inner-nt (if recursive?
                     lhs
                     (keyword (str (name lhs) "-inner")))
          new-rhs (-> []
                      (conj (first rhs))
                      (conj inner-nt)
                      (conj (last rhs)))]
      (when (and (not= [lhs] new-rhs)
                 (not= [inner-nt] inner))
        [[lhs new-rhs]
         [inner-nt inner]]))))

(defn factor-left [recursive? [lhs rhs]]
  (when (>= (count rhs) 3)
    (let [left (into []
                     (take (dec (count rhs)))
                     rhs)
          left-nt (if recursive?
                    lhs
                    (keyword (str (name lhs) "-left")))
          new-rhs (-> []
                      (conj left-nt)
                      (conj (last rhs)))]
      (when (and (not= [lhs] new-rhs)
                 (not= [left-nt] left))
        [[lhs new-rhs]
         [left-nt left]]))))

(defn -instantiate-templates [productions]
  (-> #{}
      (into (mapcat (partial drop-n-right 1)) productions)
      (into (mapcat (partial drop-n-right 2)) productions)
      (into (mapcat (partial drop-n-left 1)) productions)
      (into (mapcat (partial drop-n-left 2)) productions)
      (into (mapcat (partial factor-inner false) productions))
      (into (mapcat (partial factor-left false) productions))
      (into (mapcat (partial factor-inner true) productions))
      (into (mapcat (partial factor-left true) productions))))

(defn instantiate-templates [original-productions multigrammar-productions]
  (let [productions (-> #{}
                        (into original-productions)
                        (into (map :production (:head multigrammar-productions))))
        productions-1 (-instantiate-templates productions)
        productions-2 (-instantiate-templates productions-1)]
    #_(inspect-pp {:productions productions
                   :productions-1 productions-1
                   :productions-2 productions-2})
    (set/union productions-1 productions-2)))
