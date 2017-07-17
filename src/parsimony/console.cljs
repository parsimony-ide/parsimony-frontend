(ns parsimony.console
  (:require [cljs.pprint :refer [pprint]]
            [clojure.string :as str]
            [parsimony.config]
            [shodan.console])
  (:require-macros [parsimony.console]))

;; must duplicate pprint-str from parsimony.util to break circular dependency
(defn pprint-str [x]
  (enable-console-print!)
  (with-out-str (pprint x)))

(defn ->str [x]
  (cond
    ;; collection
    (or (vector? x) (list? x) (map? x))
    (str "\n" (pprint-str x) "\n")

    ;; keyword
    (keyword? x)
    (if-let [n (namespace x)]
      (let [short-n (as-> n x
                      (str/split x ".")
                      (take-last 2 x)
                      (remove (partial = "parsimony") x)
                      (str/join "." x))]
      (str ":" short-n "/" (name x)))
      (str x))

    ;; default
    :else
    (str x)))

(defn xs->str [& xs]
  (->> xs
       (map ->str)
       (str/join " ")
       (str/trim)))
