(ns parsimony.console)

(defn- -output [fname xs]
  `(let [res# (xs->str ~@xs)]
     (~fname res#)))

(defn- -output-group [fname x xs]
  (if (seq xs)
    `(shodan.console/with-group (->str ~x)
       ~(-output fname xs))
    `(shodan.console/with-group-collapsed (->str ~x))))

(defn- -output-group-collapsed [fname x xs]
  (if (seq xs)
    `(shodan.console/with-group-collapsed (->str ~x)
       ~(-output fname xs))
    `(shodan.console/with-group-collapsed (->str ~x))))

;; XXX: Need to insert runtime check because parsimony.config/BUILD is only
;; available from ClojureScript, not Clojure. Would be nicer to elide debug
;; calls at macro-expansion time though.
(defmacro debug [x & xs]
  `(when (= "dev" parsimony.config/BUILD)
     ~(-output-group 'shodan.console/debug x xs)))

(defmacro debug-collapsed [x & xs]
  `(when (= "dev" parsimony.config/BUILD)
     ~(-output-group-collapsed 'shodan.console/debug x xs)))

(defmacro warn [& xs]
  (-output 'shodan.console/warn xs))

(defmacro error [& xs]
  (-output 'shodan.console/error xs))
