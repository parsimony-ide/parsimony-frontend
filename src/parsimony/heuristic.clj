(ns parsimony.heuristic)

(defmacro with-cljs-query [& body]
  `(binding [*run-query-fn* run-cljs-query]
     ~@body))

(defmacro with-cpp-query [& body]
  `(binding [*run-query-fn* run-cpp-query]
     ~@body))
