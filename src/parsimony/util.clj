(ns parsimony.util)

(defn- -inspect
  ([expr]
   (-inspect (str expr) expr))
  ([msg expr]
   `(let [res# ~expr]
      (when *inspect*
        (println (str ~msg " =>") (pr-str res#)))
      res#)))

(defn- -inspect-pp
  ([expr]
   (-inspect-pp (str expr) expr))
  ([msg expr]
   `(let [res# ~expr]
      (when *inspect*
        (println (str ~msg " =>") (pprint-str res#)))
      res#)))

(defmacro inspect
  ([expr]
   (-inspect expr))
  ([msg expr]
   (-inspect msg expr)))

(defmacro inspect-pp
  ([expr]
   (-inspect-pp expr))
  ([msg expr]
   (-inspect-pp msg expr)))

(defmacro noinspect
  ([expr] expr)
  ([msg expr] expr))

(defmacro with-inspection [& body]
  `(binding [*inspect* true]
     ~@body))

(defmacro without-inspection [& body]
  `(binding [*inspect* nil]
     ~@body))
