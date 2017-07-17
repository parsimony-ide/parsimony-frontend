(ns parsimony.handlers.dna)

(defmacro hf->
  "Thread a db through a sequence of hf functions, and return the final db."
  [db & forms]
  (loop [x (vector db), forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (with-meta `(parsimony.util/rapply ~(first form) ~x ~@(next form)) (meta form))
                       (list `parsimony.util/rapply form x))]
        (recur threaded (next forms)))
      x)))
