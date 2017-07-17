(ns parsimony.refactor.parser
  (:require [instaparse.core :as insta]
            [cljs.pprint :refer [pprint]]
            [clojure.set :as set]
            [clojure.string :as str]
            [parsimony.dag :as dag]
            [parsimony.parser :as parser]
            [parsimony.union-find :as uf]
            [parsimony.util :refer [pprint-str] :refer-macros [inspect-pp with-inspection]]
            [parsimony.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn emit-sym [sym]
  (name (parser/->nonterminal sym)))

;;------------------------------------------------------------------------------
;; Pragmas
;;------------------------------------------------------------------------------

(defn emit-pragma [[type value :as pragma]]
  (case type
    :noop "noop-pragma"
    :start (str "start=" (name value))
    (do (console/warn ::emit-pragma :unknown-pragma {:pragma pragma})
        "")))

(defn emit-pragmas [{{:keys [pragmas]} :orig :as ast} options]
  (if (seq pragmas)
    (str "{-# "
         (str/join " " (map emit-pragma (sort pragmas)))
         " #-}")
    ""))

;;------------------------------------------------------------------------------
;; Productions
;;------------------------------------------------------------------------------

(defn sort-productions [options prods]
  (if (:sort-productions options)
    (into [] (sort prods))
    prods))

(defn emit-production-without-attrs [[lhs rhs]]
  (str (emit-sym lhs)
       " = "
       (str/join " " (map emit-sym rhs))))

(defn emit-attrs [attrs]
  (if (seq attrs)
    (str "{"
         (str/join "," (map name attrs))
         "}")
    ""))

(defn emit-production [{{:keys [attributes]} :orig :as ast} prod]
  (let [attr-str (emit-attrs (get attributes prod))]
    (str (emit-production-without-attrs prod)
         (if (seq attr-str)
           (str " " attr-str " ;")
           " ;"))))

(defn emit-productions [{{:keys [productions]} :orig :as ast} options]
  (let [start-prods (parser/productions-with-lhs productions (parser/start-symbol ast))]
    (str/join "\n"
              (map #(if % (emit-production ast %) "")
                   (next ;; the first element is always ""
                         (reduce
                           (fn [acc [lhs _ :as p]]
                             (if (= (first (peek acc)) lhs)
                               (conj acc p)
                               (-> acc
                                   (conj nil)
                                   (conj p))))
                           []
                           (into (vec (sort-productions options start-prods)) ;; start-prods must stay at top
                                 (sort-productions options (parser/remove-productions productions start-prods)))))))))

;;------------------------------------------------------------------------------
;; Priorities
;;------------------------------------------------------------------------------

(defn sort-priority-prods [options prods]
  (if (:sort-productions options)
    (into [] (sort-by str prods))
    prods))

(defn emit-priority [{:keys [high low]}]
  (str (emit-production-without-attrs high)
       " > "
       (emit-production-without-attrs low)
       " ;"))

(defn emit-priority-block [{{:keys [priorities]} :orig :as ast} options]
  (if (seq priorities)
    (str "priorities {\n"
         (str/join "\n"
                   (->> priorities
                        (sort-priority-prods options)
                        (map emit-priority)
                        (map (partial str "  "))))
         "\n}")
    ""))

;;------------------------------------------------------------------------------
;; Associativities
;;------------------------------------------------------------------------------

(defn emit-associativity-block [options [dir prods]]
  (str (name dir) " {\n"
       (str/join "\n"
                 (->> prods
                      (sort-productions options)
                      (map emit-production-without-attrs)
                      (map #(str "  " % " ;"))))
       "\n}"))

(defn emit-associativity-blocks [{{:keys [associativities]} :orig :as ast} options]
  (str/join "\n\n" (map (partial emit-associativity-block options) associativities)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reformat
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn simplify-filters
  "Simplify diasmbiguation filters"
  [ast]
  (letfn [(singleton-assoc? [[_ ps]] (= 1 (count ps)))
          (->orig-assocs [assoc-map dir]
            (into []
                  (comp (map vec)
                        (map (partial vector dir)))
                  (uf/->sets (get assoc-map dir))))
          (->orig-priorities [priority-dag]
            (into []
                  (map (fn [[n m]] {:high n :low m}))
                  (dag/edges priority-dag)))]
    (try
      (let [{:keys [priorities associativities attributes] :as parser} (parser/compile-parser ast)
            lefts  (->orig-assocs associativities :left)
            rights (->orig-assocs associativities :right)
            all-assocs (into lefts rights)
            priorities (-> (:dag priorities)
                           (dag/transitive-reduction)
                           (->orig-priorities))]
        #_(console/debug (pprint-str {:lefts lefts
                                      :rights rights
                                      :priorities priorities}))
        (-> ast
            (assoc-in [:orig :associativities] (into [] (remove singleton-assoc?) all-assocs))
            (update-in [:orig :attributes]
                       (fn [attr-map]
                         (merge-with set/union
                                     attr-map
                                     (into {}
                                           (comp (filter singleton-assoc?)
                                                 (map (fn [[dir [p]]]
                                                        [p #{dir}])))
                                           all-assocs))))
            (assoc-in [:orig :priorities] priorities)))
      (catch js/Error e
        (console/warn :simplify-filters :compile-failure e)
        ast))))

;; TODO: remove duplicate productions
(defn reformat
  "Reformat/normalize a grammar string"
  [ast & {:as options}]
  (let [ast (simplify-filters ast)]
    (str/join "\n\n"
              (filter seq
                      [(emit-pragmas ast options)
                       (emit-productions ast options)
                       (emit-associativity-blocks ast options)
                       (emit-priority-block ast options)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AST Rewriting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-productions [ast prods]
  (let [existing (set (get-in ast [:orig :productions]))]
    (update-in ast
               [:orig :productions]
               into
               (comp (distinct)
                     (remove existing))
               prods)))

(defn remove-productions [ast prods]
  (update-in ast
             [:orig :productions]
             #(into [] (remove (set prods)) %)))

(defn add-attributes [ast prod attrs]
  (update-in ast
             [:orig :attributes prod]
             (fnil into #{})
             attrs))

(defn remove-attributes [ast prods]
  (update-in ast
             [:orig :attributes]
             #(apply dissoc % prods)))

(defn add-block-associativity [ast prods dir]
  (if (seq prods)
    (update-in ast
               [:orig :associativities]
               (fnil conj [])
               [dir (vec prods)])
    ast))

(defn add-priority [ast low-prod high-prod]
  (let [priority {:high high-prod :low low-prod}]
    (if (contains? (set (get-in ast [:orig :priorities])) priority)
      ast
      (update-in ast
                 [:orig :priorities]
                 conj
                 priority))))

(defn remove-priorities [ast prods]
  (let [prods (set prods)]
    (letfn [(mentions-prod? [{:keys [high low]}]
              (or (contains? prods high)
                  (contains? prods low)))]
      (update-in ast
                 [:orig :priorities]
                 #(into [] (remove mentions-prod?) %)))))

(defn remove-block-associativities [ast prods]
  (let [prods (set prods)]
    (letfn [(remove-prod [[dir ps :as a]]
              (let [ps' (into [] (remove prods) ps)]
                (when (seq ps')
                  [[dir ps']])))]
      (assoc-in ast
                [:orig :associativities]
                (into []
                      (mapcat remove-prod)
                      (get-in ast [:orig :associativities]))))))

(defn remove-filter-mentions [ast prods]
  (-> ast
      (remove-attributes prods)
      (remove-block-associativities prods)
      (remove-priorities prods)))

(defn remove-all-mentions [ast prods]
  (-> ast
      (remove-productions prods)
      (remove-filter-mentions prods)))

(defn remove-all-attributes [ast]
  (assoc-in ast [:orig :attributes] {}))

(defn remove-all-priorities [ast]
  (assoc-in ast [:orig :priorities] []))

(defn remove-all-associativities [ast]
  (assoc-in ast [:orig :associativities] []))

(defn remove-all-filters [ast]
  (-> ast
      (remove-all-attributes)
      (remove-all-priorities)
      (remove-all-associativities)))
