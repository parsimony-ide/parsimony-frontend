(ns parsimony.parser
  "CYK parser implementation"
  (:require [instaparse.core :as insta]
            [instaparse.viz :as insta-viz]
            [cljs.pprint :refer [pprint]]
            [clojure.set :as set]
            [clojure.string :as str]
            [goog.string :as gstring]
            [goog.string.format]
            [parsimony.dag :as dag]
            [parsimony.union-find :as uf]
            [parsimony.util :refer [pprint-str matches-schema?] :refer-macros [inspect inspect-pp]]
            [schema.core :as s :include-macros true]
            [parsimony.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def production-schema [(s/one s/Keyword "lhs") (s/one [s/Keyword] "rhs")])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input Specification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following are not permitted:
;; - Alternation. Instead, write one production per alternative.
;; - Repetition. Instead, use recursion.
;; - Grouping. Instead, create a new production for each sub-group.
;; - Epsilons. Options cover the main use case for epsilons.

;; The following are permitted:
;; - Option. This is desugared internally into multiple productions that exhaustively
;;   cover the existence/nonexistence of all options.

(def production-specification
  "
  S                  = <maybe-ws> (pragma-block / line-comment / priority-block / assoc-block / production)

  pragma-block       = <'{-#'> <maybe-ws> pragma-list <maybe-ws> <'#-}'>
  <pragma-list>      = pragma | pragma <maybe-ws> pragma-list
  <pragma>           = noop-pragma | start-pragma
  noop-pragma        = 'noop-pragma'
  start-pragma       = <'start'> <maybe-ws> <'='> <maybe-ws> ident

  line-comment       = <';;'> #'[^\\n]*'

  production         = ident <maybe-ws> <sep> <maybe-ws> body <maybe-ws> <';'>
                     | ident <maybe-ws> <sep> <maybe-ws> body <maybe-ws> attr-block <maybe-ws> <';'>
  body               = form (<ws> form)*
  attr-block         = <'{'> <maybe-ws> attr-list <maybe-ws> <'}'>
  <attr-list>        = attr | attr <maybe-ws> (<','> <maybe-ws> attr )+
  attr               = 'prefer' | 'left' | 'right'
  sep                = '='
  form               = ident | option
  option             = ident <'?'>

  priority-block     = <'priorities'> <maybe-ws> <'{'> <maybe-ws> priority-list <maybe-ws> <'}'>
  <priority-list>    = priority | priority <maybe-ws> priority-list
  priority           = priority-prod <maybe-ws> priority-comp <maybe-ws> priority-prod <maybe-ws> <';'>
  priority-comp      = <'>'> | priority-index <maybe-ws> <'>'>
  <priority-index>   = <'<'> <maybe-ws> priority-index-args <maybe-ws> <'>'>
  <priority-index-args> = num (<maybe-ws> <','> <maybe-ws> num)*
  priority-prod      = ident <maybe-ws> <sep> <maybe-ws> body

  assoc-block        = assoc-dir <maybe-ws> <'{'> <maybe-ws> assoc-list <maybe-ws> <'}'>
  <assoc-dir>        = 'left' | 'right'
  <assoc-list>       = assoc-prod <maybe-ws> <';'> (<maybe-ws> assoc-prod <maybe-ws> <';'>)*
  assoc-prod         = ident <maybe-ws> <sep> <maybe-ws> body

  num                = #'[0-9]+'
  ident              = #'[\\-a-zA-Z][_a-zA-Z0-9\\-]*'
  ws                 = #'\\s+'
  maybe-ws           = #'\\s*'
  ")

(def production-parser
  (insta/parser production-specification))

(declare ->terminal optional? ->required ->cnf lhs rhs terminal? delete-nts-from-production
         associativity-declarations priority-declarations priority-production-declarations rhs-instances
         dummy-node? dummy-parent?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- drop-whitespace-syms
  "Remove whitespace terminals from the given production"
  [p]
  (delete-nts-from-production p #{:%ws :%ws?}))

;;------------------------------------------------------------------------------
;; Compile Pragmas
;;------------------------------------------------------------------------------

(defn compile-pragmas
  "Insert pragmas into :options map"
  [{{:keys [pragmas]} :orig options :options :as parser}]
  (assoc parser :options (into options pragmas)))

;;------------------------------------------------------------------------------
;; Compile Productions
;;------------------------------------------------------------------------------

(defn compile-productions
  [{{:keys [productions]} :orig :as parser}]
  (if-not (seq productions)
    ;; If grammar has no productions, insert dummy production: & = ws
    ;; This allows us to assume that CYK will always have at least 1 production.
    (assoc parser :productions [[:& [:%ws]]])
    (assoc parser :productions productions)))

;;------------------------------------------------------------------------------
;; Compile Attributes
;;------------------------------------------------------------------------------

(defn- compile-attributes
  "Normalize attributes"
  [{{:keys [attributes]} :orig :as parser}]
  (assoc parser :attributes
         (into {}
               (map (fn [[p attrs]] [(drop-whitespace-syms p) attrs]))
               attributes)))

;;------------------------------------------------------------------------------
;; Compile Associativities
;;------------------------------------------------------------------------------

(defn- compose-assoc-maps
  "Compose two maps of form {:left <union-find> :right <union-find>}"
  [{:keys [left right]} m']
  {:left (uf/combine left (:left m'))
   :right (uf/combine right (:right m'))})

(defn- incompatible-associativities
  "Return a set of productions that are defined with opposing associativity
   if such a set exists"
  [{:keys [left right]}]
  (first (filter seq
                 (for [l (uf/->sets left) r (uf/->sets right)]
                   (set/intersection l r)))))

(defn- compile-associativities
  "Normalize associativity equivalence classes"
  [{:keys [attributes orig] :as parser}]
  (let [orig-assoc-maps
        (for [[dir ps] (:associativities orig)]
          {dir (apply uf/add-equivalence-class (uf/new-union-find) (map drop-whitespace-syms ps))})
        attribute-assoc-maps
        (for [[p attrs] attributes
              dir [:left :right]
              :when (get attrs dir)]
          {dir (uf/add (uf/new-union-find) (drop-whitespace-syms p))})
        assoc-map (reduce compose-assoc-maps
                          {:left (uf/new-union-find)
                           :right (uf/new-union-find)}
                          (concat orig-assoc-maps
                                  attribute-assoc-maps))]
    (if-let [p (first (incompatible-associativities assoc-map))]
      (throw (ex-info "Incompatible associativity"
                      {:causes #{:incompatible-associativity}
                       :production p
                       :declarations (into (associativity-declarations parser p :left)
                                           (associativity-declarations parser p :right))}))
      (assoc parser :associativities assoc-map))))

;;------------------------------------------------------------------------------
;; Compile Priorities
;;------------------------------------------------------------------------------

(defn- compile-priorities
  "Convert raw priorities and associativities into a DAG for efficient
   disambiguation queries"
  [{:keys [attributes associativities]
    {:keys [priorities]} :orig
    :as parser}]
  ;; assume: compile-associativities has already been run
  (let [mentioned-prods ;; all productions mentioned in any priority pragma
        (into #{}
              (map drop-whitespace-syms)
              (concat (map :high priorities)
                      (map :low priorities)))
        uf (reduce uf/add
                   (uf/combine (:left associativities) (:right associativities))
                   mentioned-prods)
        f (fn [g {:keys [high low only] :as priority}]
            (let [high (drop-whitespace-syms high)
                  low (drop-whitespace-syms low)
                  high-root (uf/find uf high)
                  low-root (uf/find uf low)
                  g' (dag/add-edge g high-root low-root)]
              (if (or ;; priority cycle found
                      (dag/cyclic? g')
                      ;; declaration mentions two productions declared in same assoc group
                      (= high-root low-root))
                (do
                  (inspect-pp "priority cycle"
                              {:parser parser
                               :dag-nodes (dag/nodes g)
                               :high high
                               :low low
                               :high-root high-root
                               :low-root low-root
                               :uf (uf/->sets uf)})
                  (throw (ex-info "Priority definition contains cycle"
                                  {:causes #{:priority-cycle}
                                   :production low
                                   :declarations (priority-declarations parser low)})))
                (if only
                  (dag/add-edge-attr g' high-root low-root :only only)
                  g'))))
        g (reduce f (dag/new-dag) priorities)]
    (assoc parser :priorities {:uf uf
                               :dag g
                               :closure (dag/transitive-closure g)})))

;;------------------------------------------------------------------------------
;; Compile CNF
;;------------------------------------------------------------------------------

(defn- compile-cnf
  "Add :cnf key and value to parser"
  [{:keys [productions] :as parser}]
  (assoc parser :cnf (->cnf productions)))

;;------------------------------------------------------------------------------
;; Compile Cleanup
;;------------------------------------------------------------------------------

(defn- compile-cleanup
  [{:keys [options] :as parser}]
  (if (:keep-source-map options)
    parser
    (dissoc parser :source-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;------------------------------------------------------------------------------
;; Check Empty Grammar
;;------------------------------------------------------------------------------

(defn- check-empty-grammar
  [{{:keys [productions]} :orig :as parser}]
  (if-not (seq productions)
    (throw (ex-info "Empty grammar"
                    {:causes #{:empty-grammar}}))
    parser))

;;------------------------------------------------------------------------------
;; Check Undefined Symbols
;;------------------------------------------------------------------------------

(defn- check-undefined-symbols
  [{{:keys [productions]} :orig :as parser}]
  (let [defined-symbols (into #{} (map lhs) productions)
        used-symbols (into #{} (comp (mapcat rhs)
                                     ;; remove terminals since they aren't defined in the CFG
                                     (remove terminal?)
                                     (map ->required))
                           productions)]
    (if-let [syms (seq (set/difference used-symbols defined-symbols))]
      (throw (ex-info "Symbol without definition"
                      {:causes #{:undefined-symbol}
                       :symbols (vec syms)
                       :instances (vec (mapcat (partial rhs-instances parser) syms))}))
      parser)))

;;------------------------------------------------------------------------------
;; Check Redundant Productions
;;------------------------------------------------------------------------------

(declare production-declarations)

(defn- check-redundant-productions
  [{{:keys [productions]} :orig :as parser}]
  (let [violating-productions (->> (frequencies productions)
                                   (filter (fn [[_ n]]
                                             (> n 1)))
                                   (map first))]
    (if-let [production (first violating-productions)]
      (let [declarations (set (production-declarations parser production))]
        (throw (ex-info "Redundant production"
                        {:causes #{:redundant-production}
                         :production production
                         :declarations declarations})))
      parser)))

;;------------------------------------------------------------------------------
;; Check Cyclic Productions
;;------------------------------------------------------------------------------

(declare unit-rule?)

(defn- productions->dag [productions]
  (let [unit-productions (filter unit-rule? productions)]
    (reduce (fn [g [lhs [rhs]]]
              (dag/add-edge g lhs rhs))
            (dag/new-dag)
            unit-productions)))

(defn- cycle->productions [nts]
  (->> (conj (vec nts) (first nts)) ;; add begining to end to complete cycle
       (partition 2 1)              ;; pairs
       (map (fn [[l r]] [l [r]]))))

(defn- check-cyclic-productions
  [{{:keys [productions]} :orig :as parser}]
  (let [g (productions->dag productions)
        violating-productions (-> (dag/all-cycles g)
                                  (first)
                                  (cycle->productions))]
    #_(console/debug :violating-productions (pprint-str violating-productions))
    (if (seq violating-productions)
      (let [declarations (into #{}
                               (mapcat (partial production-declarations parser))
                               violating-productions)]
        (throw (ex-info "Grammar contains cyclic productions"
                        {:causes #{:production-cycle}
                         :productions (vec violating-productions)
                         :declarations declarations})))
      parser)))

;;------------------------------------------------------------------------------
;; Check Associativity Productions
;;------------------------------------------------------------------------------

(defn- check-associativity-productions
  [{{:keys [productions associativities]} :orig :as parser}]
  (let [prods (into #{}
                    (map drop-whitespace-syms)
                    productions)
        violating-associativities (for [[dir assoc-prods] associativities
                                        assoc-prod assoc-prods
                                        :when (not (contains? prods (drop-whitespace-syms assoc-prod)))]
                                    [dir assoc-prod])]
    (if (seq violating-associativities)
      (let [declarations (into #{}
                               (mapcat (fn [[dir p]] (associativity-declarations parser p dir)))
                               violating-associativities)]
        (throw (ex-info "Associativity declaration references previously undefined production"
                        {:causes #{:undefined-associativity-productions}
                         :associativities (vec violating-associativities)
                         :declarations declarations})))
      parser)))

;;------------------------------------------------------------------------------
;; Check Priority Productions
;;------------------------------------------------------------------------------

(defn- check-priority-productions
  [{{:keys [productions priorities]} :orig :as parser}]
  (let [prods (into #{}
                    (map drop-whitespace-syms)
                    productions)
        violating-priorities (for [{:keys [high low]} priorities
                                   p [high low]
                                   :when (not (contains? prods (drop-whitespace-syms p)))]
                               p)]
    (if (seq violating-priorities)
      (let [declarations (into #{}
                               (mapcat (partial priority-production-declarations parser))
                               violating-priorities)]
        (throw (ex-info "Priority declaration references previously undefined production"
                        {:causes #{:undefined-priority-productions}
                         :priorities (vec priorities)
                         :declarations declarations})))
      parser)))

;;------------------------------------------------------------------------------
;; Check Pragmas
;;------------------------------------------------------------------------------

(declare lhs-nts start-pragma-declarations)

(defn- check-pragmas
  [{options :options {:keys [productions]} :orig :as parser}]
  (if-let [start (:start options)]
    (if-not (contains? (set (lhs-nts productions)) start)
      (let [declarations (set (start-pragma-declarations parser))]
        (throw (ex-info "Start pragma refers to undefined nonterminal"
                        {:causes #{:undefined-start-symbol}
                         :symbol start
                         :declarations declarations})))
      parser)
    parser))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- pre-checks
  [{:keys [options] :as parser}]
  (if (:skip-checks options)
    parser
    (-> parser
        #_check-empty-grammar
        check-undefined-symbols
        check-redundant-productions
        check-cyclic-productions
        check-associativity-productions
        check-priority-productions)))

(defn- post-checks
  [{:keys [options] :as parser}]
  (if (:skip-checks options)
    parser
    (-> parser
        check-pragmas)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grammar -> AST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn compile-parser
  ([parser]
   (compile-parser parser nil))
  ([parser options]
   (-> parser
       (update :options (fnil merge {}) options)
       (pre-checks)
       (compile-pragmas)
       (compile-productions)
       (compile-attributes)
       (compile-associativities)
       (compile-priorities)
       (compile-cnf)
       (post-checks)
       (compile-cleanup))))

(defn- gen-ast-transform [tokens]
  {:S (fn [_ x] x)
   :pragma-block (fn [{:keys [span]} & pragmas]
                   {:pragmas (into []
                                   (map #(vector (:type %) (:value %)))
                                   pragmas)
                    :source-map (merge-with into
                                            (apply merge-with into (map :source-map pragmas))
                                            {:pragma-block
                                             (into []
                                                   (map #(vector (:type %) {:span span})
                                                        pragmas))})})
   :noop-pragma (fn [{:keys [span]} _]
                  {:type :noop
                   :value true
                   :source-map {:pragma [[:noop {:span span}]]}})
   :start-pragma (fn [{:keys [span]} ident]
                   (let [v (:value ident)]
                     {:type :start
                      :value v
                      :source-map (merge-with into
                                              (:source-map ident)
                                              {:pragma [[:start {:span span}]]})}))
   :line-comment (constantly nil)
   :production (fn [{:keys [span]} ident body attrs]
                 (let [v [(:value ident) (:value body)]]
                   {:production v
                    :attributes (:value attrs)
                    :source-map (merge-with into
                                            (:source-map attrs)
                                            (:source-map ident)
                                            (:source-map body)
                                            {:production [[v {:span span}]]})}))
   :body (fn [{:keys [span]} & forms]
           (letfn [(f [{:keys [value] :as form}]
                     (if (or (contains? tokens value) (contains? tokens (->required value))) ;; second case handles optional token
                       (->terminal value)
                       value))]
             {:value (into [] (map f) forms)
              :source-map (merge-with into
                            (apply merge-with into (map :source-map forms))
                            {:body
                             (into []
                                   (map #(vector (:value %) {:span span}))
                                   forms)})}))
   :attr-block (fn [{:keys [span]} & attrs]
                 {:value (into #{}
                               (map :value)
                               attrs)
                  :source-map (merge-with into
                                          (apply merge-with into (map :source-map attrs))
                                          {:attr-block
                                           (into []
                                                 (map #(vector (:value %) {:span span}))
                                                 attrs)})})
   :attr (fn [{:keys [span]} attr-str]
           (let [v (keyword attr-str)]
           {:value v
            :source-map {:attr [[v {:span span}]]}}))
   :priority-block (fn [{:keys [span]} & priorities]
                     {:priorities (vec (map :value priorities))
                      :source-map (merge-with into
                                              (apply merge-with into (map :source-map priorities))
                                              {:priority-block
                                               (into []
                                                     (map #(vector (:value %) {:span span}))
                                                     priorities)})})
   :priority (fn [{:keys [span]} lprod pcomp rprod]
               {:value (merge {:high (:value lprod)
                               :low (:value rprod)}
                              (when (seq (:value pcomp))
                                {:only (:value pcomp)}))
                :source-map (merge-with into
                                        (:source-map lprod)
                                        (:source-map rprod)
                                        {:priority [[(:value lprod) {:span span}]
                                                    [(:value rprod) {:span span}]]})})

   :priority-comp (fn [{:keys [span]} & nums]
                    {:value (into #{}
                                  (remove js/isNaN)
                                  (map :value nums))
                     ;; XXX: We're not putting anything new into source-map at
                     ;; this level. Add it later if it ends up being necessary.
                     :source-map (apply merge-with into (map :source-map nums))})

   :num (fn [{:keys [span]} num-str]
          (let [v (js/parseInt num-str)]
            {:value v
             :source-map {:num [[v {:span span}]]}}))

   :priority-prod (fn [{:keys [span]} ident body]
                    (let [v [(:value ident) (:value body)]]
                      {:value v
                       :source-map (merge-with into
                                               (:source-map ident)
                                               (:source-map body)
                                               {:priority-prod [[v {:span span}]]})}))


   :assoc-block (fn [{:keys [span]} dir & assocs]
                  {:associativities
                   [(keyword dir)
                    (into [] (map :value) assocs)]
                   :source-map (merge-with into
                                           (apply merge-with into (map :source-map assocs))
                                           {:assoc-block
                                            (into []
                                                  (map #(vector (:value %) {:span span
                                                                            :dir (keyword dir)}))
                                                  assocs)})})
   :assoc-prod (fn [{:keys [span]} ident body]
                 (let [v [(:value ident) (:value body)]]
                   {:value v
                    :source-map (merge-with into
                                            (:source-map ident)
                                            (:source-map body)
                                            {:assoc-prod [[v {:span span}]]})}))
   :form (fn [_ x]
           x)
   :option (fn [{:keys [span]} ident]
             (let [v (keyword (str (name (:value ident)) "?"))]
             {:value v
              :source-map (merge-with into
                                      (:source-map ident)
                                      {:option [[v {:span span}]]})}))
   :ident (fn [{:keys [span]} ident-str]
            (let [v (keyword ident-str)]
              {:value v
               :source-map {:ident [[v {:span span}]]}}))})

(defn- inject-spans [i t]
  (if (sequential? t)
    (apply vector
           (first t)
           {:span
            (into []
                  (map (partial + i))
                  (insta-viz/span t))}
           (map (partial inject-spans i) (next t)))
    t))

(defn definition-parser
  ([s]
   (definition-parser s nil))
  ([s tokens & {:as options}]
   {:pre [(matches-schema? (s/maybe {(s/enum :keep-source-map :skip-checks) s/Bool})
                           options)]}
   (let [options (merge {:keep-source-map false
                         :skip-checks false}
                        options)
         s' (str/triml s)
         idx-offset (- (count s) (count s'))
         s (str/trimr s')
         ast-transform (gen-ast-transform (set tokens))]
     (loop [i 0
            ps []
            attrs {}
            prios []
            assocs []
            prags []
            smap {}]
       (if (> i (dec (count s)))
         (compile-parser
           {:orig {:productions ps
                   :attributes attrs
                   :priorities prios
                   :associativities assocs
                   :pragmas prags}
            :source-map smap
            :options options})
         (let [ast (insta/parse production-parser (subs s i) :partial true)]
           (if (insta/failure? ast)
             (throw (ex-info (str "Instaparse failure:" \newline (pr-str ast))
                             {:causes #{:parse-failure}
                              :index (+ idx-offset i)
                              :string (subs s i)
                              :failure ast}))
             (let [{:keys [production attributes priorities associativities pragmas source-map]}
                   (insta/transform ast-transform (inject-spans (+ idx-offset i) ast))
                   [_ j] (insta-viz/span ast)]
               (recur (+ i j)
                      (if production (conj ps production) ps)
                      (if (seq attributes)
                        (assoc attrs production attributes)
                        attrs)
                      (if (seq priorities)
                        (into prios priorities)
                        prios)
                      (if (seq associativities)
                        (conj assocs associativities)
                        assocs)
                      (if (seq pragmas)
                        (into prags pragmas)
                        prags)
                      (merge-with into smap source-map))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source Map Manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- span-within [[from to] [_ {[f t] :span}]]
  (<= from f t to))

(defn- source-lookup
  ([vs k]
   (into []
         (filter (fn [[n _]]
                   (= n k)))
         vs)))

(defn associativity-declarations
  "Return all associativity declarations for the given production and direction"
  [{:keys [source-map] :as parser} prod -dir]
  (let [inline (vec (for [[_ {:keys [span]}] (source-lookup (:production source-map) prod)
                          v (source-lookup (:attr source-map) -dir)
                          :when (span-within span v)]
                      v))
        block (vec (for [[_ {:keys [span dir]}] (source-lookup (:assoc-block source-map) prod)
                         :when (= dir -dir)
                         v (source-lookup (:assoc-prod source-map) prod)
                         :when (span-within span v)]
                     v))]
    (set (into inline block))))

(defn priority-declarations
  "Return all priority declarations containing the given production"
  [{:keys [source-map] :as parser} prod]
  (set (source-lookup (:priority source-map) prod)))

(defn priority-production-declarations
  "Return all priority production declarations for the given production"
  [{:keys [source-map] :as parser} prod]
  (set (source-lookup (:priority-prod source-map) prod)))

(defn rhs-instances
  "Return all uses of the given nonterminal on a right hand side"
  [{:keys [source-map] :as parser} nt]
  (set
    (for [[_ {:keys [span]}] (source-lookup (:body source-map) nt)
          v (source-lookup (:ident source-map) nt)
          :when (span-within span v)]
      v)))

(defn production-declarations
  "Return all declarations for the given production"
  [{:keys [source-map] :as parser} production]
  (source-lookup (:production source-map) production))

(defn start-pragma-declarations
  "Return all start pragma declarations"
  [{:keys [source-map] :as parser}]
  (source-lookup (:pragma source-map) :start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ASTS utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn lhs [p]
  (first p))

(defn rhs [p]
  (second p))

(defn optional? [nt]
  (= "?" (last (name nt))))

(defn terminal? [kw]
  (= "%" (first (name kw))))

(defn pseudo-terminal? [kw]
  (= "*" (first (name kw))))

(defn binarized? [kw]
  (some? (re-find #":\d+$" (name kw))))

(defn multinode-nonterminal?
  "Return true iff the given symbol is the name of a multinode nonterminal"
  [kw]
  (= "-" (first (name kw))))

(defn remove-kw-suffix [kw suffix]
  (let [kw-name (name kw)]
    (if (.endsWith kw-name suffix)
      (keyword (subs kw-name 0 (- (count kw-name) (count suffix))))
      kw)))

(defn remove-kw-prefix [kw prefix]
  (let [kw-name (name kw)]
    (if (.startsWith kw-name prefix)
      (keyword (subs kw-name (count prefix)))
      kw)))

(defn ->required [nt]
  (remove-kw-suffix nt "?"))

(defn ->terminal [t]
  (if-not (.startsWith (name t) "%")
    (keyword (str "%" (name t)))))

(defn ->nonterminal [t]
  (remove-kw-prefix t "%"))

(defn ->pseudo-terminal [kw]
  (if (terminal? kw)
    (keyword (str "*" (name (remove-kw-prefix kw "%"))))
    kw))

(defn ->binarize [sym n]
  (if-not (binarized? sym)
    (keyword (str (name sym) ":" n))))

;; primes are safe to use since the above grammar does not allow quotes in nonterminal names

(defn prime? [nt]
  (= "'" (last (name nt))))

(defn ->prime [nt]
  (keyword (str (name nt) "'")))

(defn ->unprime [nt]
  (remove-kw-suffix nt "'"))

(defn ->unprime-production [p]
  [(->unprime (lhs p))
   (into [] (map ->unprime) (rhs p))])

(defn productions-with-lhs [ps nt]
  (into []
        (filter #(= (lhs %) nt))
        ps))

(defn productions-with-rhs [ps nt]
  (into []
        (filter #(contains? (set (rhs %)) nt))
        ps))

(defn lhs-nts [ps]
  (vec (into #{} (map lhs) ps)))

(defn rhs-nts [ps]
  (vec (into #{} (mapcat rhs) ps)))

(defn all-syms [ps]
  (-> (set/union (set (lhs-nts ps)) (set (rhs-nts ps)))
      (sort)
      (vec)))

(defn max-productions-per-lhs [ps]
  (->> ps
       (map lhs)
       (frequencies)
       (map second)
       (apply max)))

(defn pred-masks [p pred]
  (reduce
   (fn [acc nt]
     (if (pred nt)
       (into []
             (mapcat #(vector (conj % true)
                              (conj % false)))
             acc)
       (into []
             (mapcat #(vector (conj % true)))
             acc)))
   (vector [])
   (rhs p)))

(defn mask->production [p f m]
  [(lhs p)
   (into []
         (comp (filter first)
               (map second)
               (map f))
         (map vector m (rhs p)))])

(defn expand-optional-production [p]
  (into []
        (map (partial mask->production p ->required))
        (pred-masks p optional?)))

(defn epsilon-rule? [p]
  (not (seq (rhs p))))

(defn epsilon-rules [ps]
  (into [] (filter epsilon-rule?) ps))

(defn non-epsilon-rules [ps]
  (into []
        (remove epsilon-rule?)
        ps))

(defn unit-self-cyclic-rule? [p]
  (and (= (lhs p) (first (rhs p)))
       (= 1 (count (rhs p)))))

(defn unit-rule? [p]
  (= 1 (count (rhs p))))

(defn unit-rules [ps]
  (into [] (filter unit-rule?) ps))

(defn has-terminal? [p]
  (some terminal? (rhs p)))

(defn delete-nts-from-production [p nts]
  (update p 1 #(into [] (remove (partial contains? (set nts))) %)))

(defn remove-production [ps p]
  (into [] (remove (partial = p)) ps))

(defn remove-productions [ps to-remove]
  (into [] (remove (partial contains? (set to-remove))) ps))

(defn remove-productions-by-lhs [ps nt]
  (remove-productions ps (productions-with-lhs ps nt)))

(defn remove-productions-by-rhs [ps nt]
  (remove-productions ps (productions-with-rhs ps nt)))

(defn sort-by-nt [ps]
  (into [] (sort-by first ps)))

(defn str-pad [s n]
  (let [diff (- n (count s))]
    (if (pos? diff)
      (apply str s (repeat diff " "))
      s)))

(defn emit [ps]
  (let [lhs-max (apply max (into []
                                 (map (comp count name))
                                 (lhs-nts ps)))]
    (str/join \newline
              (map
               (fn [p]
                 (str
                  (str-pad (name (lhs p)) lhs-max)
                  " = "
                  (str/join " " (map (comp name ->nonterminal) (rhs p)))))
               ps))))

(defn emit-one [p]
  (emit [p]))

(defn start-symbol [{:keys [options] :as parser}]
  (or (:start options)
      (when-let [first-production (first (get-in parser [:orig :productions]))]
        (lhs first-production))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grammar Compression
;; - Grammars can have redundant productions. This is especially true after
;;   CNF transform, where there are many intermediate rules due to
;;   binarization.
;; - The purpose of this code is to remove those redundancies.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- equivalent-binarized
  "Return vector of vectors. Each inner vector contains a non-singleton set of nonterminals that are LHSes
  for binarized productions with the same RHS."
  [ps]
  (letfn [(f [acc [lhs rhs]]
            (if (binarized? lhs)
              (if (some? (get acc rhs))
                (update acc rhs conj lhs)
                (assoc acc rhs (vector lhs)))
              acc))]
    (into []
          (comp
           (filter #(> (count (second %)) 1))
           (map second))
          (reduce f {} ps))))

(defn- equiv->smap
  "Convert vector of vectors representation to a substitution map"
  [ss]
  (letfn [(f [acc [nt & nts]]
            (into acc (map #(vector % nt)) nts))]
    (reduce f {} ss)))

(defn- equiv->deletable
  "Convert vector of vectors representation to set of deletable nonterminals"
  [ss]
  (into #{}
        (comp
         (map next)
         (keep identity)
         (mapcat identity))
        ss))

(defn- apply-subst-rhs
  "Apply substitutions to a production RHS"
  [rhs smap]
  (into []
        (map #(get smap % %))
        rhs))

(defn- apply-subst-p
  "Apply substitutions to a production"
  [p smap]
  (update p 1 apply-subst-rhs smap))

(defn- apply-subst
  "Apply substitutions to a grammar"
  [ps smap]
  (reduce (fn [acc p]
            (conj acc (apply-subst-p p smap)))
          []
          ps))

(defn- apply-deletions
  "Apply deletions to a grammar"
  [ps deletion-set]
  (into []
        (remove #(contains? deletion-set (lhs %)))
        ps))

(defn compress
  "Compress grammar by removing redundant productions and performing relevant substitutions + deletions"
  [ps]
  (loop [ps ps]
    (let [ebin (equivalent-binarized ps)
          deletion-set (equiv->deletable ebin)
          smap (equiv->smap ebin)
          ps' (-> ps
                  (apply-deletions deletion-set)
                  (apply-subst smap))]
      (if (not= ps ps')
        (recur ps')
        ps))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CNF Transforms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn expand-optionals [ps]
  (into []
        (mapcat expand-optional-production)
        ps))

;; A = ... B ...
;; ---
;; A = ...    ...
;; A = ... B' ...
(defn elim-rules
  "Auxilary method for elim-epsilon-rules. For every mention of nt in p,
   construct all variants such that nt is either removed, or replaced with its prime"
  [p nt]
  (into []
        (map (partial mask->production p #(if (= nt %)
                                            (->prime %)
                                            %)))
        (pred-masks p (partial = nt))))

(defn elim-epsilon-rules-1 [ps]
  ;; worklist
  ;; let A = ϵ be a rule in the grammar such that A occurs on some RHS
  ;; for each rule B = ... A ...
  ;; delete B
  ;; add B = ... A' ...
  ;; add B = ...    ...
  (loop [worklist (set (epsilon-rules ps)) ps ps]
    (let [mentioned-nts (set (rhs-nts ps))]
      (if-let [a (first
                  (into #{}
                        (filter #(contains? mentioned-nts (lhs %)))
                        worklist))]
        (let [to-remove (productions-with-rhs ps (lhs a))
              to-add (mapcat #(elim-rules % (lhs a)) to-remove)
              ps (-> ps
                     (remove-productions to-remove)
                     (into to-add))]
          #_(println "epsilon rules =" (epsilon-rules ps))
          #_(println "ps =" ps)
          #_(println "to-remove =" to-remove)
          #_(println "to-add =" to-add)
          (recur (set (epsilon-rules ps))
                 ps))
        ps))))

(defn elim-epsilon-rules-2 [ps]
  ;; for each rule A = β such that A' exists
  ;;   add rule A' = β iff β ≠ ϵ
  (let [nts' (into [] (filter prime?) (rhs-nts ps))
        to-add (for [nt' nts'
                     p (productions-with-lhs ps (->unprime nt'))
                     :when (seq (rhs p))]
                 [nt' (rhs p)])]
    (into ps to-add)))

(defn elim-epsilon-rules-3 [ps]
  ;; let X be a nonterminal such that X = ϵ is its only production
  ;; then remove all rules Y = ... X' ...
  (let [ents (lhs-nts (epsilon-rules ps))
        nnts (lhs-nts (non-epsilon-rules ps))
        dead-nts (into []
                       (map ->prime)
                       (set/difference (set ents) (set nnts)))]
    (reduce remove-productions-by-rhs ps dead-nts)))

(defn elim-epsilon-rules-4 [ps]
  ;; delete all rules A = β when A' exists
  (let [dead-nts (into [] (comp (filter prime?)
                                (map ->unprime)) (rhs-nts ps))]
    (reduce remove-productions-by-lhs ps dead-nts)))

(defn elim-epsilon-rules-5 [ps]
  ;; unprime all
  (into [] (map ->unprime-production) ps))

(defn elim-epsilon-rules [ps]
  (-> ps
      elim-epsilon-rules-1
      elim-epsilon-rules-2
      elim-epsilon-rules-3
      elim-epsilon-rules-4
      elim-epsilon-rules-5
      non-epsilon-rules))

;; don't eliminate the unreachable rules, keep them for reconstructing the parse tree
(defn elim-unit-rules [ps]
  ;; A = B
  ;; B = 1 | 2 | 3
  ;; ---
  ;; A = 1 | 2 | 3
  ;; B = 1 | 2 | 3
  (let [all-rules (atom (set ps))
        new-rules (atom #{})
        ded-rules (atom #{})
        changed (atom true)]
    (while @changed
      (reset! changed false)
      (doseq [[a [b] :as a-rule] (unit-rules @all-rules)
              :when (not (has-terminal? a-rule))]
        #_(println (emit-one a-rule))
        (swap! ded-rules #(conj % a-rule))
        (doseq [[_ rhs :as b-rule] (productions-with-lhs ps b)]
          #_(println " " (emit-one b-rule))
          (let [new-rule [a rhs]]
            (when-not (or (@new-rules new-rule)
                          (@ded-rules new-rule))
              #_(println "   new rule:" (emit-one new-rule))
              (swap! new-rules #(conj % new-rule))
              (reset! changed true)))))
      (swap! all-rules #(set/difference (set/union % @new-rules) @ded-rules)))
    #_(println "new-rules =" @new-rules)
    #_(println "ded-rules =" @ded-rules)
    (into [] @all-rules)))

(defn add-terminal-rules
  [ps]
  ;; A = B %t C
  ;; ---
  ;; A = B T C
  ;; T = %t
  (letfn [(make-terminal-rule [t]
            [(->pseudo-terminal t) [t]])
          (make-replacement-rule [p]
            [(lhs p)
             (into [] (map ->pseudo-terminal) (rhs p))])]
    (let [new-rules (atom #{})
          ded-rules (atom #{})]
      (doseq [t-rule (filter #(and (has-terminal? %)
                                   (not (unit-rule? %))) ps)]
        #_(println t-rule)
        (let [ts (filter terminal? (rhs t-rule))]
          (swap! new-rules into (map make-terminal-rule) ts)
          (swap! new-rules conj (make-replacement-rule t-rule))
          (swap! ded-rules #(conj % t-rule))))
      (into (vec @new-rules)
            (set/difference (set ps) @ded-rules)))))

(defn binarize-rules [ps]
  (let [fresh (atom {}) ; map from nonterminal sym -> fresh nat
        big-rule?
        (fn [p]
          (> (count (rhs p)) 2))
        fresh-nonterminal
        (fn [sym]
          (if-let [suffix (@fresh sym)]
            (let [new-sym (->binarize sym suffix)]
              (swap! fresh #(update-in % [sym] inc))
              new-sym)
            (let [new-sym (->binarize sym 1)]
              (swap! fresh #(assoc % sym 2))
              new-sym)))
        break-rule
        (fn [[l r]]
          (let [fresh-nt (fresh-nonterminal l)]
            #{[fresh-nt (into [] (take 2) r)]
              [l (into [fresh-nt] (drop 2) r)]}))
        all-rules (atom (set ps))
        changed (atom true)]
    (while @changed
      (reset! changed false)
      (let [new-rules (atom #{})
            ded-rules (atom #{})]
        (doseq [rule (filter big-rule? @all-rules)]
          (let [rules (break-rule rule)]
            (when (seq (set/difference rules @all-rules))
              (swap! new-rules #(set/union % rules))
              (swap! ded-rules #(conj % rule))
              (reset! changed true))))
        (swap! all-rules #(set/difference (set/union % @new-rules) @ded-rules))))
    (vec @all-rules)))

(defn ->cnf [ps]
  (-> ps
      (expand-optionals)
      (elim-epsilon-rules)
      (elim-unit-rules)
      (add-terminal-rules)
      (binarize-rules)
      (compress)
      (sort-by-nt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn lookup-table
  "Return set of nonterminals that are known to match at index i with length l"
  [table i l]
  (get-in table [l i]))

(defn update-table
  "Add v to set of nonterminals known to match at index i with length l. If v is a collection,
   then add all nonterminals in v. If v is a keyword, then add it."
  [table i l v]
  (if (or (sequential? v) (set? v))
    (update-in table [l i] into v)
    (update-in table [l i] conj v)))

(defn assoc-table
  "Replace the elements at i l with v. If v is not a collection, then wrap it in a singleton set"
  [table i l v]
  (assoc-in table [l i] v))

(defn token-matches
  "Return set of nonterminals with a production such that its RHS is token"
  [ps token]
  (let [one-term-token [(->terminal token)]]
    (into #{}
          (comp
           (filter #(= one-term-token (rhs %)))
           (map lhs))
          ps)))

(defn cyk-1
  "Populate CYK table with all nonterminals for rules of form A = %t"
  [table token-vec ps]
  (let [n (count token-vec)]
    (loop [i 0 table table]
      (if (< i n)
        (let [token (nth token-vec i)
              table (update-table table i 1 (token-matches ps token))]
          (recur (inc i) table))
        table))))

(defn matches
  "Return all matching LHSes for the given table position"
  [table ps i l]
  (let [rhses
        (into #{}
         (for [mid-l (range 1 l)
               left (lookup-table table i mid-l)
               right (lookup-table table (+ i mid-l) (- l mid-l))]
           [left right]))
        lhses (into #{}
                    (comp
                     (filter #(rhses (rhs %)))
                     (map lhs))
                    ps)]
    lhses))

(defn cyk-2
  "Populate CYK table with all nonterminals for rules of form A = B C"
  [table n ps]
  (reduce
   (fn [table [l i]]
     (update-table table i l (matches table ps i l)))
   table
   (for [l (range 2 (inc n))
         i (range 0 n)]
     [l i])))

;; this is a square matrix, but could make a ragged one to save space
(defn new-table
  ([n]
   (new-table n #{}))
  ([n v]
   (vec (repeat (inc n) (vec (repeat n v))))))

(defn table-size [table]
  (dec (count table)))

(defn cyk
  "Given a vector of tokens token-vec, and a vector of production ps, generate the corresponding CYK table"
  [token-vec parser]
  (if-let [ps (seq (:cnf parser))]
    (let [n (count token-vec)]
      (-> (new-table n)
          (cyk-1 token-vec ps)
          (cyk-2 n ps)))
    (throw (ex-info "Cannot run CYK with an empty grammar" {:causes #{:empty-grammar}}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CYK Table Pretty Printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pprint-cyk
  "Given a CYK table, the original string, and the raw token vector, produce a pretty printed string representation."
  [table s raw-tokens]
  (let [tokens (into []
                     (map :label)
                     raw-tokens)]
    (println "string =" s)
    (println "tokens =" tokens)
    (println "table  =" (count table))
    (doseq [i (range 0 (count tokens))
            l (range 1 (- (inc (count tokens)) i))]
      (let [first-tok (nth raw-tokens i)
            last-tok (nth raw-tokens (+ i (dec l)))
            table-entry (lookup-table table i l)]
        (when (seq table-entry)
          (println (gstring/format (str "%02d %02d %80s %s")
                                   i l (str table-entry) (subs s (:start first-tok) (:end last-tok))))))))
  )

(defn pprint-coloring
  [table]
  (pprint (map-indexed
            (fn [i row]
              [i (map-indexed vector row)])
            table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coloring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- ignored? [sym]
  (or (binarized? sym)
      (pseudo-terminal? sym)
      (multinode-nonterminal? sym)))

(defn coloring-1
  "Populate coloring table with all singletons"
  [cyk-table [coloring-table scoring-table]]
  (let [n (table-size cyk-table)]
    (reduce
     (fn [[coloring-table scoring-table :as tables] i]
       (if-let [syms (seq (remove ignored? (lookup-table cyk-table i 1)))]
         [(assoc-table coloring-table i 1 (set (map #(vector % i 1) syms)))
          (assoc-table scoring-table  i 1 [1 1 -1])]
         tables))
     [coloring-table scoring-table]
     (range 0 n))))

(defn- partition-coverage
  "Return the coverage = # of characters that belong to some color"
  [colors]
  (count (reduce
          (fn [acc [_ i l]]
            (set/union acc (set (range i (+ i l)))))
          #{}
          colors)))

(defn- partition-cost
  "Cost function for determining partition quality.  Prefer:
   1. Higher coverage
   2. Largest color
   3. Mean color size"
  [colors]
  ;; Costs are made negative to reverse the sense of sort
  [#_(- (partition-coverage colors))
   (- (apply max (map #(nth % 2) colors)))
   (- (/ (reduce + (map #(nth % 2) colors))
         (count colors)))])

(defn- color-fn
  "Auxilary method for coloring-2"
  [cyk-table [coloring-table scoring-table] [l i]]
  (if-let [syms (seq (remove ignored? (lookup-table cyk-table i l)))]
    (do
      #_(console/debug "full-match"
                     (pprint-str
                       {:i i
                        :l l
                        :syms syms}))
      [(assoc-table coloring-table i l (set (map #(vector % i l) syms)))
       ;; negate num to invert scoring sense (fewer colors is better score)
       (assoc-table scoring-table  i l [l l -1])])
    (let [[max-score-coloring score-so-far]
          (loop [max-score-coloring #{} score-so-far [0 0 js/Number.NEGATIVE_INFINITY] mid-ls (range 1 l)]
            (if-let [mid-l (first mid-ls)]
              (let [lcolors (lookup-table coloring-table i mid-l)
                    rcolors (lookup-table coloring-table (+ i mid-l) (- l mid-l))
                    [lcoverage llargest lnum :as lscore] (lookup-table scoring-table i mid-l)
                    [rcoverage rlargest rnum :as rscore] (lookup-table scoring-table (+ i mid-l) (- l mid-l))
                    combined-score
                    (cond
                      ;; both scores exist
                      (and (some? lscore) (some? rscore))
                      [(+ lcoverage rcoverage)
                       (max llargest rlargest)
                       (+ lnum rnum)]
                      ;; only lscore exists
                      (some? lscore)
                      lscore
                      ;; only rscore exists
                      (some? rscore)
                      rscore
                      ;; no score
                      :else [0 0 0])]
                (cond
                  ;; better score found
                  (pos? (compare combined-score score-so-far))
                  (do
                    #_(console/debug "better-score"
                                   (pprint-str
                                     {:i i
                                      :l l
                                      :mid-l mid-l
                                      :combined-score combined-score
                                      :score-so-far score-so-far
                                      :lcolors lcolors
                                      :rcolors rcolors}))
                    (recur
                     (into #{}
                           (remove (comp ignored? first))
                           (set/union lcolors rcolors))
                     combined-score
                     (next mid-ls)))
                  ;; equal score found
                  (= combined-score score-so-far)
                  (do
                    #_(console/debug "same-score"
                                   (pprint-str
                                     {:i i
                                      :l l
                                      :mid-l mid-l
                                      :score combined-score
                                      :lcolors lcolors
                                      :rcolors rcolors}))
                    (recur (into max-score-coloring
                                 (remove (comp ignored? first))
                                 (set/union lcolors rcolors))
                           score-so-far
                           (next mid-ls)))
                  ;; otherwise
                  :else
                  (do
                    #_(console/debug "worse-score"
                                   (pprint-str
                                     {:i i
                                      :l l
                                      :mid-l mid-l
                                      :combined-score combined-score
                                      :score-so-far score-so-far
                                      :lcolors lcolors
                                      :rcolors rcolors}))
                    (recur max-score-coloring score-so-far (next mid-ls)))))
              [max-score-coloring score-so-far]))]
      [(update-table coloring-table i l max-score-coloring)
       (assoc-table scoring-table   i l score-so-far)])))

(defn coloring-2
  "Populate coloring table with non-singletons"
  [cyk-table [coloring-table scoring-table]]
  #_(.profile js/console "coloring-2")
  (let [n (table-size cyk-table)
        res (reduce (partial color-fn cyk-table)
                    [coloring-table scoring-table]
                    (for [l (range 2 (inc n))
                          i (range 0 (inc (- n l)))]
                      [l i]))]
    #_(.profileEnd js/console "coloring-2")
    res))

(defn coloring
  "Given a CYK table, construct the corresponding coloring table"
  [cyk-table]
  #_(.profile js/console "coloring")
  (let [n (table-size cyk-table)
        [coloring-table scoring-table]
        (->> [(new-table n) (new-table n nil)]
             (coloring-1 cyk-table)
             (coloring-2 cyk-table))]
    #_(console/debug ::coloring {:scoring-table scoring-table})
    #_(.profileEnd js/console "coloring")
    coloring-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Succinct Coloring
;; - Sometimes, there are redundant elements in a coloring: they exist
;;   as nodes in the subforest of another element of the coloring. This code
;;   removes those redundant elements.
;; - We make this code separate from `coloring` because this process is
;;   expensive, so we only want to do it as necessary, rather than up front
;;   when computing the coloring table.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare reconstruct all-nodes)

(defn- partition-coloring-by-extent
  [coloring]
  (let [f (fn [m [_ i l :as x]]
            (update m [i l] (fnil conj #{}) x))]
    (vals (reduce f {} coloring))))

(defn -succinct-coloring
  "Generic helper function for implementing succinct-coloring in both this namespace and
   in parsimony.asm.parser. reconstruct-fn is a function of one argument, an extent [nt i l],
   and returns the parse forest at that extent."
  [reconstruct-fn coloring]
  (letfn [(succinct-partition [partition]
            (let [nodes
                  (into {}
                        (map #(vector % (all-nodes (reconstruct-fn %))))
                        partition)
                  g (-> (dag/new-dag)
                        (dag/add-nodes* partition)
                        (dag/add-edges*
                          (for [x partition
                                y partition
                                :when (and (not= x y)
                                           (set/subset? (get nodes y) (get nodes x)))]
                            ;; XXX: Technically, we only need to check to see if y belongs to
                            ;; the subforest of x to determine whether to add DAG edge [x y].
                            ;; We're doing a full node subset check since it's simpler, but
                            ;; revisit this if performance becomes a problem.
                            [x y])))]
              (dag/roots g)))]
    (into #{}
      (comp
        (map succinct-partition)
        (mapcat identity))
      (partition-coloring-by-extent coloring))))

(defn succinct-coloring
  "Remove redundant elements from the given coloring"
  [cyk-table token-vec parser coloring]
  (-succinct-coloring #(apply reconstruct cyk-table token-vec parser %)
                      coloring))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse Tree Reconstruction (undoing the effect of CNF)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn candidate-productions
  "Return collection of [production i l] tuples"
  [ps nt i l]
  (vec (for [p (productions-with-lhs ps nt)]
         [p i l])))

(defn applicable?
  [cyk-table token-vec sym i l]
  ;; either sym is a terminal found in the token stream
  ;; or it is a nonterminal in the corresponding cyk-table element
  (let [res
        (or (and (= l 1) (= (->terminal (nth token-vec i)) sym))
            (contains? (lookup-table cyk-table i l) sym))]
    res))

(defn derive-partitions
  [cyk-table token-vec nts i l]
  (cond
    (and
     (= 1 (count nts))
     (applicable? cyk-table token-vec (first nts) i l))
    [(list [(first nts) i l])]

    :else
    (when-let [nt (first nts)]
      (into []
            (mapcat identity)
            (for [l' (range 1 (inc l))]
              (if (applicable? cyk-table token-vec nt i l')
                (let [partitions (derive-partitions cyk-table token-vec (next nts) (+ i l') (- l l'))]
                  (into []
                        (map #(conj % [nt i l']))
                        partitions))
                nil))))))

(defn reconstruct
  "Given a CYK table and the parser that gave rise to the table, construct the corresponding parse forest
   for the string of length l starting at index i with the given root nonterminal"
  [cyk-table token-vec parser nt i l]
  (let [ps (expand-optionals (:productions parser))] ;; don't want to deal with ?s inside of the grammar
    (loop [worklist (candidate-productions ps nt i l)
           seen #{}
           forest #{}]
      (if-let [[p i l :as candidate] (first worklist)]
        (let [partitions (derive-partitions cyk-table token-vec (rhs p) i l)
              seen (conj seen candidate)
              candidates (into #{}
                               (comp (mapcat identity) ;; remove 1 nesting level
                                     (mapcat #(apply candidate-productions ps %))
                                     (remove seen))
                               partitions)
              forest (into forest (map #(vector [(lhs p) i l] (vec %)) partitions))]
          (recur (into (next worklist) candidates)
                 seen
                 forest))
        forest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse Forest Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn child-map
  "Return map from children to their parents"
  [forest]
  (let [f (fn [acc [p cs]]
            (reduce #(update %1 %2 (fnil conj #{}) p) acc cs))]
    (reduce f {} forest)))

(defn inner-nodes [forest]
  (into #{} (map first) forest))

(defn target-nodes [forest]
  (into #{} (mapcat second) forest))

(defn leaf-nodes [forest]
  (set/difference (target-nodes forest) (inner-nodes forest)))

(declare forest->dag forest-with-dummies->dag)

(defn root-nodes [forest]
  (-> forest
      (forest->dag)
      (dag/roots)))

(defn all-nodes [forest]
  (set/union (inner-nodes forest)
             (target-nodes forest)))

(defn descendant-nodes [forest n]
  (-> forest
      (forest-with-dummies->dag forest)
      (dag/successor-subgraph [n])
      (dag/nodes)
      (disj n)))

(defn all-edges [forest]
  (into #{}
        (for [[lhs rhs] forest
              r rhs]
          [lhs r])))

(defn ambiguous-nodes [forest]
  (let [node-freqs (frequencies (into [] (map first) forest))]
    #_(console/debug "node-freqs =" (pprint-str node-freqs))
    (into #{}
          (comp
           (filter #(> (second %) 1))
           (map first))
          node-freqs)))

(defn dummy-node? [n]
  (= 4 (count n)))

(defn add-dummy-nodes
  "Add dummy nodes that represent difference choices at points of ambiguity"
  [forest]
  (let [ambiguous? (ambiguous-nodes forest)]
    #_(console/debug "ambiguous? =" (pprint-str ambiguous?))
    (first
     (reduce
      (fn [[acc per-parent-indices] [lhs rhs :as rule]]
        (if (ambiguous? lhs)
          (let [i (get per-parent-indices lhs 0)]
            [(into acc
                   [[(conj lhs i) rhs]
                    [lhs [(conj lhs i)]]])
             (assoc per-parent-indices lhs (inc i))])
          [(conj acc rule)
           per-parent-indices]))
      [#{} {}]
      (into [] (sort forest)))) ;; sort forest to enforce deterministic numbering of dummy nodes
    ))

(defn dummy-nodes
  "Return set of all dummy nodes in forest"
  [forest]
  (into #{}
        (filter dummy-node?)
        (map first forest)))

(defn dummy-parent?
  "A dummy parent is the representation of an ambiguous node in a forest DAG."
  [g n]
  (let [children (dag/successors g n)]
    (some dummy-node? (dag/successors g n))))

(defn forest-with-dummies->dag
  "Convert a parse forest as produced by reconstruct, and with dummies inserted, into an explicit parsimony.dag
  representation"
  [forest]
  (let [g (dag/new-dag)
        edges (for [production forest
                    :let [[lhs rhs] production]
                    r rhs]
                [lhs r])]
    (dag/add-edges* (dag/new-dag) edges)))

(defn forest->dag
  "Convert a parse forest as produced by reconstruct into an explicit parsimony.dag representation"
  [forest]
  (forest-with-dummies->dag (add-dummy-nodes forest)))

(defn dag->forest
  "Convert a DAG back to a forest with no dummy nodes"
  [g]
  (set (for [src (dag/nodes g)
             :let [dests (dag/successors g src)
                   src' (if (dummy-node? src)
                          (pop src)
                          src)]
             :when (and (seq dests)
                        (not (some dummy-node? dests)))]
         [src' (vec (sort-by second dests))])))

(defn underlying-production [[lhs rhs]]
  "Given a forest element, return its underlying production"
  [(first lhs)
   (into []
         (map first)
         rhs)])

(defn underlying-productions [forest]
  "Given a forest, return all used underlying productions"
  (into []
        (comp (map underlying-production)
              (distinct))
        forest))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disambiguation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn node-production
  "Return the production that gave rise to the given DAG node. Does not work on dummy parents."
  [g [nt & _ :as node]]
  (drop-whitespace-syms
    [nt (into []
              (map first)
              (sort-by second (dag/successors g node)))]))

(defn node-productions
  "Return the set of productions that gave rise to the given DAG node. The return
   value is a set, rather than just one, since a dummy parent node can have more than
   one production due to ambiguity"
  [g node]
  (if (dummy-parent? g node)
    (into #{}
          (map (partial node-production g))
          (dag/successors g node))
    #{(node-production g node)}))

(defn- remove-redundant-dummies
  "When an ambiguous node only has a single dummy child node, collapse the two into one"
  [g]
  (let [is-redundant?
        (fn [n]
          (let [children (dag/successors g n)]
            (and (= 1 (count children))
                 (dummy-node? (first children)))))
        assimilate
        (fn [g n]
          (let [child (first (dag/successors g n))
                grandchildren (dag/successors g child)]
            (-> g
                (dag/remove-node child)
                (dag/add-edges* (map (partial vector n) grandchildren)))))
        redundant-nodes (into #{}
                              (filter is-redundant?)
                              (dag/nodes g))]
    (reduce assimilate g redundant-nodes)))


(defn- remove-orphan-nodes
  "Remove all orphan nodes (except the supplied root)"
  [g roots]
  (loop [g g]
    (let [orphans (into #{}
                        (comp (remove roots)
                              (filter #(zero? (count (dag/predecessors g %)))))
                        (dag/nodes g))]
      (if (seq orphans)
        (do
          #_(inspect-pp "removing orphans" orphans)
          (recur (dag/remove-nodes* g orphans)))
        g))))

(defn- cull-illegal-children
  [g parent-node illegal-children]
  (-> g
      (dag/remove-edges* (map (partial vector parent-node) illegal-children))
      (remove-orphan-nodes (dag/roots g))
      (remove-redundant-dummies)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prefer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Vertical ambiguity
;; S = A {prefer}
;; S = B
;; ---
;; discard S = B subforest
(defn- resolve-prefer
  [g node {:keys [attributes] :as parser}]
  (if (seq attributes)
    (let [children (dag/successors g node)
          preferred (set (for [child children
                               :let [production (node-production g child)]
                               :when (:prefer (get attributes production))]
                           child))
          illegal (remove preferred children)]
      (if (seq preferred)
        ;; remove the successor-subgraph of non-preferred nodes
        (cull-illegal-children g node illegal)
        g))
    g))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Priority
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- violates-priority?
  "Return true iff the given dummy node is the root of a subtree that violates
   priority rules"
  [g dummy-node {{dag :dag closure :closure uf :uf} :priorities :as parser}]
  (let [[e] dummy-node
        low-production (node-production g dummy-node)
        cs (dag/successors g dummy-node)
        is-violation?
        (fn [i [e' :as c]]
          (when (= e e') ;; should have same nonterminal as the dummy-node
            (let [high-production (node-production g c)]
              (if-let [only (dag/edge-attr dag low-production high-production :only)]
                (let [verdict (and (contains? only i)
                                   (dag/has-edge? closure (uf/find uf low-production) (uf/find uf high-production)))]
                  #_(inspect-pp "priority argument-specific"
                              {:only only
                               :i i
                               :dummy-node dummy-node
                               :c c
                               :low-production low-production
                               :high-production high-production
                               :verdict verdict})
                  verdict)
                (let [verdict (dag/has-edge? closure (uf/find uf low-production) (uf/find uf high-production))]
                  #_(inspect-pp "priority not-argument-specific"
                              {:i i
                               :dummy-node dummy-node
                               :c c
                               :low-production low-production
                               :high-production high-production
                               :verdict verdict})
                  verdict)))))
        violations (keep-indexed (fn [i c]
                                   (when (is-violation? i c)
                                     c))
                                 cs)]
    (some? (seq violations))))

(defn- resolve-priority
  [g node parser]
  (if-not (dag/empty? (get-in parser [:priorities :dag]))
    (if-let [illegal (seq (filter #(violates-priority? g % parser)
                                 (dag/successors g node)))]
      (cull-illegal-children g node illegal)
      g)
    g))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Associativity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- violates-associativity?
  "Return true iff the given dummy node is the root of a subtree that violates
   associativity rules in the given direction"
  [g dummy-node dir parser]
  (when-let [uf (get-in parser [:associativities dir])]
    (when-not (uf/empty? uf)
      (let [[e] dummy-node
            top-production (node-production g dummy-node)
            cs (filter (fn [[e']] (= e e')) (dag/successors g dummy-node))
            is-violation?
            (fn [c]
              (let [bot-production (node-production g c)]
                (let [top-root (uf/find uf top-production)
                      bot-root (uf/find uf bot-production)]
                  (and top-root
                       bot-root
                       (= top-root bot-root)))))
            relevant (case dir
                       :left (next cs) ;; all but the first child
                       :right (drop-last cs) ;; all but the last child
                       nil)
            violations (filter is-violation? relevant)]
        (some? (seq violations))))))

(defn- -resolve-associativity
  [g node parser dir]
  (if-not (uf/empty? (get-in parser [:associativities dir]))
    (if-let [illegal (seq (filter #(violates-associativity? g % dir parser)
                                  (dag/successors g node)))]
      (cull-illegal-children g node illegal)
      g)
    g))

(defn- resolve-associativity
  [g node parser]
  (-> g
      (-resolve-associativity node parser :left)
      (-resolve-associativity node parser :right)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn disambiguate
  "Given a parse forest (as generated by reconstruct) and corresponding parser,
   return a new parse forest in which ambiguities resolved by disambiguating
   filters have been removed"
  [forest {:keys [attributes priorities associativities] :as parser}]
  (let [resolve-ambiguity
        (fn [g node]
          (-> g
              (resolve-prefer node parser)
              (resolve-associativity node parser)
              (resolve-priority node parser)))
        g (forest->dag forest)
        ;; resolve ambiguities from lowest subtrees up, since lower ambiguities
        ;; can prevent resolution of higher ones
        sorted-nodes (filter (ambiguous-nodes forest)
                             (reverse (dag/topological-sort g)))]
    (dag->forest (reduce resolve-ambiguity g sorted-nodes))))

