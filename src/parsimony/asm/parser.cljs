(ns parsimony.asm.parser
  "Wrapper API for Emscripten-based CYK and coloring algorithms"
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [parsimony.asm-impl-js]
            [parsimony.parser :as parser]
            [parsimony.util :refer [pprint-str]]
            [parsimony.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emscripten <-> CLJS Shim
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def empty-codec
  {:encode {}  ;; map from keywords to ints
   :decode {}  ;; map from ints to keywords
   :ignore #{} ;; encoded ints, NOT keywords
   :n 1        ;; we start at 1, since we reserve 0 as a nil sentinel
   })

(defn- gen-codec
  "Construct codec from the given CNF grammar"
  [ps]
  (reduce
   (fn [{:keys [n] :as acc} sym]
     (if (parser/terminal? sym)
       acc
       (-> acc
           (assoc-in [:encode sym] n)
           (assoc-in [:decode n] sym)
           (update :ignore into (when (parser/ignored? sym)
                                  (list n)))
           (update :n inc))))
   empty-codec
   (parser/all-syms ps)))

(defn- encode [codec sym]
  (get (:encode codec) sym 0))

(defn- decode [codec i]
  (get (:decode codec) i nil))

(defn- gen-cpp-grammar
  "Construct Emscripten Grammar instance."
  [codec ps]
  (let [g (new js/Module.Grammar
                     (:n codec)
                     (parser/max-productions-per-lhs ps))]
    (doseq [[l [r1 r2 :as rhs]] ps :when (= 2 (count rhs))]
      ;; ignore singleton rules since they're never used by CYK
      (let [l (encode codec l)
            r1 (encode codec r1)
            r2 (encode codec r2)]
        (.add g l r1 r2)))
    g))

(defn cpp-init-cyk
  "Construct Emscripten CYK instance. Initialize singletons and ignore set."
  [token-vec parser]
  (cond
    (empty? token-vec) (throw (ex-info "Empty token stream" {:causes #{:empty-token-stream}}))
    (empty? (:cnf parser)) (throw (ex-info "Empty grammar" {:causes #{:empty-grammar}}))
    :else
    (let [ps (seq (:cnf parser))
          t (system-time)]
      (let [codec (gen-codec ps)
            g (gen-cpp-grammar codec ps)
            cyk (new js/Module.CYK (:n codec) (count token-vec) g)]
        ;; initialize table with all nonterminals for rules of form A = %t
        (doseq [[i t] (map-indexed vector token-vec)]
          (doseq [nt (parser/token-matches ps t)]
            (.set_cyk cyk (encode codec nt) i 1)))
        ;; initialize ignore sets
        (doseq [nt (:ignore codec)]
          (.ignore cyk nt))
        {:cyk cyk
         :codec codec
         :exec-time (- (system-time) t)}))))

(defn cpp-run-cyk
  "Run CYK parser. Returns runtime in ms."
  [cyk]
  (let [t (system-time)]
    (.parse cyk)
    (- (system-time) t)))

(defn cpp-run-cyk-partial
  "Run CYK parser partially. Corresponds to CYK::parse_partial in parser.cpp. The first call should use l=2"
  [cyk l]
  (let [t (system-time)
        l' (.parse_partial cyk l)]
    {:l l' :exec-time (- (system-time) t)}))

(defn cpp-run-color
  "Run CYK colorizer. Returns runtime in ms."
  [cyk]
  (let [t (system-time)]
    (.colorize cyk)
    (- (system-time) t)))

(defn cpp-get-lmax
  [cyk]
  (.get_lmax cyk))

(defn cpp-init-color-partial
  "Initialize the partial CYK colorizer. Corresponds to CYK::init_colorize_partial in parser.cpp."
  [cyk]
  (let [t (system-time)]
    (.init_colorize_partial cyk)
    (- (system-time) t)))

(defn cpp-run-color-partial
  "Run CYK colorizer partially. Corresponds to CYK::colorize_partial in parser.cpp. The first call should use l=2"
  [cyk l]
  (let [t (system-time)
        l (.colorize_partial cyk l)]
    {:l l :exec-time (- (system-time) t)}))

(defn cpp-free
  "Free heap"
  [cyk]
  #_(console/warn "Freeing asm.parser CYK heap space")
  (.delete cyk))

(defn get-cyk
  "Return the true/false value at the given position in the CYK table"
  [codec cyk nt i l]
  (when-not (zero? l) ;; cyk table has no epsilon entries
    (.get_cyk cyk (encode codec nt) i l)))

(defn unset-cyk
  "Set the value at the given position in the CYK table to false"
  [codec cyk nt i l]
  (.unset_cyk cyk (encode codec nt) i l))

(defn set-cyk
  "Set the value at the given position in the CYK table to true"
  [codec cyk nt i l]
  (.set_cyk cyk (encode codec nt) i l))

(defn get-colors
  "Return the CLJ decoded colors at (i,l)"
  [codec cyk i l]
  (let [colors (.get_colors cyk i l)
        n (.size colors)]
    (into #{}
          (map (fn [i] (vector (decode codec (.nt colors i)) (.i colors i) (.l colors i))))
          (range 0 n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Succinct Coloring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare reconstruct)

(defn succinct-coloring
  [codec cyk token-vec parser coloring]
  (parser/-succinct-coloring #(apply reconstruct codec cyk token-vec parser %)
                             coloring))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse Tree Reconstruction (undoing the effect of CNF)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn applicable?
  [codec cyk token-vec sym i l]
  ;; either sym is a terminal found in the token stream
  ;; or it is a nonterminal in the corresponding cyk-table element
  (let [res
        (or (and (= l 1) (= (parser/->terminal (nth token-vec i)) sym))
            (get-cyk codec cyk sym i l))]
    res))

(defn derive-partitions
  [codec cyk token-vec nts i l]
  (cond
    (and
     (= 1 (count nts))
     (applicable? codec cyk token-vec (first nts) i l))
    [(list [(first nts) i l])]

    :else
    (when-let [nt (first nts)]
      (into []
            (mapcat identity)
            (for [l' (range 1 (inc l))]
              (if (applicable? codec cyk token-vec nt i l')
                (let [partitions (derive-partitions codec cyk token-vec (next nts) (+ i l') (- l l'))]
                  (into []
                        (map #(conj % [nt i l']))
                        partitions))
                nil))))))

(defn reconstruct
  "Given a CYK instance and the non-CNF productions that gave rise to the table, construct the corresponding parse forest
   for the string of length l starting at index i with the given root nonterminal"
  [codec cyk token-vec parser nt i l]
  (let [ps (parser/expand-optionals (:productions parser))] ;; don't want to deal with ?s inside of the grammar
    (loop [worklist (parser/candidate-productions ps nt i l)
           seen #{}
           forest #{}]
      (if-let [[p i l :as candidate] (first worklist)]
        (let [partitions (derive-partitions codec cyk token-vec (parser/rhs p) i l)
              seen (conj seen candidate)
              candidates (into #{}
                               (comp (mapcat identity) ;; remove 1 nesting level
                                     (mapcat #(apply parser/candidate-productions ps %))
                                     (remove seen))
                               partitions)
              forest (into forest (map #(vector [(parser/lhs p) i l] (vec %)) partitions))]
          (recur (into (next worklist) candidates)
                 seen
                 forest))
        forest))))
