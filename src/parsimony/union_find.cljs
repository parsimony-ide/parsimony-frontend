(ns parsimony.union-find
  "Implements weighted quick-union disjoint-set data structure without path
   compression"
  (:refer-clojure :exclude [find empty?]))

(defn new-union-find []
  {:id []            ;; the union-find array
   :index {}         ;; maps elems to their ids
   :reverse-index {} ;; maps ids to elems
   :count 0          ;; next unused id
   :size []          ;; size of set rooted at id
   })

(defn- encode [uf x]
  (get-in uf [:index x]))

(defn- decode [uf i]
  (get-in uf [:reverse-index i]))

(defn add [uf x]
  (if (encode uf x)
    uf
    (let [i (:count uf)]
    (-> uf
        (assoc-in [:index x] i)
        (assoc-in [:reverse-index i] x)
        (update :count inc)
        (assoc-in [:id i] i)
        (assoc-in [:size i] 1)))))

(defn- find-index [uf x]
  (if-let [i (encode uf x)]
    (loop [i i]
      (let [i' (get-in uf [:id i])]
        (if (not= i i')
          (recur i')
          i')))
    nil))

(defn find [uf x]
  (if-let [i (find-index uf x)]
    (decode uf i)
    nil))

(defn union [uf x y]
  (let [i (find-index uf x)
        j (find-index uf y)]
    (cond
      ;; either x or y not found
      (not (and i j)) uf
      ;; x and y are already in the same set
      (= i j) uf
      :else
      (let [si (get-in uf [:size i])
            sj (get-in uf [:size j])]
        (if (< si sj)
          (-> uf
              (assoc-in [:id i] j)
              (update-in [:size j] + si))
          (-> uf
              (assoc-in [:id j] i)
              (update-in [:size i] + sj)))))))

(defn ->sets [uf]
  (set (vals (reduce
               (fn [acc x] (update acc (find uf x) (fnil conj #{}) x))
               {}
               (keys (:index uf))))) )

(defn add-equivalence-class [uf x & xs]
  (let [uf (reduce add (add uf x) xs)]
    (reduce (fn [uf y] (union uf x y)) uf xs)))

(defn combine [uf uf']
  (reduce
    (fn [uf [e root]]
      (-> uf
          (add e)
          (add root)
          (union e root)))
    uf
    (for [elems (->sets uf')
          :let [root (first elems)]
          e elems]
      [e root])))

(defn nodes [uf]
  (set (keys (:index uf))))

(defn empty? [uf]
  (= 0 (:count uf)))
