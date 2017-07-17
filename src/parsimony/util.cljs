(ns parsimony.util
  (:require [cljs.pprint :refer [pprint]]
            [cljs.reader :as reader]
            [clojurewerkz.balagan.core :as balagan]
            [clojure.string :as str]
            [com.rpl.specter :refer [transform setval select ALL LAST]]
            [medley.core :refer [dissoc-in]]
            [re-com.util :refer [deref-or-value]]
            [schema.core :as s]
            [parsimony.console :as console]))

 ;; bind to true to enable output from (inspect ...) and (inspect-pp ...) calls
(def ^:dynamic *inspect* nil)

(declare pprint-str)

;; see re-frame todomvc example app
(defn check-and-throw
  "Throw an exception if x doesn't match the schema."
  ([a-schema x]
   (check-and-throw a-schema x nil))
  ([a-schema x tag]
   (if-let [problems (s/check a-schema x)]
     (throw (ex-info (str "schema check failed")
                     (merge {:problems problems}
                            #_{:actual x}
                            (when (some? tag)
                              {:tag tag}))))
     x)))

(defn matches-schema?
  "Return true iff x matches the schema."
  ([a-schema x]
   (matches-schema? a-schema x nil))
  ([a-schema x msg]
   (try
     (check-and-throw a-schema x)
     true
     (catch js/Error e
       (let [debug-args
             (-> []
                 (into (when msg [msg]))
                 (conj (ex-data e))
                 (conj (.-message e)))]
         (console/error :matches-schema? :failure debug-args))
       false))))

(defn rapply
  "Like apply, but the args sequence comes first"
  ([f args & more]
   (if (seq more)
     (apply f (concat args more))
     (apply f args))))

(defn map-values
  "Like map, but only over the values of the given map. Keys remain unmodified."
  [f m]
  (into {}
        (map (fn [[k v]]
               [k (f v)]))
        m))

(defn vec-non-empty
  "Like vec, but returns nil if (seq xs) evaluates to nil"
  [xs]
  (when (seq xs)
    (vec xs)))

(defn vec-remove
  "Remove an element at specified index in the given vector, if the index exists, otherwise just return coll
  unmodified"
  [coll idx]
  {:pre [(vector? coll)]}
  (if (<= 0 idx (dec (count coll)))
    (vec (concat (subvec coll 0 idx) (subvec coll (inc idx))))
    coll))

(defn vec-insert
  "Insert an element at specified index in the given vector. If the index is >= (count coll), then insert at end. If an
  element exists at index, then move that and all subsequent elements to the right to make room. This is pretty
  inefficient so only use on small vectors. "
  [coll idx elem]
  {:pre [(vector? coll)
         (>= idx 0)]}
  (if (> idx (count coll))
    (conj coll elem)
    (vec (concat (conj (subvec coll 0 idx) elem) (subvec coll idx)))))


(defn third [xs]
  (nth (seq xs) 2))

(defn fourth [xs]
  (nth (seq xs) 3))

(defn find-first [pred xs]
  (first (filter pred xs)))

(defn filter-not [pred coll]
  (filter (complement pred) coll))

(def lfirst
  (comp last first))

(def llast
  (comp last last))

(def flast
  (comp first last))

(defn find-first-index
  "Return the first index into sequence xs at which pred evaluates to true on (nth xs index)"
  [pred xs]
  (first (find-first #(pred (second %)) (map-indexed vector xs))))

(defn- modify-if-exists [f m ks v]
  (if (get-in m ks)
    (f m ks v)
    m))

(defn- modify-unless-exists [f m ks v]
  (if-not (get-in m ks)
    (f m ks v)
    m))

(def assoc-if-exists (partial modify-if-exists assoc-in))
(def assoc-unless-exists (partial modify-unless-exists assoc-in))

(defn assoc-non-nil
  "Assoc only if value is non-nil"
  [m k v]
  (if (some? v)
    (assoc m k v)
    m))

(defn conj-non-nil
  "Conj only if value is non-nil"
  [xs v]
  (if (some? v)
    (conj xs v)
    xs))

(defn dissoc-in-keep-empty
  "Like dissoc-in, but instead of dissociating empty structures, keep them"
  [m ks]
  (if (> (count ks) 1)
    (let [ks' (drop-last ks)
          v (get-in m ks')
          m (dissoc-in m ks)]
      (if-not (get-in m ks')
        (assoc-in m ks' (empty v))
        m))
    (dissoc-in m ks)))

;; See clojure.contrib.map-utils
(defn deep-merge-with [f & maps]
  (apply
    (fn m [& maps]
      (if (every? map? maps)
        (apply merge-with m maps)
        (apply f maps)))
    maps))

(defn deep-merge [& maps]
  (apply deep-merge-with (fn [_ y] y) maps))

(defn deep-select-keys [m keyseqs]
  (let [value-path-pairs (atom [])]
    (apply balagan/with-paths m
           (into []
                 (mapcat #(vector %
                                  (fn [& args]
                                    (swap! value-path-pairs conj args))))
                 keyseqs))
    (reduce
      (fn [acc [v p]]
        (assoc-in acc p v))
      {}
      @value-path-pairs)))

(defn cartesian-product
  "Given a collection of collections, return their cartesian product as a lazy sequence"
  [colls]
  (if (empty? colls)
    '(())
    (for [x (first colls)
          more (cartesian-product (rest colls))]
      (cons x more))))

(defn debug
  ([x] (debug "" x))
  ([msg x]
   (console/debug "DEBUG:" msg (str x))
   x))

(defn warn
  ([x] (warn "" x))
  ([msg x]
   (console/warn "WARNING:" msg (str x))
   x))

(defn error
  ([x] (error "" x))
  ([msg x]
   (console/error "ERROR:" msg (str x))
   x))

(defn pprint-str [x]
  (enable-console-print!)
  (with-out-str (pprint x)))

(defn newline-pprint-str [x]
  (str \newline (pprint-str x)))

(defn split-on-char [string pat]
  (->> (map-indexed vector string)
       (partition-by #(re-matches pat (second %)))
       (filter-not #(re-matches pat (second (first %))))
       (map #(vector (ffirst %) (inc (flast %))))
       vec))

(defn file-extension [path-str]
  (last (str/split path-str ".")))

(defn string-idx->line-col
  "Return {:line :col} map from index into string"
  [string idx]
  (reduce
    (fn [acc [i c]]
      (if (= i idx)
        acc
        (if (= \newline c)
          (-> acc
              (update :line inc)
              (assoc :col 1))
          (update acc :col inc))))
    {:line 1 :col 1}
    (->> (map vector (range) string)
         (filter #(<= (first %) idx)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Newline Handling
;; - special case to deal with multiple consecutive newlines and newlines at
;;   beginning/end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- xform
  "If a newline span of length > 1 is found, insert a 0-length char-range
  between them to represent the empty line."
  [span]
  (let [xf (fn [x] (list (assoc x 1 "")))]
    (if (re-matches (re-pattern \newline) (lfirst span))
      (map xf (next span))
      (list span))))

(defn- ->range [span]
  (if (= "" (lfirst span))
      (vector (ffirst span) (ffirst span))
      (vector (ffirst span) (inc (flast span)))))

(defn split-on-newline [string]
  (let [pat (re-pattern \newline)]
    (->> (map-indexed vector string)
         (cons [0 \newline]) ;; inserting a fake newline at the start handles the case where the string starts with a newline
         (partition-by #(re-matches pat (second %)))
         (mapcat xform)
         (map ->range)
         vec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random String Generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def alphanum (vec (map char (concat (repeat 20 32)
                                     (range 48 58)
                                     (range 66 91)
                                     (range 97 123)))))

(defn random-char []
  (nth alphanum (rand-int (count alphanum))))

(defn random-line [length]
  (apply str
         (take length (repeatedly random-char))))

(defn random-lines [max-line-length max-lines]
  (str/join \newline
            (take max-lines (repeatedly #(random-line (rand-int max-line-length))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dealing with Overlay Maps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn overlay-seq [all-overlays]
  (let [all-overlays (deref-or-value all-overlays)]
    (for [[overlay-type overlays] all-overlays
          [overlay-tag ol] overlays]
        ol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DOM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dom-visible?
  "Returns true if the given element or one of its ancestors has display: none. Does not work if element or any ancestor
  has fixed position"
  [element]
  ;; See https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/offsetParent for details
  (some? (.-offsetParent element)))

(defn dom-computed-style
  "Return the computed style value of the given CSS property on the given elemnt"
  [element property]
  (.getPropertyValue (js/getComputedStyle element nil) property))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Event Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn set-drag-data
  "Set data on the drag event. If type is not specified, then text/plain is assumed"
  ([event data]
   (set-drag-data event "text/plain" data))
  ([event type data]
   #_(console/debug (pr-str data))
   (-> event
       (.-dataTransfer)
       (.setData (str type) (pr-str data)))
   event))  ;; data put into a DataTransfer object must be a string

(defn get-drag-data
  "Return the data attached to this drag event as a Clojure data structure. If type is not specified,
  then text/plain is assumed"
  ([event]
   (get-drag-data event "text/plain"))
  ([event type]
   (-> event
       (.-dataTransfer)
       (.getData (str type))
       (reader/read-string))))

(defn read-drag-type [ty]
  (try
    (reader/read-string ty)
    (catch js/Error e
      ty)))

(defn get-drag-types
  "Return a set of the drag types defined on this drag event"
  [event]
  (->> event
       (.-dataTransfer)
       (.-types)
       (map read-drag-type)
       (set)))

(defn get-all-drag-data
  "Return map from type to data"
  [event]
  (reduce
    (fn [m ty]
      (assoc m ty (get-drag-data event ty)))
    {}
    (get-drag-types event)))

(defn drag-is-external?
  [event]
  (contains? (get-drag-types event) 'Files))

(defn get-drag-files
  "Return any local files available on the drag event data transfer."
  [event]
  (-> event
      (.-dataTransfer)
      (.-files) ;; js FileList object
      (array-seq))) ;; get seq over FileList

(defn js-read-text-file
  [file success-cb error-cb]
  (let [type (.-type file)]
    (if (or (re-seq #"^text" type)
            (= "application/json" type)
            ;; if unrecognized, then just try
            (= "" type))
      (let [rdr (js/FileReader.)]
        (set! (.-onload rdr) (fn [event]
                               (success-cb {:success true
                                            :filename (.-name file)
                                            :mime-type type
                                            :content (.-result (.-target event))
                                            :js-event event})))
        (set! (.-onerror rdr) (fn [event]
                                (error-cb {:success false
                                           :filename (.-name file)
                                           :mime-type type
                                           :js-event event
                                           :reason "on-error"})))
        (.readAsText rdr file))
      (error-cb {:success false
                 :filename (.-name file)
                 :mime-type type
                 :js-event nil
                 :reason (str "File " (.-name file) " has unrecognized mime-type " type)}))))

(defn get-drag-tags
  "Return a set of the drag tags defined on this drag event"
  [event]
  (let [read-or-nil (fn [s]
                      (try
                        (reader/read-string s)
                        (catch js/Error e
                          nil)))]
    (->> (get-drag-types event)
         (map read-or-nil)
         (filter keyword?)
         (set))))

(defn set-drag-tag
  "Create a type on this drag event as a tag. Tag must be a keyword. Its
  corresponding data is empty and should not be relied upon. This is basically a
  hack to allow transferring data between a drag-start and drag-enter, since
  drag-enter handlers are now allowed to inspect drag data (only types). See
  http://stackoverflow.com/q/11065803/128927 for details."
  [event tag]
  {:pre [(keyword? tag)]}
  (let [existing-tags (get-drag-tags event)]
    (when (contains? existing-tags tag)
      (throw (ex-info (str "Tag " tag " already exists on this drag event")
                      {:event event})))
    (set-drag-data event (pr-str tag) "tag")))

(defn show-drag-data
  ([event]
   (show-drag-data event "text/plain"))
  ([event type]
   (enable-console-print!)
   (->> (get-drag-data event type)
        (pprint-str)
        (console/debug "drag-data"))))

(defn set-drop-effect [event effect]
  {:pre [(#{:copy :move :link :none} effect)]}
  #_(console/debug "set-drop-effect" (pr-str effect))
  (-> event
      (.-dataTransfer)
      (.-dropEffect)
      (set! (name effect)))
  event)

(defn non-input-keycode? [keycode]
  (let [keycode-set
        (-> #{}
            (into (range 16 21)) ;; Shift, Ctrl, Alt, Pause, Break, CapsLock
            (conj 27) ;; Esc
            (into (range 33 41)) ;; Page Up/Down, End, Home, Arrows
            (conj 44) ;; PrScrn
            (conj 45) ;; Insert
            (conj 91) ;; Win
            (conj 93) ;; Win Menu
            (into (range 112 124)) ;; F1-F12
            (conj 144) ;; NumLock
            (conj 145) ;; ScrollLock
            )]
    (contains? keycode-set keycode)))

