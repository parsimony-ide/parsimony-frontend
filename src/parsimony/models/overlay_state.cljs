(ns parsimony.models.overlay-state
  (:require [medley.core :refer [dissoc-in]]
            [parsimony.models.colors :as colors]
            [parsimony.models.overlay :as overlay]
            [parsimony.util :refer [overlay-seq]]
            [schema.core :as s :include-macros true]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schemas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def schema
  {:overlays (s/maybe {s/Keyword {s/Str overlay/schema}})
   :disabled-overlays (s/maybe #{[(s/one s/Keyword "overlay-type") (s/one s/Str "overlay-tag")]})
   :peek (s/maybe #{[(s/one s/Keyword "overlay-type") (s/one s/Str "overlay-tag")]}) ;; overlays that should be peeked (aka., disable all other overlays but this temporarily)
   })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defaults
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def default-value
  {:overlays (sorted-map)
   :disabled-overlays nil
   :peek nil})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn overlays-at-index
  "Return sequence of maps of form {:overlay-type :overlay-tag :decoration-index :decoration-mod :char-range}, one map for each overlay that
   intersects with the given index"
  [db {:keys [overlays disabled-overlays] :as overlay-state} index]
  (vec (for [{:keys [char-ranges decoration-index decoration-mod type tag]} (overlay-seq overlays)
             [start end _ :as char-range] char-ranges
             :when (and (not (contains? disabled-overlays [type tag]))
                        (<= start index)
                        (< index end))]
         {:overlay-type type
          :overlay-tag tag
          :decoration-index decoration-index
          :decoration-mod decoration-mod
          :char-range char-range})))

(defn- -allocate-decoration-index [db tag]
  (let [[affinities decoration-index] (colors/allocate-affinity (:decoration-affinities db) tag)]
    [(assoc db :decoration-affinities affinities) decoration-index]))

(defn- -attach-normal-overlay [db overlay-state char-ranges overlay-type overlay-tag decoration-mod]
  (letfn [(assign-decoration [[db overlay-state]]
            (if-not (some? (get-in overlay-state [:overlays overlay-type overlay-tag :decoration-index]))
              (let [[db decoration-index] (-allocate-decoration-index db overlay-tag)]
                [db (assoc-in overlay-state [:overlays overlay-type overlay-tag :decoration-index] decoration-index)])
              [db overlay-state]))
          (update-or-add-overlay [[db overlay-state]]
            (if-let [overlay (get-in overlay-state [:overlays overlay-type overlay-tag])]
              ;; overlay already exists, just update its char-ranges and nothing else
              (let [overlay (assoc overlay :char-ranges char-ranges)]
                [db (assoc-in overlay-state [:overlays overlay-type overlay-tag] overlay)])
              ;; overlay does not exist, create a new one
              [db (assoc-in overlay-state
                            [:overlays overlay-type overlay-tag]
                            (assoc overlay/default-value
                                   :decoration-mod decoration-mod
                                   :type overlay-type
                                   :tag overlay-tag
                                   :char-ranges char-ranges))]))]
    (-> [db overlay-state]
      (update-or-add-overlay)
      (assign-decoration))))

(defn inject-normal-overlays
  ([db overlay-state overlay-type overlays]
   (inject-normal-overlays db overlay-state overlay-type overlays nil))
  ([db overlay-state overlay-type overlays decoration-mod]
   (reduce (fn [[db overlay-state] [overlay-tag char-ranges]]
             (-attach-normal-overlay db overlay-state char-ranges overlay-type overlay-tag decoration-mod))
           [db overlay-state] (seq overlays))))

(defn- -attach-error-overlay [db overlay-state char-ranges overlay-tag]
  [db (assoc-in overlay-state [:overlays :error overlay-tag]
                (assoc overlay/default-value
                       :type :error
                       :tag overlay-tag
                       :char-ranges char-ranges
                       :active-infos (if (seq char-ranges)
                                       (sorted-set
                                         (dec (count char-ranges))) ;; make the last error visible by default
                                       (sorted-set))
                       :decoration-index colors/SEVERE))])

(defn inject-error-overlays [db overlay-state overlays]
  (reduce (fn [[db overlay-state] [overlay-tag char-ranges]]
            (-attach-error-overlay db overlay-state char-ranges overlay-tag))
          [db overlay-state] (seq overlays)))

(defn disable-overlay [overlay-state overlay-type overlay-tag]
  (update overlay-state :disabled-overlays (fnil conj (sorted-set)) [overlay-type overlay-tag]))

(defn disable-overlays-by-type [{:keys [overlays] :as overlay-state} overlay-type]
  (letfn [(f [overlay-state {:keys [type tag]}]
            (if (= type overlay-type)
              (disable-overlay overlay-state type tag)
              overlay-state))]
    (reduce f overlay-state (overlay-seq overlays))))

(defn enable-overlay [overlay-state overlay-type overlay-tag]
  (update overlay-state :disabled-overlays (fnil disj (sorted-set)) [overlay-type overlay-tag]))

(defn enable-overlays-by-type [{:keys [overlays] :as overlay-state} overlay-type]
  (letfn [(f [overlay-state {:keys [type tag]}]
            (if (= type overlay-type)
              (enable-overlay overlay-state type tag)
              overlay-state))]
    (reduce f overlay-state (overlay-seq overlays))))

(defn exclusively-enable-overlays-by-type [{:keys [overlays] :as overlay-state} overlay-type]
  (letfn [(f [overlay-state {:keys [type tag]}]
            (if (= type overlay-type)
              (enable-overlay overlay-state type tag)
              (disable-overlay overlay-state type tag)))]
    (reduce f overlay-state (overlay-seq overlays))))

(declare disabled-overlays)

(defn toggle-overlay [overlay-state overlay-type overlay-tag]
  (if (contains? (disabled-overlays overlay-state)
                 [overlay-type overlay-tag])
    (enable-overlay overlay-state overlay-type overlay-tag)
    (disable-overlay overlay-state overlay-type overlay-tag)))

(defn peek-overlay [overlay-state overlay-type overlay-tag]
  (update overlay-state :peek (fnil conj (sorted-set)) [overlay-type overlay-tag]))

(defn unpeek-overlay [overlay-state overlay-type overlay-tag]
  (update overlay-state :peek (fnil disj (sorted-set)) [overlay-type overlay-tag]))

(defn remove-overlay [overlay-state overlay-type overlay-tag]
  (-> overlay-state
      (dissoc-in [:overlays overlay-type overlay-tag])
      (update :overlays identity)))

(defn remove-overlays-by-type [overlay-state overlay-type]
  (let [overlay-tags (keys (get-in overlay-state [:overlays overlay-type]))]
    (reduce (fn [overlay-state overlay-tag]
              (remove-overlay overlay-state overlay-type overlay-tag))
            overlay-state overlay-tags)))

(defn remove-all-overlays [overlay-state]
  (assoc overlay-state :overlays (sorted-map)))

(defn has-overlays? [overlay-state]
  (boolean (seq (:overlays overlay-state))))

(defn apply-emphasis [overlay-state overlay-type overlay-tag key index mod]
  (if-let [overlay (get-in overlay-state [:overlays overlay-type overlay-tag])]
    (update-in overlay-state [:overlays overlay-type overlay-tag]
               overlay/apply-emphasis key index mod)
    overlay-state))

(defn apply-all-emphasis [{:keys [overlays] :as overlay-state} key mod]
  (letfn [(f [overlay-state {:keys [type tag]}]
            (update-in overlay-state [:overlays type tag]
                       overlay/apply-all-emphasis key mod))]
    (reduce f overlay-state (overlay-seq overlays))))

(defn clear-all-emphasis [{:keys [overlays] :as overlay-state} key]
  (letfn [(f [overlay-state {:keys [type tag]}]
            (update-in overlay-state [:overlays type tag] overlay/clear-all-emphasis key))]
    (reduce f overlay-state (overlay-seq overlays))))

(defn all-overlay-types [{:keys [overlays] :as overlay-state}]
  (keys overlays))

(defn all-overlay-tags [{:keys [overlays] :as overlay-state} overlay-type]
  (keys (get overlays overlay-type)))

(defn all-overlays [{:keys [overlays] :as overlay-state}]
  overlays)

(defn normal-overlays [overlay-state]
  (dissoc (all-overlays overlay-state) :error))

(defn error-overlays [overlay-state]
  (select-keys (all-overlays overlay-state) [:error]))

(defn disabled-overlays [{:keys [disabled-overlays] :as overlay-state}]
  disabled-overlays)

(defn get-overlay [{:keys [overlays] :as overlay-state} overlay-type overlay-tag]
  (get-in overlays [overlay-type overlay-tag]))

(defn all-peek [{:keys [peek] :as overlay-state}]
  peek)

(defn has-error? [{:keys [overlays] :as overlay-state}]
  (some? (:error overlays)))
