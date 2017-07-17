(ns parsimony.debug
  (:require [parsimony.workers.common :refer [get-workers-by-algo]]
            [parsimony.util :refer [pprint-str] :refer-macros [inspect inspect-pp with-inspection]]
            [parsimony.models.parse-dashboard :as parse-dashboard]
            [re-frame.db]
            [cljs.pprint :refer [pprint]]))

(defn- -elide [m keyseq]
  (if (some? (get-in m keyseq))
    (assoc-in m keyseq :...)
    m))

(defn- -cleanup-sample [sample]
  (-> sample
      (-elide [:string])
      (update :labels #(into [] (sort-by :label-id %)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -cleanup-overlay-tag [[tag overlay]]
  [tag (-elide overlay [:char-ranges])])

(defn- -cleanup-overlay-type [[type tag-overlays]]
  [type (into (sorted-map)
              (map -cleanup-overlay-tag)
              tag-overlays)])

(defn- -cleanup-overlays [overlays]
  (into (sorted-map)
        (map -cleanup-overlay-type)
        overlays))

(defn- -cleanup-overlay-state [overlay-state]
  (-> overlay-state
      (update :overlays -cleanup-overlays)))

(defn- -cleanup-edit [n {:keys [string] :as edit}]
  (assoc edit :string
         (if (> (count string) n)
           [(subs string 0 (inc n)) :...]
           string)))

(defn- -cleanup-history [{:keys [undone done] :as history}]
  {:done (into []
               (map (partial -cleanup-edit 20))
               done)
   :undone (into []
                 (map (partial -cleanup-edit 20))
                 undone)})

(defn- -reduce-editor-verbosity [editor level]
  (into (sorted-map)
        (as-> editor editor
          (update editor :history -cleanup-history)
          (case level
            1 (update editor :overlay-state -cleanup-overlay-state)
            2 (-elide editor [:overlay-state])
            editor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti cleanup (fn [db kw] kw))

(defmethod cleanup :parse-dashboard
  [db _]
  (into []
        (map -cleanup-sample)
        (sort-by :sample-id (parse-dashboard/all-samples (:parse-dashboard db)))))

(defmethod cleanup :editors
  [db _]
  (into (sorted-map)
        (:editors db)))

(defmethod cleanup :editors-1
  [db _]
  (into (sorted-map)
        (map (fn [[id editor]] [id (-reduce-editor-verbosity editor 1)]))
        (:editors db)))

(defmethod cleanup :editors-2
  [db _]
  (into (sorted-map)
        (map (fn [[id editor]] [id (-reduce-editor-verbosity editor 2)]))
        (:editors db)))

(defn cleanup-db
  ([keyseq]
   (cleanup-db @re-frame.db/app-db keyseq))
  ([db keyseq]
   (into {}
         (map #(vector % (cleanup db %)))
         keyseq)))

(defn pprint-db
  ([keyseq]
   (pprint-db @re-frame.db/app-db keyseq))
  ([db keyseq]
   (pprint (cleanup-db db keyseq))
   nil))
