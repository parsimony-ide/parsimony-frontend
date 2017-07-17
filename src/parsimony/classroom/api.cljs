(ns parsimony.classroom.api
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            [parsimony.models.editor :as editor]
            [parsimony.models.source :as source]
            [parsimony.console :as console]))

(defn answer-key? [source]
  (str/ends-with? (str (:source-path source)) "soln.clj"))

(defn lesson-plan? [source]
  (str/ends-with? (str (:source-path source)) "lesson-plan.clj"))

(defn ->answer-key-path [source-path]
  (str source-path ".soln.clj"))

(defn read-source [source]
  (try
    (reader/read-string (:string source))
    (catch js/Error e
      (console/error ::read-source {:exception e})
      nil)))

(defn get-answer-key
  [db editor-id]
  (when-let [source (editor/backing-source db editor-id)]
    (let [soln-source-path (->answer-key-path (:source-path source))]
      (when-let [soln-source (first (source/sources-with-path db soln-source-path))]
        (read-source soln-source)))))

(defn get-lesson-plan
  [db]
  (when-let [source
             (first
               (for [[_ source] (:sources db)
                     :when (lesson-plan? source)]
                 source))]
    (read-source source)))

(defn- previous-source-path
  [lesson-plan source-path]
  (->> lesson-plan
       (reverse)
       (partition 2 1)
       (filter #(= (first %) source-path))
       (first)
       (last)))

(defn answer-status [db source-id]
  {:pass? (boolean (get-in db [:classroom source-id]))
   :source-path (get-in db [:sources source-id :source-path])
   :source-id source-id})

(defn prerequisite-status [db source-id]
  (let [source (get-in db [:sources source-id])
        lesson-plan (get-lesson-plan db)]
    (if-let [source-path (previous-source-path lesson-plan (:source-path source))]
      ;; prerequisite found
      (let [source-id (:id (first (source/sources-with-path db source-path)))]
        (answer-status db source-id))
      ;; no prerequisite found
      {:pass? true})))

(defn force-pass [db source-path]
  (let [lesson-plan (get-lesson-plan db)
        force-one-pass
        (fn [db source-path]
          (if-let [source-id
                   (->> source-path
                        (parsimony.models.source/sources-with-path db)
                        (first)
                        (:id))]
            (assoc-in db [:classroom source-id] true)
            db))
        source-paths
        (for [sp lesson-plan :while (not= sp source-path)]
          sp)]
    (if (contains? (set lesson-plan) source-path)
      (do (console/debug ::force-pass {:source-paths source-paths})
          (reduce force-one-pass db source-paths))
      (do (console/warn ::force-pass :source-path-not-found {:source-path source-path})
          db))))
