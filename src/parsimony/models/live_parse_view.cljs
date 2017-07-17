(ns parsimony.models.live-parse-view
  (:require [schema.core :as s :include-macros true]
            [parsimony.disambiguation :as disambiguation]
            [parsimony.models.editor :as editor]
            [parsimony.models.focus-ring :as focus-ring]
            [parsimony.models.parse-dashboard :as parse-dashboard]
            [parsimony.parser :as parser]
            [parsimony.query :as q]
            [parsimony.util :refer [matches-schema? pprint-str] :refer-macros [with-inspection inspect-pp]]
            [parsimony.workers.parser :as workers.parser]
            [parsimony.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def schema {:editor-id (s/maybe s/Num)
             :sample-id (s/maybe s/Num)
             :parse-overlays s/Any
             :token-overlays s/Any
             :selected-overlay s/Any
             :parser s/Any
             :tokens s/Any
             :string (s/maybe s/Str)
             :forest s/Any
             :candidate-id (s/maybe s/Num)
             :disambiguations (s/maybe
                                {s/Num ;; candidate-id
                                 {:candidate-id s/Num
                                  :candidate disambiguation/candidate-schema
                                  (s/optional-key :forest) s/Any}})})

(def default-model {:editor-id nil
                    :sample-id nil
                    :parse-overlays nil
                    :token-overlays nil
                    :selected-overlay nil
                    :parser nil
                    :tokens nil
                    :string nil
                    :forest nil
                    :candidate-id nil
                    :disambiguations nil})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- underlying-parser-worker [db editor-id]
  (let [token-editor-id (first (q/token-editors db))
        cfg-editor-id (first (q/cfg-editors db))]
    (get-in db [:workers (workers.parser/->cache-key token-editor-id
                                                     cfg-editor-id
                                                     editor-id)])))

(defn- sample-for-editor [{:keys [buffers parse-dashboard] :as db} editor-id]
  (let [buffer-id (first (q/editors->buffers db [editor-id]))
        {:keys [source-id string] :as buffer} (get buffers buffer-id)
        matching-samples (into []
                               (comp (filter #(= (:source-id %) source-id))
                                     (filter #(= (:string %) string)))
                               (parse-dashboard/all-samples parse-dashboard))]
    (:sample-id (first matching-samples))))

(defn reset [db]
  (assoc db :live-parse-view default-model))

(defn refresh [db]
  (let [state (:live-parse-view db)]
    (if-let [editor-id (focus-ring/last-focused-sample-editor db)]
      (if-let [{:keys [compiled-parser target-tokens] :as worker} (underlying-parser-worker db editor-id)]
        (let [all-overlays (editor/overlays-at-cursor db editor-id)
              parse-overlays (into []
                                   (filter #(= :parse (:overlay-type %)))
                                   all-overlays)
              token-overlays (into []
                                   (filter #(= :tokens (:overlay-type %)))
                                   all-overlays)
              selected-overlay (if (= 1 (count parse-overlays))
                                 (first parse-overlays)
                                 (:selected-overlay state))
              string (editor/-string db editor-id)
              forest (when-let [{:keys [overlay-tag char-range]} selected-overlay]
                       (let [nt (keyword overlay-tag)]
                         (workers.parser/parse-forest worker nt char-range)))
              sample-id (sample-for-editor db editor-id)
              new-state {:editor-id editor-id
                         :sample-id sample-id
                         :parse-overlays parse-overlays
                         :token-overlays token-overlays
                         :selected-overlay selected-overlay
                         :parser compiled-parser
                         :tokens target-tokens
                         :string string
                         :forest forest}]
          (update db :live-parse-view merge new-state))
        (reset db))
      (reset db))))

(defn select-overlay [db overlay]
  (assoc-in db [:live-parse-view :selected-overlay] overlay))

(defn set-candidate-id [db candidate-id]
  (assoc-in db [:live-parse-view :candidate-id] candidate-id))

(defn clear-candidate-id [db]
  (set-candidate-id db nil))

(defn gen-disambiguation-preview [db candidate-id]
  (let [{:keys [forest parser] :as state} (:live-parse-view db)
        candidate (get-in state [:disambiguations candidate-id :candidate])
        parser' (parser/compile-parser (disambiguation/apply-candidate parser candidate))
        forest' (parser/disambiguate forest parser')]
    (assoc-in db [:live-parse-view :disambiguations candidate-id :forest] forest')))

(defn clear-disambiguations [state]
  (assoc state :disambiguations (:disambiguations default-model)))

(defn parser [state]
  (:parser state))

(defn disambiguation-candidate [state candidate-id]
  (get-in state [:disambiguations candidate-id :candidate]))

(defn synthesize-disambiguations [db]
  (let [{:keys [forest tokens string parser sample-id] :as state} (:live-parse-view db)
        sample (parse-dashboard/get-sample (:parse-dashboard db) sample-id)
        diagnoses (disambiguation/diagnose-forest forest tokens string parser sample)]
    #_(console/debug (pprint-str {:forest forest
                                  :tokens tokens
                                  :string string
                                  :parser parser
                                  :sample-id sample-id
                                  :sample sample
                                  :diagnoses diagnoses}))
    (->> diagnoses
         (map-indexed #(vector (inc %1) {:candidate-id (inc %1)
                                         :candidate %2}))
         (into {0 {:candidate-id 0
                   :candidate nil}}))))
