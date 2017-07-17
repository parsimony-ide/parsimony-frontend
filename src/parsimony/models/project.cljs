(ns parsimony.models.project
  (:require [parsimony.models.colors :as colors]
            [parsimony.models.parse-dashboard :as parse-dashboard]
            [parsimony.models.preferences :as preferences]
            [parsimony.models.editor :as editor]
            [parsimony.models.source :as source]
            [parsimony.models.token-dashboard :as token-dashboard]
            [parsimony.util :refer [matches-schema? deep-merge deep-select-keys]]
            [parsimony.views.file-picker :as file-picker]
            [parsimony.views.tree-view :as tree-view]
            [re-frame.db]
            [schema.core :as s :include-macros true]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def schema {:id (s/maybe s/Uuid)
             :last-saved (s/maybe s/Num)
             :description (s/maybe s/Str)
             :sources {s/Num source/schema}
             :file-picker-state tree-view/flat-model-schema
             :token-dashboard (select-keys token-dashboard/schema [:samples])
             :parse-dashboard (select-keys parse-dashboard/schema [:samples])})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defaults
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def default-model
  {:id nil
   :last-saved nil
   :description nil
   :sources (sorted-map)
   :file-picker-state file-picker/default-model
   :token-dashboard (select-keys token-dashboard/default-model [:samples])
   :parse-dashboard (select-keys parse-dashboard/default-model [:samples])})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn new-project []
  {:post [(matches-schema? schema %)]}
  (merge default-model
         {:id (random-uuid)
          :last-saved (.now js/Date)}))

(defn db->project
  "Given a run-time app db, return the corresponding project map"
  [db]
  {:post [(matches-schema? schema %)]}
  (merge default-model
         (select-keys db [:sources :file-picker-state])
         {:id (if-let [project-id (:current-project db)]
                project-id
                (random-uuid))
          :last-saved (.now js/Date)
          :token-dashboard (select-keys (token-dashboard/pristine (:token-dashboard db)) [:samples])
          :parse-dashboard (select-keys (parse-dashboard/pristine (:parse-dashboard db)) [:samples])}))

(defn set-description
  [project description]
  (assoc project :description description))

(defn- -update-project-by-spec [p p' [kw path :as spec]]
  (case kw
    :merge (deep-merge p (deep-select-keys p' [path]))
    :replace (let [v (get-in p' path ::not-found)]
               (if (not= v ::not-found)
                 (assoc-in p path v)
                 p))
    ;; default
    (throw (ex-info "No matching clause in -update-project-by-spec"
                    {:causes #{:no-matching-clause}
                     :kw kw
                     :path path}))))

(defn update-project
  "Update project p with the contents of p'.  If called with only two
   arguments, then perform a shallow merge of p and p'.   If called with three
   arguments, then the third argument is a vector of path-specs specifying the
   strategy for merging, as follows:

   Each path-spec is a two-element vector of form [kw path]. kw is either
   :merge or :replace, and path is a keyseq.  :merge indicates that the
   projection of p' with respect to path will be deep merged into p.  :replace
   indicates that the given path into p will be overwritten by the
   corresponding value in p'. In either case, if a path does not exist in p',
   then the corresponding path-spec is ignored."
  ([p p']
   (update-project p p' nil))
  ([{:keys [description] :as p} p' path-specs]
   (let [project (if (seq path-specs)
                   (reduce
                     (fn [p spec]
                       (-update-project-by-spec p p' spec))
                     p
                     path-specs)
                   (merge p p'))]
     (if-not (:description project)
       (assoc project :description description)
       project))))

(defn db-modified?
  "Return true iff db has changed since the last project save, or if the db has
   never been saved"
  [db]
  (let [project (db->project db)
        project-id (:id project)
        existing-project (get-in db [:projects project-id])]
    (or (seq (editor/unsaved-editor-ids db))
        (not= (dissoc project :id :description :last-saved)
              (dissoc existing-project :id :description :last-saved)))))

(defn load-project-into-db
  [db {:keys [token-dashboard parse-dashboard]
       project-id :id :as project}]
  (-> db
      (merge (select-keys project [:sources :file-picker-state]))
      (assoc :current-project project-id)
      (update :token-dashboard merge token-dashboard)
      (update :parse-dashboard merge parse-dashboard)))

(defn in-project? []
  ;; XXX: kind of dirty inspecting the DB atom directly like this
  (some? (:current-project @re-frame.db/app-db)))
