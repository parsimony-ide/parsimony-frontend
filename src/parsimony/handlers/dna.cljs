(ns parsimony.handlers.dna
  "Basic building blocks for handlers. Functions in this namespace are not guaranteed to preserve application state
  invariants."
  (:require [parsimony.classroom :as classroom]
            [parsimony.comm :as comm]
            [parsimony.config :as config]
            [parsimony.db :as db]
            [parsimony.measurement :as measurement]
            [parsimony.storage :as storage]
            [parsimony.util :refer [check-and-throw find-first filter-not split-on-char assoc-unless-exists find-first-index pprint-str]]
            [parsimony.models.buffer :as buffer]
            [parsimony.models.colors :as colors]
            [parsimony.models.editor :as editor]
            [parsimony.models.focus-ring :as focus-ring]
            [parsimony.models.overlay :as models.overlay]
            [parsimony.models.parse-dashboard :as parse-dashboard]
            [parsimony.models.project :as project]
            [parsimony.models.project-picker :as project-picker]
            [parsimony.models.source :as source]
            [parsimony.models.token-dashboard :as token-dashboard]
            [parsimony.query :as q]
            [parsimony.com.neo-codemirror :as codemirror]
            [parsimony.views.cards :as cards]
            [parsimony.views.tree-view :as tree-view]
            [parsimony.views.file-picker :as file-picker]
            [parsimony.views.modal :as modal]
            [parsimony.views.workspace :as workspace]
            [parsimony.worker :as worker]
            [parsimony.workers.compile-lexer :as compile-lexer]
            [parsimony.workers.lexer :as lexer]
            [parsimony.workers.batch-lexer :as batch-lexer]
            [parsimony.workers.compile-parser :as compile-parser]
            [parsimony.workers.parser :as parser]
            [parsimony.workers.batch-parser :as batch-parser]
            [parsimony.workers.auto-parse :as auto-parse]
            [parsimony.workers.reformat :as reformat]
            [parsimony.workers.solver :as solver]
            [parsimony.workers.test-answer :as test-answer]
            [re-frame.core :refer [path trim-v after dispatch]]
            [schema.core :as s]
            [parsimony.console :as console]
            [clojure.set :as set]
            [clojure.string :as str])
  (:require-macros [parsimony.handlers.dna :refer [hf->]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Middleware
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; after an event handler has run, this middleware checks that
;; the app db still correctly matches the schema.
(def check-schema-mw (after #(check-and-throw db/schema % :db)))

;; TODO: another middleware called check-invariants-mw that ensures higher level invariants
;; are maintained.  For example, no orphan editors; no sources without file-picker link.

(def dev-middleware [check-schema-mw
                     #_measurement/log-event-interceptor
                     #_classroom/open-file-interceptor
                     trim-v])

(def production-middleware [#_measurement/log-event-interceptor
                            #_classroom/open-file-interceptor
                            trim-v])

(def middleware
  (if (= "dev" config/BUILD)
    dev-middleware
    production-middleware))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn hf-return
  "A special return for handler functions that allows threading a db along with a return value."
  [db ret]
  (vector db ret))

(defn hf-pure
  "Ignore the input value and return ret without modifying db"
  [db _ ret]
  (hf-return db ret))

(defn <-hf
  [db _]
  db)

(defn hf-debug
  [db ret]
  (console/debug "hf-debug" (pprint-str db) (pprint-str ret))
  (hf-return db ret))

(defn next-source-id
  [{:keys [sources] :as db}]
  (hf-return db ((fnil inc 0) (last (keys sources)))))

(defn next-buffer-id
  [{:keys [buffers] :as db}]
  (hf-return db ((fnil inc 0) (last (keys buffers)))))

(defn next-editor-id
  [{:keys [editors] :as db}]
  (hf-return db ((fnil inc 0) (last (keys editors)))))

(defn next-file-picker-id
  [{:keys [file-picker-state] :as db}]
  (hf-return db ((fnil inc 0) (last (sort (map :id file-picker-state))))))

(defn next-token-sample-id
  [{:keys [token-dashboard] :as db}]
  (hf-return db ((fnil inc 0) (last (sort (map :id (:samples token-dashboard)))))))

(defn next-parse-sample-id
  [{:keys [parse-dashboard] :as db}]
  (hf-return db ((fnil inc 0) (last (sort (map :sample-id (parse-dashboard/all-samples parse-dashboard)))))))

(defn next-parse-label-id
  [{:keys [parse-dashboard] :as db}]
  (hf-return db ((fnil inc 0) (last (sort (map :label-id (parse-dashboard/all-labels parse-dashboard)))))))

(defn reset-app-state [_]
  (hf-return db/default-value nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Network Communication
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn network-request [db message-id payload succ-cb fail-cb]
  (comm/request message-id payload succ-cb fail-cb)
  (hf-return db nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn set-source-attr
  "Set attribute on the given source"
  [db source-id attr-key attr-val]
  (console/debug ::set-source-attr {:source-id source-id
                                    :attr-key attr-key
                                    :attr-val attr-val})
  (if (and source-id (contains? (:sources db) source-id))
    (hf-return
     (assoc-in db [:sources source-id attr-key] attr-val)
     source-id)
    (hf-return db nil)))

(defn create-source
  ([db source-type source-path]
   (create-source db source-type source-path ""))
  ([db source-type source-path content]
   (let [[_ source-id] (next-source-id db)
         source
         (cond (= :scratch source-type)
               (assoc source/default-value
                      :id source-id
                      :string content)
               (#{:token :grammar :file} source-type)
               (assoc source/default-value
                      :id source-id
                      :string content
                      :source-type source-type
                      :source-path source-path)
               :else nil)]
     (if source
       (do (console/debug ::create-source {:source-id source-id
                                           :source (dissoc source :string)})
           (hf-return (update-in db [:sources] assoc source-id source)
                      source-id))
       (do (console/error ::create-source {:reason "Unrecognized source-type"
                                           :source-type source-type})
           (hf-return db nil))))))

(defn delete-sources
  [db sids]
  (hf-return
   (apply update db :sources dissoc sids)
   nil))

(defn delete-source
  [db sid]
  (delete-sources db [sid]))

(defn promote-source
  "If the given source is a scratch-file, promote it to a named file, otherwise do nothing."
  [db source-id]
  (let [{:keys [source-type] :as source} (get-in db [:sources source-id])]
    (console/debug ::promote-source {:source-id source-id :source-type source-type})
    (case source-type
      :scratch (hf-> db
                     (set-source-attr source-id :source-type :file)
                     (set-source-attr :source-path (source/source-path source)))
      (hf-return db source-id))))

(defn rename-source [db source-id new-source-name]
  (console/debug ::rename-source {:source-id source-id :new-source-name new-source-name})
  (hf-> db
        (promote-source source-id)
        (set-source-attr :source-path new-source-name)))

(defn set-default-tokens [db source-id]
  (console/debug ::set-default-tokens {:source-id source-id})
  (let [other-token-sources (filter #(= :token (:source-type %)) (vals (:sources db)))
        db (reduce #(first (set-source-attr %1 %2 :source-type :file)) db (map :id other-token-sources))]
    (hf-> db
          (promote-source source-id)
          (set-source-attr :source-type :token))))

(defn set-default-grammar [db source-id]
  (console/debug ::set-default-grammar {:source-id source-id})
  (let [other-grammar-sources (filter #(= :grammar (:source-type %)) (vals (:sources db)))
        db (reduce #(first (set-source-attr %1 %2 :source-type :file)) db (map :id other-grammar-sources))]
    (hf-> db
          (promote-source source-id)
          (set-source-attr :source-type :grammar))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn save-buffer [db buffer-id]
  (let [buffer (get-in db [:buffers buffer-id])
        source-id (:source-id buffer)
        source (get-in db [:sources source-id])]
    (if (some? source)
      (do (console/debug ::save-buffer {:buffer-id buffer-id})
          (hf-return
            (assoc-in db [:sources (:id source) :string] (:string buffer))
            nil))
      (do (console/debug ::save-buffer :skipping-unbacked-buffer {:buffer-id buffer-id :source-id source-id})
          (hf-return db nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Picker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn file-picker-toggle
  "Collapse a file-picker element"
  [db element-id]
  {:pre [(number? element-id)]}
  (hf-return
   (update-in db [:file-picker-state] tree-view/toggle-collapse-element element-id)
   element-id))

(defn file-picker-add-file
  "Create a new entry in :file-picker-state for the given source"
  ([db source-id]
   (file-picker-add-file db source-id 0))
  ([db source-id parent-id]
   (let [[_ element-id] (next-file-picker-id db)]
     (hf-return (update-in db [:file-picker-state] tree-view/add-element
                           {:id element-id
                            :parent parent-id
                            :targetable false
                            :data (file-picker/source-link source-id)})
                element-id))))

(defn file-picker-add-heading
  [db parent-id]
  (let [[_ heading-id] (next-file-picker-id db)]
    (hf-return (update-in db [:file-picker-state] tree-view/add-element
                          {:id heading-id
                           :parent parent-id
                           :targetable true
                           :data (file-picker/heading
                                  heading-id
                                  "<New Folder>"
                                  "zmdi zmdi-folder"
                                  true)})
               heading-id)))

(defn file-picker-rename-heading
  [db element-id fname]
  (hf-return
   (update-in db [:file-picker-state] tree-view/set-element-attr element-id [:data :fname] fname)
   element-id))

;; Note that source-element-id and target-element-id are element ids, NOT parsimony.model.source ids
(defn file-picker-move
  "Change the position of a file-picker element"
  [db source-element-id target-element-id idx]
  (hf-return
   (update db :file-picker-state tree-view/change-position-by-id source-element-id target-element-id idx)
   nil))

(defn file-picker-delete
  "Remove the given file-picker elements. Does not remove child elements or transitive dependencies."
  [db element-ids]
  (hf-return
   (update db :file-picker-state tree-view/remove-elements element-ids)
   nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare project-picker-add-project)

(defn create-new-project [db]
  (let [{:keys [id] :as project} (project/new-project)
        db (assoc-in db [:projects id] project)]
    (project-picker-add-project db id)))

(defn update-project-from-db
  ([db]
   (update-project-from-db db nil))
  ([{:keys [projects] :as db} path-spec]
   (let [project (project/db->project db)
         db (-> db
                (update-in [:projects (:id project)]
                           project/update-project
                           project
                           path-spec)
                (assoc :current-project (:id project)))]
     (project-picker-add-project db (:id project)))))

(defn delete-project [db project-id]
  (hf-return (update db :projects
                     dissoc
                     project-id)
             project-id))

(defn set-project-description [db project-id description]
  (hf-return (update-in db
                        [:projects project-id]
                        project/set-description
                        description)
             nil))

;; -----------------------------------------------------------------------------
;; Persistence
;; -----------------------------------------------------------------------------

(defn persist-project! [db project-id]
  (if-let [project (get-in db [:projects project-id])]
    (do (console/debug ::persist-project! {:project-id project-id})
        (storage/persist! project-id project)
        (hf-return db nil))
    (do (console/warn ::persist-project! :skipping :project-has-no-id)
        (hf-return db nil))))

(defn persist-buffer! [db buffer-id]
  (let [buffer (get-in db [:buffers buffer-id])
        source-id (:source-id buffer)]
    (if (some? source-id)
      (do (console/debug ::persist-buffer! {:buffer-id buffer-id :source-id source-id})
          (hf-> db
                (update-project-from-db [[:merge [:sources source-id]]])
                (persist-project!)))
      (do (console/debug ::persist-buffer! :skipping-unbacked-buffer {:buffer-id buffer-id})
          (hf-return db nil)))))

(defn persist-file-state! [db]
  (console/debug ::persist-file-state!)
  (hf-> db
        (update-project-from-db [[:replace [:sources]]
                                 [:replace [:file-picker-state]]])
        (persist-project!)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project Picker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn project-picker-add-project
  [db project-id]
  (if-not (tree-view/contains-element? (get-in db [:project-picker :tree]) project-id)
    (do (console/debug ::project-picker-add-project {:project-id project-id})
        (hf-return (update-in db [:project-picker :tree]
                              tree-view/add-element
                              {:id project-id
                               :parent 0
                               :targetable false
                               :data (project-picker/project project-id)})
                   project-id))
    (do (console/debug ::project-picker-add-project :already-exists {:project-id project-id})
        (hf-return db project-id))))

(defn project-picker-remove-project
  [db project-id]
  (hf-return (update-in db [:project-picker :tree] tree-view/remove-element project-id)
             nil))

(defn project-picker-toggle
  [db element-id]
  (hf-return
    (update-in db [:project-picker :tree] tree-view/toggle-collapse-element element-id)
    element-id))

(defn project-picker-move
  [db source-element-id target-element-id idx]
  (hf-return
    (update-in db [:project-picker :tree] tree-view/change-position-by-id source-element-id target-element-id idx)
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modebar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn select-mode [db mode]
  (console/debug ::select-mode {:mode mode})
  (hf-return
   (-> db
       (assoc :current-mode mode))
   mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workspace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare create-editor-from-scratch)

(defn create-workspace [db workspace-id layout-kw]
  (console/debug ::create-workspace {:workspace-id workspace-id :layout-kw layout-kw})
  ;; TODO: check that workspace-id is not already taken and that layout-kw is valid
  (hf-return
   (-> db
       (assoc-in [:workspaces workspace-id] (workspace/init-model workspace-id layout-kw))
       (assoc :current-workspace workspace-id))
   workspace-id))

(defn change-workspace-layout [db workspace-id layout-kw layout-args]
  (console/debug ::change-workspace-layout {:workspace-id workspace-id :layout-kw layout-kw :layout-args layout-args})
  (if-let [ws (get-in db [:workspaces workspace-id])]
    (hf-return (assoc-in db
                         [:workspaces workspace-id]
                         (assoc ws :layout-kw layout-kw :layout-args layout-args))
               workspace-id)
    (hf-return db nil)))

(defn create-editor-card [db editor-id workspace-id pile-id & {:as options}]
  (console/debug ::create-editor-card {:editor-id editor-id :workspace-id workspace-id :pile-id pile-id :options options})
  ;; TODO: check that workspace exists
  (hf-return
    (update-in db
               [:workspaces workspace-id]
               workspace/add-card pile-id (cards/editor-card editor-id) -1 :background (:background options))
    nil))

(defn create-project-picker-card [db workspace-id pile-id]
  (console/debug ::create-project-picker-card {:workspace-id workspace-id :pile-id pile-id})
  (hf-return
    (update-in db
               [:workspaces workspace-id]
               workspace/add-card pile-id (cards/project-picker-card) -1)
    workspace-id))

(defn create-file-picker-card [db workspace-id pile-id]
  (console/debug ::create-file-picker-card {:workspace-id workspace-id :pile-id pile-id})
  (hf-return
   (update-in db
              [:workspaces workspace-id]
              workspace/add-card pile-id (cards/file-picker-card) -1)
   workspace-id))

(defn create-overlay-picker-card [db workspace-id pile-id]
  (console/debug ::create-overlay-picker-card {:workspace-id workspace-id :pile-id pile-id})
  (hf-return
   (update-in db
              [:workspaces workspace-id]
              workspace/add-card pile-id (cards/overlay-picker-card) -1)
   workspace-id))

(defn create-token-dashboard-card [db workspace-id pile-id]
  (console/debug ::create-token-dashboard-card {:workspace-id workspace-id :pile-id pile-id})
  (hf-return
   (update-in db
              [:workspaces workspace-id]
              workspace/add-card pile-id (cards/token-dashboard-card) -1)
   workspace-id))

(defn create-live-parse-view-card [db workspace-id pile-id]
  (console/debug ::create-live-parse-view-card {:workspace-id workspace-id :pile-id pile-id})
  (hf-return
   (update-in db
              [:workspaces workspace-id]
              workspace/add-card pile-id (cards/live-parse-view-card) -1)
   workspace-id))

(defn create-parse-dashboard-card [db workspace-id pile-id]
  (console/debug ::create-parse-dashboard-card {:workspace-id workspace-id :pile-id pile-id})
  (hf-return
   (update-in db
              [:workspaces workspace-id]
              workspace/add-card pile-id (cards/parse-dashboard-card) -1)
   workspace-id))

(defn create-solver-card [db workspace-id pile-id]
  (console/debug ::create-solver-card {:workspace-id workspace-id :pile-id pile-id})
  (hf-return
   (update-in db
              [:workspaces workspace-id]
              workspace/add-card pile-id (cards/solver-card) -1)
   workspace-id))

(defn create-disambiguation-card [db workspace-id pile-id]
  (console/debug ::create-disambiguation-card {:workspace-id workspace-id :pile-id pile-id})
  (hf-return
   (update-in db
              [:workspaces workspace-id]
              workspace/add-card pile-id (cards/disambiguation-card) -1)
   workspace-id))

(defn create-log-card [db workspace-id pile-id]
  (console/debug ::create-log-card {:workspace-id workspace-id :pile-id pile-id})
  (hf-return
    (if (= "dev" config/BUILD)
      (update-in db
                 [:workspaces workspace-id]
                 workspace/add-card pile-id (cards/log-card) -1)
      db)
    workspace-id))

(defn focus-card
  "Activate and focus the given card.  If the card has a backing editor, then
   also update :current-editor."
  [db workspace-id pile-id idx]
  (console/debug ::focus-card {:workspace-id workspace-id :pile-id pile-id :idx idx})
  (if-let [card (workspace/get-card (get-in db [:workspaces workspace-id]) pile-id idx)]
    (let [db (as-> db db
               (update-in db [:workspaces workspace-id] workspace/activate-card pile-id idx)
               (if (workspace/focusable? card)
                 (let [card-key (workspace/card-key card)]
                   (-> db
                       (assoc :current-card card-key)
                       (update :focus-ring focus-ring/add card-key)))
                 db))]
      (if (cards/editor-link? card)
        (let [editor-id (cards/backing-editor-id card)
              editor (get-in db [:editors editor-id])]
          (when-let [cm (:codemirror editor)]
            (codemirror/focus cm))
          (hf-return (assoc db :current-editor editor-id) nil))
        (hf-return db nil)))
    (hf-return db nil)))

(defn focus-card-by-key
  [db workspace-id key]
  (console/debug ::focus-card-by-key {:workspace-id workspace-id :key key})
  (if-let [loc (first
                 (for [[card {:keys [pile-id idx]}]
                       (workspace/card-locs (get-in db [:workspaces workspace-id]))
                       :when (= key (workspace/card-key card))]
                   [pile-id idx]))]
    (apply focus-card db workspace-id loc)
    (hf-return db nil)))

(defn activate-card
  "Activate the given card. If the given card is in the same pile as the
   currently focused card, then also switch focus."
  [db workspace-id pile-id idx]
  (let [workspace (get-in db [:workspaces workspace-id])
        {focused-pile-id :pile-id} (workspace/card-key->card-loc workspace (:current-card db))]
    (if-not (= focused-pile-id pile-id)
      (hf-return
        (update-in db
                   [:workspaces workspace-id]
                   workspace/activate-card pile-id idx)
        nil)
      (focus-card db workspace-id pile-id idx))))

(defn activate-card-by-key [db workspace-id key]
  (console/debug ::activate-card-by-key {:workspace-id workspace-id :key key})
  (if-let [{:keys [pile-id idx]} (workspace/card-key->card-loc (get-in db [:workspaces workspace-id]) key)]
    (activate-card db workspace-id pile-id idx)
    (hf-return db nil)))


(defn remove-card [db workspace-id pile-id idx]
  (console/debug ::remove-card {:workspace-id workspace-id :pile-id pile-id :idx idx})
  (hf-return
   (update-in db
              [:workspaces workspace-id]
              workspace/remove-card pile-id idx)
   nil))

(defn move-card [db workspace-id from-pile-id from-idx to-pile-id to-idx card]
  (hf-return
   (update-in db
              [:workspaces workspace-id]
              workspace/move-card from-pile-id from-idx to-pile-id to-idx card)
   nil))

(defn pile-id-for-source
  "Return the preferred pile-id for opening the given source"
  [db workspace-id source-id]
  (let [source (get-in db [:sources source-id])
        layout-kw (get-in db [:workspaces workspace-id :layout-kw])
        {:keys [primary-pile secondary-pile]} (get workspace/layouts layout-kw)
        primary-pile (if primary-pile primary-pile 1)
        secondary-pile (if secondary-pile secondary-pile primary-pile)
        pile-id (if-not (contains? #{:token :grammar} (:source-type source))
                  secondary-pile
                  primary-pile)]
    (hf-return db pile-id)))

(defn active-cards [db workspace-id]
  (let [workspace (get-in db [:workspaces workspace-id])
        cards (workspace/active-cards workspace)]
    (hf-return db cards)))

(defn active-card-keys [db workspace-id]
  (let [[db cards] (active-cards db workspace-id)]
    (hf-return db
               (into []
                     (map workspace/card-key)
                     cards))))

(defn active-editor-ids [db workspace-id]
  (let [[db cards] (active-cards db workspace-id)]
    (hf-return db
               (into []
                     (comp (filter #(satisfies? cards/IEditorLink %))
                           (map cards/backing-editor-id))
                     cards))))

(defn active-sample-editor-ids [db workspace-id]
  (let [[db editor-ids] (active-editor-ids db workspace-id)]
    (hf-return db
               (into []
                     (filter (partial editor/sample-editor? db))
                     editor-ids))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn show-modal [db modal]
  {:pre [(satisfies? modal/IModal modal)]}
  (hf-return
   (assoc db :modal modal)
   nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-buffer-from-source [db source-id]
  {:pre [(get-in db [:sources source-id])]}
  (let [[_ buffer-id] (next-buffer-id db)
        source (get-in db [:sources source-id])
        buffer (assoc buffer/default-value :id buffer-id :string (:string source) :source-id source-id)]
    (console/debug ::create-buffer-from-source {:source-id source-id :buffer-id buffer-id})
    (hf-return
     (update db :buffers assoc buffer-id buffer)
     buffer-id)))

(defn create-unbacked-buffer
  "Create a buffer with no backing source"
  [db string]
  (let [[_ buffer-id] (next-buffer-id db)
        buffer (assoc buffer/default-value :id buffer-id :string string :source-id nil)]
    (console/debug ::create-unbacked-buffer {:buffer-id buffer-id :string string})
    (hf-return
     (update db :buffers assoc buffer-id buffer)
     buffer-id)))

(defn delete-buffer [db buffer-id]
  (hf-return
   (update db :buffers dissoc buffer-id)
   nil))

(defn delete-buffers [db bids]
  (hf-return
   (apply update db :buffers dissoc bids)
   nil))

(defn set-buffer-string [db buffer-id string]
  (if (contains? (:buffers db) buffer-id)
    (hf-return (assoc-in db [:buffers buffer-id :string] string)
               nil)
    (do (console/warn ::set-buffer-string :failure :no-such-buffer-id {:buffer-id buffer-id})
        (hf-return db nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-editor-from-buffer [db buffer-id tag]
  {:pre [(get-in db [:buffers buffer-id])]}
  (let [[_ editor-id] (next-editor-id db)
        editor (assoc editor/default-value
                      :id editor-id
                      :buffer-id buffer-id
                      :tag tag)]
    (console/debug ::create-editor-from-buffer {:buffer-id buffer-id :editor-id editor-id})
    (hf-return
     (update db :editors assoc editor-id editor)
     editor-id)))

(defn create-editor [db source-id tag]
  {:pre [(get-in db [:sources source-id])]}
  (console/debug ::create-editor {:source-id source-id :tag tag})
  (hf-> db
        (create-buffer-from-source source-id)
        (create-editor-from-buffer tag)))

(defn create-editor-from-scratch
  "Create an editor backed by a scratch file"
  [db tag]
  (hf-> db
        (create-source :scratch nil)
        (create-editor tag)))

(defn create-unbacked-editor
  "Create an editor with an unbacked buffer"
  [db string tag]
  (hf-> db
        (create-unbacked-buffer string)
        (create-editor-from-buffer tag)))

(defn delete-editor [db editor-id]
  (hf-return
   (update db :editors dissoc editor-id)
   nil))

(defn delete-editors [db eids]
  (hf-return
   (apply update db :editors dissoc eids)
   nil))

(defn set-editor-read-only [db eid]
  (hf-return
    (editor/set-read-only db eid)
    eid))

(defn clear-editor-selection [db eid]
  (hf-return
    (editor/clear-selection db eid)
    nil))

(defn toggle-editor-auto-parse [db eid]
  (hf-return
    (editor/toggle-auto-parse db eid)
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-worker [db cache-key]
  (if-let [worker (get-in db [:workers cache-key])]
    (hf-return db worker)
    (do (console/debug ::get-worker :no-worker-with-cache-key {:cache-key cache-key})
        (hf-return db nil))))

(defn- -maybe-new-worker [db cache-key worker-fn & args]
  (let [worker (get-in db [:workers cache-key] (apply worker-fn args))]
    (hf-return (assoc-in db [:workers cache-key] worker)
               worker)))

(defn get-or-create-worker
  "Create worker with the given cache-key, or just return it if already exists"
  [db {:keys [:algo token-editor-id cfg-editor-id target-editor-id] :as cache-key}]
  (case (:algo cache-key)
      :compile-lexer
      (-maybe-new-worker db cache-key
                         compile-lexer/compile-lexer-worker
                         token-editor-id)
      :lexer
      (-maybe-new-worker db cache-key
                         lexer/lexer-worker
                         token-editor-id
                         target-editor-id)
      :batch-lexer
      (-maybe-new-worker db cache-key
                         batch-lexer/batch-lexer-worker
                         token-editor-id
                         target-editor-id)
      :compile-parser
      (-maybe-new-worker db cache-key
                         compile-parser/compile-parser-worker
                         token-editor-id
                         cfg-editor-id)
      :parser
      (-maybe-new-worker db cache-key
                         parser/parser-worker
                         token-editor-id
                         cfg-editor-id
                         target-editor-id)
      :batch-parser
      (-maybe-new-worker db cache-key
                         batch-parser/batch-parser-worker
                         token-editor-id
                         cfg-editor-id
                         target-editor-id)
      :auto-parse
      (-maybe-new-worker db cache-key
                         auto-parse/auto-parse-worker
                         token-editor-id
                         cfg-editor-id)
      :solver
      (-maybe-new-worker db cache-key
                         solver/solver-worker
                         token-editor-id
                         cfg-editor-id)
      :test-answer
      (-maybe-new-worker db cache-key
                         test-answer/test-answer-worker
                         token-editor-id
                         cfg-editor-id
                         target-editor-id)
      :reformat
      (-maybe-new-worker db cache-key
                         reformat/reformat-worker
                         token-editor-id
                         cfg-editor-id)
      ;; default
      (hf-return db nil)))

(defn add-or-update-worker [db worker]
  (cond
    (satisfies? worker/IAsyncWorker worker)
    (hf-return (assoc-in db [:workers (worker/cache-key worker)] worker) nil)

    (satisfies? worker/IWorker worker)
    (hf-return (assoc-in db [:workers (worker/cache-key worker)] worker) nil)

    :else
    (do (console/error ::add-or-update-worker :invalid-protocol {:worker worker})
        (hf-return db nil))))

(defn reset-worker [db worker]
  (if (satisfies? worker/ISyncWorker worker)
    (do (console/debug ::reset-worker {:cache-key (worker/cache-key worker)})
        (hf-return (assoc-in db [:workers (worker/cache-key worker)] (worker/reset worker)) nil))
    (do (console/warn ::reset-worker
                      "worker does not implement ISyncWorker"
                      {:cache-key (worker/cache-key worker)})
        (hf-return db nil))))

(defn reset-worker-by-cache-key [db cache-key]
  (let [[db worker] (get-worker db cache-key)]
    (if (some? worker)
      (reset-worker db worker)
      (hf-return db nil))))

(defn reset-workers-by-cache-key [db cache-keys]
  (let [db (reduce (fn [db cache-key]
                     (first (reset-worker-by-cache-key db cache-key)))
                   db
                   cache-keys)]
    (hf-return db nil)))

(defn reset-all-workers [{:keys [workers] :as db}]
  (let [db (reduce (fn [db cache-key]
                     (first (reset-worker-by-cache-key db cache-key)))
                   db
                   (keys workers))]
    (hf-return db nil)))

;; TODO: remove this and inline into :workers-step-all handler
(defn step-worker [db cache-key]
  (if-let [worker (get-in db [:workers cache-key])]
    (let [worker' (worker/step worker db)]
      (hf-return (assoc-in db [:workers cache-key] worker')
                 worker'))
    (do (console/error ::step-worker :no-worker-with-cache-key {:cache-key cache-key})
        (hf-return db nil))))

(defn delete-worker-by-cache-key
  "Delete the worker with the given cache-key. Does not reset the worker before
   deletion."
  [db cache-key]
  (hf-return (update db :workers dissoc cache-key) nil))

(defn delete-workers-by-cache-key
  [db cache-keys]
  (let [db (reduce (fn [db cache-key]
                     (first (delete-worker-by-cache-key db cache-key)))
                   db
                   cache-keys)]
    (hf-return db nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Token Dashboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn token-dashboard-toggle
  [db element-id]
  (hf-return
    (update db :token-dashboard token-dashboard/toggle-category element-id)
    element-id))

(defn token-dashboard-move
  [db source-element-id target-element-id idx]
  (hf-return
    (update db :token-dashboard token-dashboard/move source-element-id target-element-id idx)
    nil))

(defn token-dashboard-uncollapse-category
  [db element-id]
  (hf-return
    (update db :token-dashboard token-dashboard/uncollapse-category element-id)
    nil))

(defn token-dashboard-deselect-all
  [db]
  (hf-return
    (update db :token-dashboard token-dashboard/deselect-all)
    nil))

(defn token-dashboard-select
  [db element-id]
  (hf-return
    (update db :token-dashboard token-dashboard/select element-id)
    element-id))

(defn add-token-sample [db string]
  (let [[_ sample-id] (next-token-sample-id db)
        string (str/trim string)]
    (if (seq string)
      (hf-return
        (update db :token-dashboard token-dashboard/add-sample sample-id string)
        sample-id)
      (hf-return db nil))))

(defn process-token-sample [db sample-id]
  (if-let [string (token-dashboard/get-sample-string (:token-dashboard db) sample-id)]
    (network-request db
                     :algo/horizon
                     [string]
                     (fn [horizon]
                       (console/debug ::process-token-sample :success {:string string :horizon horizon})
                       (dispatch [:set-token-horizon sample-id horizon]))
                     #(console/error ::process-token-sample :failure {:server-reply %}))
    (do (console/error ::process-token-sample :no-such-sample {:sample-id sample-id})
        (hf-return db nil))))

(defn process-token-sample-if-necessary [db sample-id]
  (if-let [horizon (token-dashboard/get-sample-horizon (:token-dashboard db) sample-id)]
    (do (console/debug ::process-token-sample-if-necessary
                       :already-up-to-date
                       {:sample-id sample-id :horizon horizon})
        (hf-return db nil))
    (process-token-sample db sample-id)))

(defn set-token-horizon [db sample-id horizon]
  (hf-return
    (update db :token-dashboard token-dashboard/set-horizon sample-id horizon)
    sample-id))

(defn delete-token-sample [db element-id]
  (hf-return
    (update db :token-dashboard token-dashboard/delete-sample element-id)
    nil))

(defn add-token-category [db parent-id]
  (let [[_ heading-id] (next-token-sample-id db)]
    (hf-return
      (update db :token-dashboard token-dashboard/add-category parent-id heading-id)
      heading-id)))

(defn delete-token-category [db element-id]
  (hf-return
    (update db :token-dashboard token-dashboard/delete-category element-id)
    nil))

(defn rename-token-category [db element-id new-name]
  (hf-return
    (update db :token-dashboard token-dashboard/rename-category element-id new-name)
    element-id))

(defn fetch-token-packages-if-necessary [db]
  (if-not (seq (get-in db [:token-dashboard :package-cache]))
    (hf-> db
          (network-request :algo/all-packages nil
                           (fn [packages]
                             (console/debug ::fetch-token-packages-if-necessary :success {:num-packages (count packages)})
                             (dispatch [:set-token-package-cache packages]))
                           #(console/error ::fetch-token-packages-if-necessary :failure {:reply %})))
    (do (console/debug ::fetch-token-packages-if-necessary :already-up-to-date)
        (hf-return db nil))))

(defn set-token-package-cache [db packages]
  (hf-return
    (update db :token-dashboard token-dashboard/set-package-cache packages)
    nil))

(defn fetch-token-example-strings [db id path]
  (hf-> db
        (network-request :algo/example-strings path
                         (fn [strings]
                           (console/debug ::fetch-token-example-strings :success {:num-strings (count strings)})
                           (dispatch [:set-token-example-strings id path strings]))
                         #(console/error ::fetch-token-example-strings :failure {:reply %}))))

(defn set-token-example-strings [db id path strings]
  (hf-return
    (update db :token-dashboard token-dashboard/set-example-strings id path strings)
    nil))

(defn fetch-category-horizon [db id]
  (let [strings (token-dashboard/get-nested-sample-strings (:token-dashboard db) id)]
    (hf-> db
          (network-request :algo/horizon
                           strings
                           (fn [horizon]
                             (console/debug ::fetch-category-horizon :success {:strings strings :result horizon})
                             (dispatch [:set-token-horizon id horizon]))
                           #(console/error ::fetch-category-horizon :failure {:strings strings :server-reply %})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse Dashboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -parse-dashboard-backing-source-id
  "If the given editor is a sample editor found in [:parse-dashboard :sample-editor-ids], then
   it has no backing source. Instead, return the source-id of its associated sample"
  [db editor-id]
  (if-let [sample-id (get (set/map-invert (get-in db [:parse-dashboard :sample-editor-ids])) editor-id)]
    (do (console/debug ::-parse-dashboard-backing-source-id :editor-has-matching-sample {:editor-id editor-id :sample-id sample-id})
        (:source-id (parse-dashboard/get-sample (:parse-dashboard db) sample-id)))
    (first (q/editors->sources db [editor-id]))))

(defn parse-dashboard-add-sample-via-editor
  ([db nt-str editor-id char-from char-to]
   (parse-dashboard-add-sample-via-editor db nt-str editor-id char-from char-to false))
  ([db nt-str editor-id char-from char-to negative?]
   (if-let [buffer-id (first (q/editors->buffers db [editor-id]))]
     (let [[_ sample-id] (next-parse-sample-id db)
           [_ label-id] (next-parse-label-id db)
           string (get-in db [:buffers buffer-id :string])
           label {:nt (keyword nt-str)
                  :char-from char-from
                  :char-to char-to
                  :type (if negative? :negative :positive)
                  :label-id label-id}
           source-id (-parse-dashboard-backing-source-id db editor-id)
           source-path (source/source-path (get-in db [:sources source-id]))
           #_(first (q/editors->sources db [editor-id]))
           new-sample (parse-dashboard/sample sample-id
                                              source-id
                                              source-path
                                              string
                                              [label])]
       (if-let [s (parse-dashboard/existing-sample (:parse-dashboard db) new-sample)]
         (hf-return (update db :parse-dashboard parse-dashboard/update-sample new-sample)
                    (:sample-id s))
         (hf-return (update db :parse-dashboard parse-dashboard/add-sample new-sample)
                    sample-id)))
     (hf-return db nil))))

(defn parse-dashboard-associate-editor
  "Associate the given editor-id with the given sample-id"
  [db editor-id sample-id]
  (if-not (get-in db [:parse-dashboard :sample-editor-ids sample-id])
    (hf-return (assoc-in db [:parse-dashboard :sample-editor-ids sample-id] editor-id)
               nil)
    (do (console/error ::parse-dashboard-associate-editor :editor-already-exists {:editor-id editor-id :sample-id sample-id})
        (hf-return db nil))))

(defn parse-dashboard-emphasize-label-overlay
  [{:keys [parse-dashboard] :as db} sample-id label-id]
  (let [editor-id (parse-dashboard/get-underlying-sample-editor-id parse-dashboard sample-id)
        {:keys [nt char-from char-to type] :as label} (parse-dashboard/get-label parse-dashboard sample-id label-id)
        tag (name nt)
        index (editor/polygon-index db editor-id type (name nt) char-from char-to)]
    (if (and editor-id label index)
      (hf-return
        (-> db
            (editor/apply-all-emphasis editor-id :parse-dashboard colors/invisible-mod)
            (editor/apply-emphasis editor-id type tag :parse-dashboard index {}))
        nil)
      (do (console/error ::parse-dashboard-emphasize-label-overlay :missing-required-args {:editor-id editor-id
                                                                                           :label label
                                                                                           :index index})
          (hf-return db nil)))))

(defn parse-dashboard-clear-all-emphasis
  [{:keys [parse-dashboard] :as db} sample-id]
  (let [editor-id (parse-dashboard/get-underlying-sample-editor-id parse-dashboard sample-id)]
    (if editor-id
      (hf-return
        (editor/clear-all-emphasis db editor-id :parse-dashboard)
        nil)
      (do (console/error ::parse-dashboard-clear-all-emphasis :missing-required-args {:editor-id editor-id})
          (hf-return db nil)))))

(defn parse-dashboard-peek-label
  [db sample-id label-id]
  (parse-dashboard-emphasize-label-overlay db sample-id label-id))

(defn parse-dashboard-unpeek-all-labels
  [{:keys [parse-dashboard] :as db} sample-id]
  (if-let [label-id (get-in parse-dashboard [:selected :label-id])]
    (hf-> db
          (parse-dashboard-clear-all-emphasis sample-id :parse-dashboard)
          (hf-pure sample-id)
          (parse-dashboard-emphasize-label-overlay label-id))
    (parse-dashboard-clear-all-emphasis db sample-id)))

(defn parse-dashboard-select-sample
  [db sample-id]
  (hf-> (-> db
            (update :parse-dashboard parse-dashboard/select-sample sample-id)
            (update-in [:parse-dashboard :samples] tree-view/exclusively-select-element sample-id))
        (parse-dashboard-clear-all-emphasis sample-id)))

(defn parse-dashboard-select-label
  [db sample-id label-id]
  (hf-> (update db :parse-dashboard parse-dashboard/select-label sample-id label-id)
        (parse-dashboard-clear-all-emphasis sample-id)
        (hf-pure sample-id)
        (parse-dashboard-emphasize-label-overlay label-id)))

(defn parse-dashboard-delete-label
  [db sample-id label-id]
  (let [db (-> db
               (update :parse-dashboard parse-dashboard/delete-label sample-id label-id)
               (update :parse-dashboard parse-dashboard/clear-label-status sample-id label-id))]
    (if (and (= sample-id (get-in db [:parse-dashboard :selected :sample-id]))
             (= label-id  (get-in db [:parse-dashboard :selected :label-id])))
      ;; if the currently selected label is deleted, then select the parent sample to clear the selection
      (parse-dashboard-select-sample db sample-id)
      (hf-return db nil))))

(defn parse-dashboard-move
  [db source-element-id target-element-id idx]
  (hf-return
    (update-in db [:parse-dashboard :samples] tree-view/change-position-by-id source-element-id target-element-id idx)
    nil))

(defn parse-dashboard-change-layout
  [db layout-args]
  (hf-return
    (update-in db [:parse-dashboard :layout] merge layout-args)
    nil))

(defn parse-dashboard-clear-all-status
  [db]
  (hf-return
    (update db :parse-dashboard parse-dashboard/clear-all-status)
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Live Parse View
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-tokens
  [db editor-id]
  (let [token-editor-id (first (q/token-editors db))
        cfg-editor-id (first (q/cfg-editors db))
        worker (get-in db [:workers (parser/->cache-key token-editor-id
                                                        cfg-editor-id
                                                        editor-id)])]
    (hf-return db (:target-tokens worker))))
