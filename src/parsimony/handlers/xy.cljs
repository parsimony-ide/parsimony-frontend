(ns parsimony.handlers.xy
  "Higher level building blocks for handlers. Functions in this namespace must preserve application state invariants."
  (:require [cljs.reader :as reader]
            [clojure.set :as set]
            [parsimony.db :as db]
            [parsimony.disambiguation :as disambiguation]
            [parsimony.example-projects :as example-projects]
            [parsimony.handlers.dna :as dna :refer [<-hf hf-pure hf-return] :refer-macros [hf->]]
            [parsimony.parser :refer [definition-parser] :as parser]
            [parsimony.refactor.parser :as refactor.parser]
            [parsimony.storage :as storage]
            [parsimony.worker :as worker]
            [parsimony.workers.common :refer [get-worker-keys-by-algo]]
            [parsimony.workers.compile-lexer :as compile-lexer]
            [parsimony.workers.compile-parser :as compile-parser]
            [parsimony.workers.lexer :as lexer]
            [parsimony.workers.batch-lexer :as workers.batch-lexer]
            [parsimony.workers.parser :as workers.parser]
            [parsimony.workers.batch-parser :as workers.batch-parser]
            [parsimony.workers.auto-parse :as auto-parse]
            [parsimony.workers.reformat :as reformat]
            [parsimony.workers.solver :as workers.solver]
            [parsimony.workers.test-answer :as workers.test-answer]
            [parsimony.models.colors :as colors]
            [parsimony.models.editor :refer [inject-normal-overlays remove-overlays ->char-range -string]]
            [parsimony.models.overlay :as overlay]
            [parsimony.models.live-parse-view :as live-parse-view]
            [parsimony.models.parse-dashboard :as parse-dashboard]
            [parsimony.models.project :as project]
            [parsimony.models.solver :as models.solver]
            [parsimony.models.token-dashboard :as token-dashboard]
            [parsimony.query :as q]
            [parsimony.util :refer [dissoc-in-keep-empty find-first pprint-str file-extension]]
            [parsimony.views.tree-view :as tree-view]
            [parsimony.views.modal :as modal :refer [IModal]]
            [parsimony.views.workspace :as workspace]
            [re-frame.core :refer [dispatch]]
            [parsimony.console :as console]))

(defn create-source-with-type-detection [db source-path content]
  (let [ext (file-extension source-path)
        source-type (case (file-extension source-path)
                      "t" :token
                      "g" :grammar
                      ;; default
                      :file)
        [db source-id] (dna/create-source db source-type source-path content)]
    (if source-id
      (case source-type
        :token (dna/set-default-tokens db source-id)
        :grammar (dna/set-default-grammar db source-id)
        ;; default
        (hf-return db source-id))
      (hf-return db nil))))

(defn file-picker-upload [db filename content]
  (let [[db source-id] (create-source-with-type-detection db filename content)]
    (if source-id
      (dna/file-picker-add-file db source-id)
      (hf-return db nil))))

(defn open-source-in-workspace
  ([db source-id workspace-id]
   (open-source-in-workspace db source-id workspace-id nil))
  ([db source-id workspace-id options]
   (console/debug ::open-source-in-workspace {:source-id source-id
                                              :workspace-id workspace-id
                                              :options options})
   (if-not workspace-id
     (hf-return db nil)
     (if-let [{:keys [pile-id card-idx]} (first (q/sources->cards db workspace-id [source-id]))]
       ;; if any is an editor card whose underlying source is source-id
       (if (:background options)
         (hf-return db {:pile-id pile-id :idx card-idx})
         ;; then set the card as active in its pile
         (let [matching-card (get-in db [:workspaces workspace-id :cards pile-id :available card-idx])]
           (console/debug ::open-source-in-workspace :found-matching-card {:matching-card matching-card})
           (hf-> db
                 (dna/focus-card workspace-id pile-id card-idx)
                 (hf-pure {:pile-id pile-id :idx card-idx}))))
       ;; else create a new editor, create a card for it,
       ;; - If the source is a grammar or token file, then put the card in the workspace's primary pile.
       ;; - Otherwise, put the card in the workspace's secondary pile if it exists.
       (let [[_ pile-id] (dna/pile-id-for-source db workspace-id source-id)
             focus-if-not-background
             (fn [db _]
               (if-not (:background options)
                 (dna/focus-card db workspace-id pile-id -1)
                 (hf-return db nil)))]
         (hf-> db
               (dna/create-editor source-id "")
               (dna/create-editor-card workspace-id pile-id :background (:background options))
               (focus-if-not-background)
               (hf-pure {:pile-id pile-id :idx -1})))))))

(defn open-sources-in-workspace [db source-ids workspace-id]
  (let [db (reduce
            (fn [db sid]
              (first (open-source-in-workspace db sid workspace-id)))
            db
            source-ids)]
    (hf-return db nil)))

(defn annihilate-sources [db source-ids]
  "Scorch the earth. Any mention of source is removed from application state"
  (let [buffers-to-kill (q/sources->buffers db source-ids)
        editors-to-kill (q/sources->editors db source-ids)
        fp-elements-to-kill (q/sources->fp-elements db source-ids)
        workspace-id (:current-workspace db)
        cards-to-kill (q/sources->cards db (:current-workspace db) source-ids)
        kill-cards (fn [db]
                     (if workspace-id
                       (hf-return
                        (reduce
                         (fn [acc {:keys [pile-id card-idx]}]
                           (first (dna/remove-card acc workspace-id pile-id card-idx)))
                         db
                         ;; delete in reverse order so that multiple deletions in the same pile don't collide with each other
                         (reverse (sort-by :card-idx cards-to-kill)))
                        nil)
                       (hf-return db nil)))]
    (console/debug ::annihilate-sources
                   {:source-ids source-ids
                    :buffers-to-kill buffers-to-kill
                    :editors-to-kill editors-to-kill
                    :fp-elements-to-kill fp-elements-to-kill
                    :cards-to-kill cards-to-kill})
    (hf-> db
          (dna/delete-sources source-ids)
          (hf-pure buffers-to-kill)
          (dna/delete-buffers)
          (hf-pure editors-to-kill)
          (dna/delete-editors)
          (hf-pure fp-elements-to-kill)
          (dna/file-picker-delete)
          (kill-cards))))

(defn annihilate-card
  "Destroy any editor or buffer associated with the given card"
  [db workspace-id pile-id card-idx]
  (let [buffers-to-kill (q/card->buffers db workspace-id pile-id card-idx)
        editors-to-kill (q/card->editors db workspace-id pile-id card-idx)]
    (hf-> db
          (dna/delete-editors editors-to-kill)
          (hf-pure buffers-to-kill)
          (dna/delete-buffers)
          (hf-pure workspace-id)
          (dna/remove-card pile-id card-idx))))

;; technically, can use this to delete sources from the file picker as well
(defn annihilate-file-picker-heading
  "Destroy a file picker and all descendant sources, and and their transitive dependencies."
  [db element-id]
  (let [dead-elements (-> (:file-picker-state db)
                          (tree-view/get-descendant-ids element-id)
                          (conj element-id))
        dead-sources (mapcat (fn [fid] (q/fp-elements->sources db [fid])) dead-elements)]
    (console/debug ::annihilate-file-picker-heading {:dead-sources dead-sources})
    (hf-> db
          (dna/file-picker-delete dead-elements)
          (hf-pure dead-sources)
          (annihilate-sources))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn annihilate-project [db project-id]
  (console/debug ::annihilate-project {:project-id project-id})
  (storage/delete! project-id)
  (hf-> db
        (dna/delete-project project-id)
        (dna/project-picker-remove-project)))

(defn reset-db
  "Return db to pristine state, but keep projects, workspace layout, token package-cache, and comm status"
  [{:keys [projects project-picker current-workspace workspaces workers token-dashboard comm] :as db}]
  (let [workspaces (into (sorted-map)
                         (map (fn [[workspace-id workspace]]
                                [workspace-id (workspace/remove-cards-with-matching-key workspace #"editor-")]))
                         workspaces)
        token-package-cache (token-dashboard/get-package-cache token-dashboard)]
    (dna/reset-all-workers db)
    (hf-return (assoc db/default-value
                      :comm comm
                      :projects projects
                      :project-picker project-picker
                      :current-workspace current-workspace
                      :workspaces workspaces
                      :token-dashboard (token-dashboard/set-package-cache token-dashboard/default-model token-package-cache))
               nil)))

(defn close-current-project
  "Close the current project. Does NOT check that the current project has been
   saved: if that functionality is required, see
   close-current-project-with-confirmation."
  [db]
  (console/debug ::close-current-project)
  (hf-> db
        (reset-db)
        (hf-pure (:current-workspace db))
        (dna/focus-card-by-key "project-picker-card")))

(defn close-current-project-with-confirmation
  "Close the current project. If the current project has been modified without
   save, then show a modal to confirm."
  [db]
  (console/debug ::close-current-project-with-confirmation)
  (if (project/db-modified? db)
    (hf-return (assoc db :modal (modal/confirm-close-project-modal)) nil)
    (close-current-project db)))

(declare process-parse-sample)

(defn open-project [db project-id]
  (console/debug ::open-project {:project-id project-id})
  (letfn [(restore-parse-dashboard [{:keys [parse-dashboard] :as db}]
            (console/debug ::open-project
                           {:num-restored-parse-samples (count (parse-dashboard/all-samples parse-dashboard))})
            (reduce (fn [db sample-id]
                      (first (process-parse-sample db sample-id)))
                    db
                    (parse-dashboard/all-sample-ids parse-dashboard)))
          (restore-token-dashboard [{:keys [token-dashboard] :as db}]
            (console/debug ::open-project
                           {:num-restored-token-samples (count (token-dashboard/all-samples token-dashboard))})
            (reduce (fn [db sample-id]
                      (first (dna/process-token-sample-if-necessary db sample-id)))
                    (first (dna/fetch-token-packages-if-necessary db))
                    (token-dashboard/all-sample-ids token-dashboard)))]
    (if-let [project (get-in db [:projects project-id])]
      (let [db (-> db
                   (project/load-project-into-db project)
                   (restore-token-dashboard)
                   (restore-parse-dashboard))]
        (hf-return db nil))
      (do (console/error ::open-project :project-does-not-exist {:project-id project-id})
          (hf-return db nil)))))

;; -----------------------------------------------------------------------------
;; Persistence
;; -----------------------------------------------------------------------------

(defn- load-persisted-projects [db]
  (let [projects (storage/all)]
    (console/debug ::load-persisted-projects {:num-projects (count projects)})
    (let [db (reduce
               (fn [db project]
                 (first (dna/project-picker-add-project db (:id project))))
               (update db :projects merge projects)
               (->> (vals projects)
                    (sort-by (fn [{:keys [description last-saved]}]
                               [description (- last-saved)]))))]
      (hf-return db nil))))

(defn create-and-persist-new-project! [db]
  (console/debug ::create-and-persist-new-project!)
  (hf-> db
        (dna/create-new-project)
        (dna/persist-project!)))

(defn- save-all-buffers
  [db]
  (hf-return
   (reduce
    (fn [acc buffer-id]
      (first (dna/save-buffer acc buffer-id)))
    db
    (keys (:buffers db)))
   nil))

(defn save-and-persist-current-project! [{:keys [current-project] :as db}]
  (console/debug ::save-and-persist-current-project! {:current-project current-project})
  (hf-> db
        (save-all-buffers)
        (dna/update-project-from-db)
        (dna/persist-project!)))

(defn save-and-persist-current-project-as-new! [{:keys [current-project] :as db}]
  (console/debug ::save-and-persist-current-project-as-new!)
  (hf-> (assoc db :current-project nil)
        (save-and-persist-current-project!)))

(defn save-and-persist-card!
  [db workspace-id pile-id card-idx]
  (if-let [buffer-id (first (q/card->buffers db workspace-id pile-id card-idx))]
    (hf-> db
          (dna/save-buffer buffer-id)
          (hf-pure buffer-id)
          (dna/persist-buffer!))
    (hf-return db nil)))

(defn save-and-persist-editor!
  [db editor-id]
  (if-let [buffer-id (first (q/editors->buffers db [editor-id]))]
    (hf-> db
          (dna/save-buffer buffer-id)
          (hf-pure buffer-id)
          (dna/persist-buffer!))
    (hf-return db nil)))

(defn save-and-persist-token-dashboard!
  [db]
  (hf-> db
        (dna/update-project-from-db [[:replace [:token-dashboard :samples]]])
        (dna/persist-project!)))

(defn save-and-persist-parse-dashboard!
  [db]
  (hf-> db
        (dna/update-project-from-db [[:replace [:parse-dashboard :samples]]])
        (dna/persist-project!)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- topo-sort-deps
  "Given a collection of directed edges between dependencies, return topologically ordered sequence
   if possible, otherwise return nil"
  [edges]
  (let [no-incoming?
        (fn [edges m]
          (every? #(not= (second %) m)
                  edges))
        all-nodes (into #{} (map first) edges)
        start-nodes
        (into #{}
              (filter (partial no-incoming? edges))
              all-nodes)]
    (loop [sorted [] worklist start-nodes edges edges]
      #_(console/debug ::topo-sort-deps
                       {:sorted sorted
                        :worklist worklist
                        :edges edges})
      (if-let [n (first worklist)]
        (let [n-outgoing (into #{} (filter #(= n (first %))) edges)
              edges (into [] (remove n-outgoing) edges)
              ms (into #{}
                       (comp (map second)
                             (filter (partial no-incoming? edges)))
                       n-outgoing)]
          (recur (conj sorted n)
                 (-> worklist
                     (into ms)
                     (disj n))
                 edges))
        (if (seq edges)
          nil
          sorted)))))

(defn- resolve-worker-dependencies
  "Return the dependency graph for the worker with the given cache-key. If any dependency
  does not exist, also create it."
  [db cache-key]
  #_(console/debug ::resolve-worker-dependencies {:cache-key cache-key})
  (let [[db edges]
        (loop [db db worklist #{cache-key} seen #{} edges #{}]
          (if-let [dep (first worklist)]
            (let [[db worker] (dna/get-or-create-worker db dep)
                  deps (remove seen (worker/dependencies worker db))]
              (recur db
                     (-> worklist
                         (into deps)
                         (disj dep))
                     (conj seen dep)
                     (into edges
                           (map #(vector % dep))
                           deps)))
            [db edges]))]
    (if (seq edges)
      (hf-return db (topo-sort-deps edges))
      ;; only one worker, just return it
      (hf-return db [cache-key]))))

(defn run-worker-with-dependencies
  "Run the worker with the given cache key, ensuring that all dependencies are met first"
  [db cache-key]
  (let [[db deps] (resolve-worker-dependencies db cache-key)]
    (console/debug ::run-worker-with-dependencies {:algo (:algo cache-key)})
    (dispatch [:workers-step-all deps])
    (hf-return db nil)))

(defn run-worker-without-dependencies
  "Run the worker with the given cache key. Do not run dependencies first."
  [db cache-key]
  (console/debug ::run-worker-without-dependencies {:algo (:algo cache-key)})
  (dispatch [:workers-step-all [cache-key]])
  (hf-return db nil))

(defn- reset-and-delete-workers
  [db cache-keys]
  (hf-> db
        (dna/reset-workers-by-cache-key cache-keys)
        (hf-pure cache-keys)
        (dna/delete-workers-by-cache-key)))

(defn- stale-workers
  "Return set of cache-keys for workers referencing non-existent editor-ids"
 [{:keys [editors workers] :as db}]
 (let [active-editor-ids (set (keys editors))
       stale-cache-keys (for [cache-key (keys workers)
                              :when (seq (set/difference (set (vals (dissoc cache-key :algo)))
                                                         active-editor-ids))]
                          cache-key)]
   (set stale-cache-keys)))

(defn garbage-collect-workers
  [db]
  (let [cache-keys (stale-workers db)]
    (console/debug ::garbage-collect-workers {:cache-keys cache-keys})
    (reset-and-delete-workers db cache-keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lexer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn open-token-editor-card [db]
  (if-let [token-editor-id (first (q/token-editors db))]
    (hf-return db token-editor-id)
    (if-let [token-source-id (first (q/token-sources db))]
      (let [[db' _] (open-source-in-workspace db token-source-id (:current-workspace db) {:background true})
            token-editor-id (first (q/token-editors db'))]
        (hf-return db' token-editor-id))
      (dna/show-modal db (modal/no-token-definition-modal)))))

(defn ensure-token-editor
  "If a token editor exists, then run function f with two arguments: db and the
   token editor id.  If a token editor does not exist, first open a token
   editor, then run f."
  [db f]
  (if-let [token-editor-id (first (q/token-editors db))]
    (f db token-editor-id)
    (let [[db' token-editor-id] (open-token-editor-card db)]
      (if token-editor-id
        (f db' token-editor-id)
        (do (console/error ::ensure-token-editor :failure)
            (hf-return db' nil))))))

(defn compile-lexer
  [db]
  (ensure-token-editor db #(run-worker-with-dependencies %1 (compile-lexer/->cache-key %2))))

(defn run-lexer [db target-editor-id]
  (ensure-token-editor db #(run-worker-with-dependencies %1 (lexer/->cache-key %2 target-editor-id))))

(defn run-lexers
  "Run lexer on several files"
  [db editor-ids]
  (let [db (reduce
             (fn [db eid]
               (first (run-lexer db eid)))
             db
             editor-ids)]
    (hf-return db nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Batch Lexer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run-batch-lexer [db target-editor-id]
  (console/debug ::run-batch-lexer)
  (let [db (first (dna/activate-card-by-key db (:current-workspace db) "log-card"))]
    (ensure-token-editor db #(run-worker-with-dependencies %1 (workers.batch-lexer/->cache-key %2 target-editor-id)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn open-cfg-editor-card [db]
  (if-let [cfg-editor-id (first (q/cfg-editors db))]
    (hf-return db cfg-editor-id)
    (if-let [cfg-source-id (first (q/cfg-sources db))]
      (let [[db' _] (open-source-in-workspace db cfg-source-id (:current-workspace db) {:background true})
            cfg-editor-id (first (q/cfg-editors db'))]
        (hf-return db' cfg-editor-id))
      (dna/show-modal db (modal/no-cfg-definition-modal)))))

(defn ensure-cfg-editor
  "If a cfg editor exists, then run function f with two arguments: db and the
   cfg editor id.  If a cfg editor does not exist, first open a cfg editor,
   then run f."
  [db f]
  (if-let [cfg-editor-id (first (q/cfg-editors db))]
    (f db cfg-editor-id)
    (let [[db' cfg-editor-id] (open-cfg-editor-card db)]
      (if cfg-editor-id
        (f db' cfg-editor-id)
        (do (console/error ::ensure-cfg-editor :failure)
            (hf-return db' nil))))))

(defn ensure-token-and-cfg-editors
  "Run f with three arguments: db, token editor id, and cfg editor id. If
   either editor does not exist, create it first."
  [db f]
  (ensure-token-editor db
    (fn [db token-editor-id]
      (ensure-cfg-editor db #(f %1 token-editor-id %2)))))

(defn compile-parser
  [db]
  (console/debug ::compile-parser)
  (ensure-token-and-cfg-editors db #(run-worker-with-dependencies %1 (compile-parser/->cache-key %2 %3))))

(defn run-parser
  [db target-editor-id]
  (console/debug ::run-parser {:target-editor-id target-editor-id})
  (ensure-token-and-cfg-editors db #(run-worker-with-dependencies %1 (workers.parser/->cache-key %2 %3 target-editor-id))))

(defn run-parsers
  "Run parser on several files"
  [db editor-ids]
  (console/debug ::run-parsers {:editor-ids editor-ids})
  (let [db (reduce
             (fn [db eid]
               (first (run-parser db eid)))
             db
             editor-ids)]
    (hf-return db nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Batch Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run-batch-parser
  [db target-editor-id]
  (console/debug ::run-batch-parser)
  (let [db (first (dna/activate-card-by-key db (:current-workspace db) "log-card"))]
    (ensure-token-and-cfg-editors db #(run-worker-with-dependencies %1 (workers.batch-parser/->cache-key %2 %3 target-editor-id)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto Parse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run-auto-parse
  [db]
  (console/debug ::run-auto-parse)
  (ensure-token-and-cfg-editors db #(run-worker-with-dependencies %1 (auto-parse/->cache-key %2 %3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run Solver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run-solver
  [db]
  (let [[db' _] (-> db
                    (update :solver models.solver/reset)
                    (dna/activate-card-by-key (:current-workspace db) "solver-card"))]
    (ensure-token-and-cfg-editors db'
      (fn [db token-editor-id cfg-editor-id]
        (let [cache-key (workers.solver/->cache-key token-editor-id cfg-editor-id)]
          (run-worker-with-dependencies (first (dna/reset-worker-by-cache-key db cache-key)) cache-key))))))

(defn run-solver-continuation
  [db]
  (let [[db' _] (-> db
                    (dna/activate-card-by-key (:current-workspace db) "solver-card"))]
    (ensure-token-and-cfg-editors db'
      (fn [db token-editor-id cfg-editor-id]
        (let [cache-key (workers.solver/->cache-key token-editor-id cfg-editor-id)]
          (run-worker-with-dependencies db cache-key))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Answer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn test-answer
  [db editor-id]
  (console/debug ::test-answer)
  (let [db (first (save-and-persist-current-project! db))
        db (first (dna/activate-card-by-key db (:current-workspace db) "log-card"))]
    (ensure-token-and-cfg-editors db #(run-worker-with-dependencies %1 (workers.test-answer/->cache-key %2 %3 editor-id)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Refactor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn reformat-cfg [db cfg-editor-id]
  (console/debug ::reformat-cfg {:cfg-editor-id cfg-editor-id})
  (ensure-token-editor db #(run-worker-with-dependencies %1 (reformat/->cache-key %2 cfg-editor-id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode Switch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn switch-mode
  [db mode]
  (let [sources-to-open
        (case mode
          :token (q/token-sources db)
          :parse (q/cfg-sources db)
          :manual nil)]
    (hf-> db
          (dna/select-mode mode)
          (hf-pure sources-to-open)
          (open-sources-in-workspace (:current-workspace db)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Token Dashboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- token-dashboard-update-selection-and-visibility
  "If the currently selected element is a non-root category, then don't change
   selection. Otherwise, select the given sample-id."
  [{:keys [token-dashboard] :as db} sample-id]
  (let [selected-id (token-dashboard/get-selected-id token-dashboard)
        non-root-category-ids (-> (token-dashboard/all-category-ids token-dashboard)
                                  (set)
                                  (disj 0))
        nearest-category-id (token-dashboard/get-nearest-selected-category-id token-dashboard)]
    (if (contains? non-root-category-ids selected-id)
      (dna/token-dashboard-uncollapse-category db selected-id)
      (hf-> db
            (dna/token-dashboard-select sample-id)
            (hf-pure nearest-category-id)
            (dna/token-dashboard-uncollapse-category)))))

(defn add-and-process-token-sample [db string]
  (let [[db sample-id] (dna/add-token-sample db string)]
    (if sample-id
      (hf-> db
            (dna/fetch-token-packages-if-necessary)
            (hf-pure sample-id)
            (token-dashboard-update-selection-and-visibility)
            (hf-pure sample-id)
            (dna/process-token-sample))
      (do (console/warn ::add-and-process-token-sample :did-not-add-sample)
          (hf-return db nil)))))

(defn token-dashboard-accept
  [db path rule-name-str]
  (letfn [(-inner-fn [db token-editor-id]
            (let [buffer-id (first (q/editors->buffers db [token-editor-id]))
                  {:keys [string]} (get-in db [:buffers buffer-id])
                  {:keys [source-str]} (get-in db (into [:token-dashboard :package-cache] path))
                  rhs (token-dashboard/source-str-rhs source-str)]
              (if (and (seq rhs)
                       (seq rule-name-str))
                (let [rule-str (str rule-name-str " = " rhs)
                      db (first (dna/set-buffer-string db buffer-id (str string \newline rule-str)))
                      [db editor-ids] (dna/active-sample-editor-ids db (:current-workspace db))]
                  (run-lexers db editor-ids))
                (do (console/error ::token-dashboard-accept :no-source-str
                                   {:rhs rhs
                                    :rule-name-str rule-name-str})
                    (hf-return db nil)))))]
    (ensure-token-editor db -inner-fn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse Dashboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- attach-sample-overlays [db {:keys [sample-id] :as sample}]
  (if-let [editor-id (parse-dashboard/get-underlying-sample-editor-id (:parse-dashboard db) sample-id)]
    (let [{:keys [positive negative]} (parse-dashboard/sample->overlays sample)
          db (-> db
                 (remove-overlays editor-id :positive)
                 (remove-overlays editor-id :negative)
                 (inject-normal-overlays editor-id :positive positive colors/green-outline-mod)
                 (inject-normal-overlays editor-id :negative negative (colors/mix colors/red-outline-mod colors/hatch-45-mod)))]
      (hf-return db nil))
    (do (console/error ::attach-sample-overlays :no-editor-found-for-sample {:sample sample})
        (hf-return db nil))))

(defn process-parse-sample [{:keys [parse-dashboard] :as db} sample-id]
  (let [sample (parse-dashboard/get-sample parse-dashboard sample-id)]
    (if (contains? (:sample-editor-ids parse-dashboard) sample-id)
      (attach-sample-overlays db sample)
      (hf-> db
            (dna/create-unbacked-editor (:string sample) "")
            (dna/set-editor-read-only)
            (dna/parse-dashboard-associate-editor sample-id)
            (hf-pure sample)
            (attach-sample-overlays)))))

(defn add-and-process-parse-sample-via-editor
  "Add parse sample and create underlying buffers and editors if necessary"
  ([db nt-str editor-id char-from char-to]
   (add-and-process-parse-sample-via-editor db nt-str editor-id char-from char-to false))
  ([db nt-str editor-id char-from char-to negative?]
   (let [[db sample-id] (dna/parse-dashboard-add-sample-via-editor db nt-str editor-id char-from char-to negative?)]
     (if sample-id
       (hf-> db
             (process-parse-sample sample-id)
             (hf-pure sample-id))
       (do (console/error ::add-and-process-parse-sample-via-editor
                          "Failed to add parse sample"
                          {:nt-str nt-str
                           :editor-id editor-id
                           :char-from char-from
                           :char-to char-to
                           :negative? negative?})
           (hf-return db nil))))))

(defn add-and-process-parse-sample-label
  [db sample-id nt-kw char-from char-to negative?]
  (let [[_ label-id] (dna/next-parse-label-id db)
        db' (update db :parse-dashboard parse-dashboard/add-label
                    sample-id
                    {:nt nt-kw
                     :char-from char-from
                     :char-to char-to
                     :type (if negative? :negative :positive)
                     :label-id label-id})
        sample (parse-dashboard/get-sample (:parse-dashboard db') sample-id)]
    (attach-sample-overlays db' sample)))

(defn annihilate-parse-sample
  "Delete parse sample and any underlying buffers and editors"
  [db sample-id]
  (let [db (-> db
               (update-in [:parse-dashboard :samples] tree-view/deselect-element sample-id)
               (update-in [:parse-dashboard :samples] tree-view/remove-element sample-id)
               (update :parse-dashboard parse-dashboard/clear-sample-status sample-id))
        editor-id (get-in db [:parse-dashboard :sample-editor-ids sample-id])
        buffer-id (first (q/editors->buffers db [editor-id]))
        db (dissoc-in-keep-empty db [:parse-dashboard :sample-editor-ids sample-id])]
    (hf-> db
          (dna/delete-buffer buffer-id)
          (hf-pure editor-id)
          (dna/delete-editor))))

(defn annihilate-all-parse-samples
  "Delete all parse samples and all underlying buffers and editors"
  [db]
  (let [db (reduce
             (fn [db sample-id]
               (first (annihilate-parse-sample db sample-id)))
             db
             (parse-dashboard/all-sample-ids (:parse-dashboard db)))]
    (hf-return db nil)))

(defn annihilate-parse-sample-label-by-id
  [db sample-id label-id]
  (let [[db' _] (dna/parse-dashboard-delete-label db sample-id label-id)
        existing-labels (tree-view/get-element-attr (get-in db' [:parse-dashboard :samples])
                                                    sample-id [:data :labels])]
    (if (seq existing-labels)
      (if-let [sample (parse-dashboard/get-sample (:parse-dashboard db') sample-id)]
        (attach-sample-overlays db' sample)
        (hf-return db' nil))
      (annihilate-parse-sample db' sample-id))))

(defn annihilate-parse-sample-label
  [db sample-id nt-kw char-from char-to]
  (if-let [labels (parse-dashboard/all-labels-for (:parse-dashboard db) sample-id)]
    (if-let [{:keys [label-id]} (first (filter #(and (= (:nt %) nt-kw)
                                                     (= (:char-from %) char-from)
                                                     (= (:char-to %) char-to))
                                               labels))]
      (annihilate-parse-sample-label-by-id db sample-id label-id)
      (hf-return db nil))
    (hf-return db nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn solver-accept-solution
  [db]
  (letfn [(-inner-fn [{:keys [solver] :as db} cfg-editor-id]
            (let [buffer-id (first (q/editors->buffers db [cfg-editor-id]))]
              (try
                (let [parser' (-> (models.solver/try-solution-parser solver)
                                  (disambiguation/apply-candidate (models.solver/disambiguation-candidate solver))
                                  (parser/compile-parser))]
                  (let [db (-> db
                               (update :solver models.solver/reset)
                               (dna/set-buffer-string buffer-id (refactor.parser/reformat parser' :sort-productions true))
                               (first))
                        [db editor-ids] (dna/active-sample-editor-ids db (:current-workspace db))]
                    (console/debug ::solver-accept-solution
                                   {:cfg-editor-id cfg-editor-id
                                    :sample-editor-ids editor-ids
                                    :parser' parser'})
                    (hf-> db
                          (run-parsers editor-ids)
                          (run-solver))))
                (catch js/Error e
                  (console/warn ::solver-accept-solution :compile-failure
                                {:error (ex-data e)})
                  (hf-return db nil)))))]
    (ensure-cfg-editor db -inner-fn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Live Parse View
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn live-parse-view-accept-disambiguation [db candidate-id]
  (letfn [(-inner-fn [db cfg-editor-id]
            (let [buffer-id (first (q/editors->buffers db [cfg-editor-id]))]
              (let [state (:live-parse-view db)
                    candidate (live-parse-view/disambiguation-candidate state candidate-id)
                    parser (-> (live-parse-view/parser state)
                               (disambiguation/apply-candidate candidate)
                               (parser/compile-parser))
                    db (-> db
                           (update :live-parse-view live-parse-view/clear-disambiguations)
                           (dna/set-buffer-string buffer-id (refactor.parser/reformat parser :sort-productions true))
                           (first))
                    [db editor-ids] (dna/active-sample-editor-ids db (:current-workspace db))]
                (console/debug ::live-parse-view-accept-disambiguation
                               {:cfg-editor-id cfg-editor-id
                                :sample-editor-ids editor-ids
                                :candidate candidate
                                :parser parser})
                (run-parsers db editor-ids))))]
    (ensure-cfg-editor db -inner-fn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example Projects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn install-example-projects [db]
  (example-projects/install!)
  (load-persisted-projects db))
