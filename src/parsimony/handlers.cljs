(ns parsimony.handlers
  (:require [parsimony.db :as db]
            [parsimony.query :as q]
            [parsimony.classroom.api :as classroom.api]
            [parsimony.classroom.ui :as classroom.ui]
            [parsimony.comm :as comm]
            [parsimony.commands.handlers]
            [parsimony.measurement :as measurement]
            [parsimony.handlers.dna
             :refer [<-hf hf-return hf-pure middleware production-middleware]
             :refer-macros [hf->]
             :as dna]
            [parsimony.handlers.xy :as xy]
            [parsimony.lexer :as lexer]
            [parsimony.parser :as parser]
            [parsimony.refactor.parser :as refactor.parser]
            [parsimony.disambiguation :as disambiguation]
            [parsimony.util :refer [check-and-throw find-first assoc-unless-exists pprint-str]]
            [parsimony.models.colors :as colors]
            [parsimony.models.editor :as editor]
            [parsimony.models.focus-ring :as focus-ring]
            [parsimony.models.live-parse-view :as live-parse-view]
            [parsimony.models.overlay-state :as overlay-state]
            [parsimony.models.project :as project]
            [parsimony.models.solver :as solver]
            [parsimony.models.source :as source]
            [parsimony.models.token-dashboard :as token-dashboard]
            [parsimony.models.log :as log]
            [parsimony.com.neo-codemirror :as codemirror]
            [parsimony.views.file-picker :as file-picker]
            [parsimony.views.modal :as modal]
            [parsimony.views.workspace :as workspace]
            [parsimony.worker :as worker]
            [parsimony.workers.solver :as workers.solver]
            [re-frame.core :refer [reg-event-db path trim-v after dispatch]]
            [schema.core :as s]
            [parsimony.console :as console]
            [clojure.string :as str]
            #_[clairvoyant.core :refer-macros [trace-forms]]
            #_[re-frame-tracer.core :refer [tracer]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (trace-forms {:tracer (tracer :color "green")}

;; Set up DB schema with default values
(reg-event-db
 :initialize-db
 middleware
 (fn initialize-db [_ _]
   #_(console/debug ::initialize-db)
   db/default-value))

;; Install initial app state into DB
(reg-event-db
 :initialize-app
 middleware
 (fn initialize-app [db _]
   (hf-> db
         (dna/create-workspace 1 :layout-3)
         (dna/create-project-picker-card 1)
         (dna/create-file-picker-card 1)
         (dna/create-overlay-picker-card 5)
         (dna/create-token-dashboard-card 4)
         (dna/create-parse-dashboard-card 4)
         (dna/create-live-parse-view-card 4)
         (dna/create-solver-card 4)
         (dna/create-log-card 4)
         (dna/focus-card 1 0)
         (hf-pure :manual)
         (dna/select-mode)
         (xy/load-persisted-projects)
         (<-hf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(reg-event-db
 :rename-source
 middleware
 (fn rename-source [db [source-id new-source-name]]
   (hf-> db
         (dna/rename-source source-id new-source-name)
         (dna/persist-file-state!)
         (<-hf))))

(reg-event-db
 :set-default-tokens
 middleware
 (fn set-default-tokens [db [source-id]]
   (hf-> db
         (dna/set-default-tokens source-id)
         (dna/persist-file-state!)
         (<-hf))))

(reg-event-db
 :set-default-grammar
 middleware
 (fn set-default-grammar [db [source-id]]
   (hf-> db
         (dna/set-default-grammar source-id)
         (dna/persist-file-state!)
         (<-hf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Picker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(reg-event-db
 :file-picker-toggle
 middleware
 (fn file-picker-toggle [db [element-id]]
   (console/debug ::file-picker-toggle {:element-id element-id})
   (hf-> db
         (dna/file-picker-toggle element-id)
         #_(dna/hf-debug)
         (<-hf))))

(reg-event-db
 :file-picker-click
 middleware
 (fn file-picker-click [db [source-id]]
   (console/debug ::file-picker-click {:source-id source-id})
   (first (xy/open-source-in-workspace db source-id (:current-workspace db)))))

(reg-event-db
 :file-picker-move
 middleware
 (fn file-picker-move [db [source-element-id target-element-id idx]]
   (console/debug ::file-picker-move {:source-element-id source-element-id
                                      :target-element-id target-element-id
                                      :idx idx})
   (hf-> db
         (dna/file-picker-move source-element-id target-element-id idx)
         (dna/persist-file-state!)
         #_(dna/hf-debug)
         (<-hf))))

(reg-event-db
  :file-picker-upload
  middleware
  (fn file-picker-upload [db [filename mime-type content]]
    (console/debug ::file-picker-upload {:filename filename
                                         :mime-type mime-type})
    (hf-> db
          (xy/file-picker-upload filename content)
          (dna/persist-file-state!)
          (<-hf))))

(reg-event-db
 :file-picker-delete
 middleware
 (fn file-picker-delete [db [source-id]]
   (let [element (find-first #(= source-id (get-in % [:data :source-id])) (:file-picker-state db))]
     (console/debug ::file-picker-delete {:element element})
     (hf-> db
           (xy/annihilate-sources [source-id])
           (dna/persist-file-state!)
           (<-hf)))))

(reg-event-db
 :file-picker-add-heading
 middleware
 (fn file-picker-add-heading [db [element-id]]
   (console/debug ::file-picker-add-heading {:element-id element-id})
   (hf-> db
         (dna/file-picker-add-heading element-id)
         (dna/persist-file-state!)
         (<-hf))))

(reg-event-db
 :file-picker-rename-heading
 middleware
 (fn file-picker-rename-heading [db [element-id new-name]]
   (console/debug ::file-picker-rename-heading {:element-id element-id
                                                :new-name new-name})
   (hf-> db
         (dna/file-picker-rename-heading element-id new-name)
         (dna/persist-file-state!)
         (<-hf))))

(reg-event-db
 :file-picker-add-scratch
 middleware
 (fn file-picker-add-scratch [db [element-id]]
   (console/debug ::file-picker-add-scratch {:element-id element-id})
   (hf-> db
         (dna/create-source :scratch nil)
         (dna/file-picker-add-file element-id)
         (dna/persist-file-state!)
         (<-hf))))

;; TODO: unify this with file-picker-delete
(reg-event-db
 :file-picker-delete-heading
 middleware
 (fn file-picker-delete-heading [db [element-id]]
   (console/debug ::file-picker-delete-heading {:element-id element-id})
   (hf-> db
         (xy/annihilate-file-picker-heading element-id)
         (dna/persist-file-state!)
         (<-hf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(reg-event-db
  :save-then-close-current-project
  middleware
  (fn save-then-close-current-project [db]
    (console/debug ::save-then-close-current-project)
    (hf-> db
          (xy/save-and-persist-current-project!)
          (xy/close-current-project)
          (<-hf))))

(reg-event-db
  :close-current-project-without-saving
  middleware
  (fn close-current-project-without-saving [db]
    (console/debug ::close-current-project-without-saving)
    (first (xy/close-current-project db))))

(reg-event-db
  :open-project
  middleware
  (fn open-project [db [project-id]]
    (console/debug ::open-project {:project-id project-id})
    (hf-> db
          (xy/close-current-project)
          (hf-pure project-id)
          (xy/open-project)
          (hf-pure (:current-workspace db))
          (dna/focus-card-by-key "file-picker-card")
          (<-hf))))

(reg-event-db
  :delete-project
  middleware
  (fn delete-project [db [project-id]]
    (console/debug ::delete-project {:project-id project-id})
    (first (xy/annihilate-project db project-id))))

(reg-event-db
  :set-project-description
  middleware
  (fn set-project-description [db [project-id description]]
    (console/debug ::set-project-description {:project-id project-id
                                              :description description})
    (hf-> db
          (dna/set-project-description project-id description)
          (hf-pure project-id)
          (dna/persist-project!)
          (<-hf))))

(reg-event-db
  :create-new-project
  middleware
  (fn create-new-project [db _]
    (console/debug ::create-new-project)
    (first (xy/create-and-persist-new-project! db))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project Picker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(reg-event-db
  :project-picker-toggle
  middleware
  (fn project-picker-toggle [db [element-id]]
    (console/debug ::project-picker-toggle {:element-id element-id})
    (first (dna/project-picker-toggle db element-id))))

(reg-event-db
  :project-picker-move
  middleware
  (fn project-picker-move [db [source-element-id target-element-id idx]]
    (console/debug ::project-picker-move {:source-element-id source-element-id
                                          :target-element-id target-element-id
                                          :idx idx})
    (first (dna/project-picker-move db source-element-id target-element-id idx))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workspace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(reg-event-db
 :create-workspace
 middleware
 (fn create-workspace [db [workspace-id layout-kw]]
   (hf-> db
         (dna/create-workspace workspace-id layout-kw)
         (<-hf))))

;; XXX: This should not exist. Editor cards should only be opened from the file picker, not by creating an editor directly.
(reg-event-db
 :create-scratch-card-in-workspace
 middleware
 (fn create-scratch-card-in-workspace [db [workspace-id pile-id]]
   (hf-> db
         (dna/create-editor-from-scratch "scratch")
         (dna/create-editor-card workspace-id pile-id)
         (<-hf))))

(reg-event-db
 :create-file-picker-in-workspace
 middleware
 (fn create-file-picker-in-workspace [db [workspace-id pile-id]]
   (hf-> db
         (dna/create-file-picker-card workspace-id pile-id)
         (<-hf))))

(reg-event-db
 :change-workspace-layout
 middleware
 (fn change-workspace-layout [db [workspace-id layout-kw layout-args]]
   (hf-> db
         (dna/change-workspace-layout workspace-id layout-kw layout-args)
         (<-hf))))

(reg-event-db
 :card-click
 middleware
 (fn card-click [db [workspace-id pile-id idx]]
   (console/debug ::card-click {:workspace-id workspace-id
                                :pile-id pile-id
                                :idx idx})
   (first (dna/focus-card db workspace-id pile-id idx))))

(reg-event-db
 :card-close
 middleware
 (fn card-close [db [workspace-id pile-id idx]]
   (console/debug ::card-close {:workspace-id workspace-id
                                :pile-id pile-id
                                :idx idx})
   (hf-> db
         (xy/save-and-persist-card! workspace-id pile-id idx)
         (hf-pure workspace-id)
         (xy/annihilate-card pile-id idx)
         (<-hf))))

(reg-event-db
 :card-discard
 middleware
 (fn card-discard [db [workspace-id pile-id idx]]
   (console/debug ::card-discard {:workspace-id workspace-id
                                  :pile-id pile-id
                                  :idx idx})
   (first (xy/annihilate-card db workspace-id pile-id idx))))

(reg-event-db
 :card-move
 middleware
 (fn card-move [db [workspace-id pile-id idx card-info]]
   (console/debug ::card-move {:workspace-id workspace-id
                               :pile-id pile-id
                               :idx idx
                               :card-info card-info})
   (let [{card :card from-pile-id :pile-id from-idx :idx} card-info]
     (hf-> db
           ;; always focus card being moved
           (dna/focus-card workspace-id from-pile-id from-idx)
           (hf-pure workspace-id)
           (dna/move-card from-pile-id from-idx pile-id idx card)
           (<-hf)))))

(reg-event-db
  :tabber-fp-drop
  middleware
  (fn tabber-fp-drop [db [workspace-id pile-id idx source-id]]
    (console/debug ::tabber-fp-drop {:workspace-id workspace-id
                                     :pile-id pile-id
                                     :idx idx
                                     :source-id source-id})
    (if-let [[db {from-pile-id :pile-id from-idx :idx}]
             (xy/open-source-in-workspace db source-id workspace-id)]
      (let [card (workspace/get-card (get-in db [:workspaces workspace-id]) from-pile-id from-idx)]
        (first (dna/move-card db workspace-id from-pile-id from-idx pile-id idx card)))
      (hf-return db nil))))

(reg-event-db
  :activate-live-parse-view
  middleware
  (fn activate-live-parse-view [db [overlay]]
    (let [db (first (dna/activate-card-by-key db (:current-workspace db) "live-parse-view-card"))]
      (-> db
          (live-parse-view/select-overlay overlay)
          (live-parse-view/refresh)))))

(reg-event-db
  :source-hyperlink-click
  middleware
  (fn source-hyperlink-click [db [source-id]]
    (console/debug ::source-hyperlink-click {:source-id source-id})
    (if source-id
      (first (xy/open-source-in-workspace db source-id (:current-workspace db)))
      (do (console/warn ::source-hyperlink-click :null-source-id)
          db))))

(reg-event-db
  :source-hyperlink-click-and-lex
  middleware
  (fn source-hyperlink-click-and-lex [db [source-id]]
    (console/debug ::source-hyperlink-click-and-lex {:source-id source-id})
    (if source-id
      (let [[db _] (xy/open-source-in-workspace db source-id (:current-workspace db))]
        (if-let [editor-id (first (q/sources->editors db [source-id]))]
          (first (xy/run-lexer db editor-id))
          db))
      (do (console/warn ::source-hyperlink-click-and-lex :null-source-id)
          db))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modebar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(reg-event-db
 :modebar-click
 middleware
 (fn modebar-click [db [mode]]
   (console/debug ::modebar-click {:mode mode})
   (first (xy/switch-mode db mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(reg-event-db
 :clear-modal
 middleware
 (fn clear-modal [db _]
   (console/debug ::clear-modal)
   (assoc db :modal nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(reg-event-db
 :create-editor-from-scratch
 middleware
 (fn create-editor-from-scratch [db [tag]]
   (console/debug ::create-editor-from-scratch {:tag tag})
   (hf-> db
         (dna/create-editor-from-scratch tag)
         (<-hf))))

(reg-event-db
  :internal-editor-change
  middleware
  (fn internal-editor-change [db [id new-string]]
    #_(console/debug ::internal-editor-change {:editor-id id :new-string new-string})
    (let [editor (get (:editors db) id)
          buffer-id (:buffer-id editor)
          buffer (get (:buffers db) buffer-id)]
      (if (and editor buffer)
        (assoc-in db [:buffers buffer-id :string] new-string)
        db))))

(reg-event-db
  :external-editor-change
  middleware
  (fn external-editor-change [db [id new-string]]
    #_(console/debug ::external-editor-change {:editor-id id :new-string new-string})
    db))

(reg-event-db
  :editor-init-codemirror
  middleware
  (fn editor-init-codemirror [db [id cm]]
    (console/debug ::editor-init-codemirror {:editor-id id})
    (codemirror/focus cm)
    (if-let [editor (get-in db [:editors id])]
      (as-> db db
        (assoc-in db [:editors id :codemirror] cm)
        (let [history (:history editor)]
          (if-not (or (seq (:done history))
                      (seq (:undone history)))
            (update-in db [:editors id] editor/history-add {:string (editor/-string db id)
                                                            :cursor-info (codemirror/get-cursor-info cm)})
            db)))
      db)))

(reg-event-db
  :editor-history-add
  middleware
  (fn editor-history-add [db [id edit]]
    (console/debug ::editor-history-add {:editor-id id :edit edit})
    (if (get-in db [:editors id])
      (update-in db [:editors id] editor/history-add edit)
      db)))

(reg-event-db
  :editor-undo
  middleware
  (fn editor-undo [db [id]]
    (if (get-in db [:editors id])
      (update-in db [:editors id] editor/undo)
      db)))

(reg-event-db
  :editor-redo
  middleware
  (fn editor-redo [db [id]]
    (if (get-in db [:editors id])
      (update-in db [:editors id] editor/redo)
      db)))

(reg-event-db
 :editor-cursor
 production-middleware
 (fn editor-cursor [db [id cursor-info]]
   #_(console/debug ::editor-cursor {:editor-id id :cursor-info cursor-info})
   (if (get-in db [:editors id])
     (assoc-in db [:editors id :cursor-info] cursor-info)
     db)))

(reg-event-db
 :editor-save
 middleware
 (fn editor-save [db [id]]
   (console/debug ::editor-save {:editor-id id})
   (first (xy/save-and-persist-editor! db id))))

(reg-event-db
 :editor-blur
 middleware
 (fn editor-blur [db [id]]
   #_(console/debug ::editor-blur {:editor-id id})
   ;; don't do anything here, :current-editor gets set by subsequent :editor-focus event
   db))

(reg-event-db
 :editor-focus
 middleware
 (fn editor-focus [db [id]]
   #_(console/debug ::editor-focus {:editor-id id})
   (assoc db :current-editor id)))

(reg-event-db
  :editor-toggle-auto-parse
  middleware
  (fn editor-toggle-auto-parse [db [id]]
    (console/debug ::editor-toggle-auto-parse {:editor-id id})
    (first (dna/toggle-editor-auto-parse db id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sample Editor Contextbar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(reg-event-db
  :contextbar-emphasize-overlay
  middleware
  (fn contextbar-emphasize-overlay [db [editor-id overlay-type overlay-tag char-from char-to]]
    (console/debug ::contextbar-emphasize-overlay {:editor-id editor-id
                                                   :overlay-type overlay-type
                                                   :overlay-tag overlay-tag
                                                   :char-from char-from
                                                   :char-to char-to})
    (if-let [pindex (editor/polygon-index db editor-id overlay-type overlay-tag char-from char-to)]
      (-> db
          (editor/apply-all-emphasis editor-id :contextbar colors/invisible-mod)
          (editor/apply-emphasis editor-id overlay-type overlay-tag :contextbar pindex {}))
      db)))

(reg-event-db
  :contextbar-clear-all-emphasis
  middleware
  (fn contextbar-clear-all-emphasis [db [editor-id]]
    (console/debug ::contextbar-clear-all-emphasis {:editor-id editor-id})
    (editor/clear-all-emphasis db editor-id :contextbar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overlays
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(reg-event-db
 :remove-all-overlays
 middleware
 (fn remove-all-overlays [db [editor-id]]
   (if (get-in db [:editors editor-id])
     (update-in db [:editors editor-id :overlay-state]
                overlay-state/remove-all-overlays)
     db)))

;; TODO: refactor to use overlay-state/toggle-overlay instead
(reg-event-db
 :toggle-overlay
 middleware
 (fn toggle-overlay [db [editor-id overlay-type overlay-tag]]
   (console/debug ::toggle-overlay {:editor-id editor-id
                                    :overlay-type overlay-type
                                    :overlay-tag overlay-tag})
   (let [disabled-overlays (overlay-state/disabled-overlays (get-in db [:editors editor-id :overlay-state]))]
     (if (contains? disabled-overlays [overlay-type overlay-tag])
       (dispatch [:enable-overlay editor-id overlay-type overlay-tag])
       (dispatch [:disable-overlay editor-id overlay-type overlay-tag])))
   db))

(reg-event-db
  :peek-overlay
  middleware
  (fn peek-overlay [db [editor-id overlay-type overlay-tag]]
    #_(console/debug ::peek-overlay {:editor-id editor-id
                                     :overlay-type overlay-type
                                     :overlay-tag overlay-tag})
    (editor/peek-overlay db editor-id overlay-type overlay-tag)))

(reg-event-db
  :unpeek-overlay
  middleware
  (fn unpeek-overlay [db [editor-id overlay-type overlay-tag]]
    #_(console/debug ::unpeek-overlay {:editor-id editor-id
                                       :overlay-type overlay-type
                                       :overlay-tag overlay-tag})
    (editor/unpeek-overlay db editor-id overlay-type overlay-tag)))

(reg-event-db
  :disable-overlay
  middleware
  (fn disable-overlay [db [editor-id overlay-type overlay-tag]]
    #_(console/debug ::disable-overlay {:editor-id editor-id
                                        :overlay-type overlay-type
                                        :overlay-tag overlay-tag})
    (editor/disable-overlay db editor-id overlay-type overlay-tag)))

(reg-event-db
  :enable-overlay
  middleware
  (fn enable-overlay [db [editor-id overlay-type overlay-tag]]
    #_(console/debug ::enable-overlay {:editor-id editor-id
                                       :overlay-type overlay-type
                                       :overlay-tag overlay-tag})
    (editor/enable-overlay db editor-id overlay-type overlay-tag)))

(reg-event-db
  :disable-overlays-by-type
  middleware
  (fn disable-overlays-by-type [db [editor-id overlay-type]]
    (console/debug :disable-overlays-by-type {:editor-id editor-id
                                              :type overlay-type})
    (editor/disable-overlays-by-type db editor-id overlay-type)))

(reg-event-db
  :enable-overlays-by-type
  middleware
  (fn enable-overlays-by-type [db [editor-id overlay-type]]
    (console/debug :enable-overlays-by-type {:editor-id editor-id
                                             :type overlay-type})
    (editor/enable-overlays-by-type db editor-id overlay-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- progress-map
  "Return a map from each cache key to its min and max total progress"
  [db cache-keys]
  (second
    (reduce
      (fn [[n m] [k p]]
        ;; if max-progress is nil, then assume 0
        (let [p (if p p 0)]
          [(+ n p)
           (assoc m k {:min n
                       :max (+ n p)})]))
      [0 {}]
      (for [k cache-keys
            :let [[_ worker] (dna/get-worker db k)]]
        [k (worker/max-progress worker db)]))))

(reg-event-db
  :workers-step-all
  middleware
  (fn workers-step-all [db [cache-keys]]
    (dispatch [:garbage-collect-workers])
    (let [pm (progress-map db cache-keys)
          max-progress (apply max (map :max (vals pm)))]
      (console/debug ::workers-step-all (map :algo cache-keys))
      (dispatch [:-workers-step-all pm cache-keys])
      (-> db
          (assoc :progress {:current 0 :max max-progress})
          (update :running-workers into cache-keys)))))

(defn- update-worker-progress [pm worker db]
  (let [cache-key (worker/cache-key worker)
        cumulative-progress (if-let [current-progress (worker/current-progress worker db)]
                              (+ (get-in pm [cache-key :min] 0) current-progress)
                              (get-in db [:progress :current]))
        status (worker/status worker)
        description (if-let [description (worker/progress-description worker db)]
                      description
                      (get-in db [:progress :description]))]
    #_(console/debug ::update-worker-progress
                   {:algo (:algo cache-key)
                    :cumulative cumulative-progress
                    :max (get-in db [:progress :max])
                    :description description
                    :status status})
    (update db :progress merge
            {:current cumulative-progress
             :status status
             :description description})))

(reg-event-db
  :disj-running-worker
  middleware
  (fn disj-running-worker [db [cache-key]]
    (update db :running-workers disj cache-key)))

(reg-event-db
  :reset-running-workers
  middleware
  (fn reset-running-workers [db _]
    (assoc db :running-workers (:running-workers db/default-value))))

(defn- step-async-worker [pm worker db remaining-cache-keys]
  (letfn [(succ-cb [worker]
            (measurement/log-worker-status db worker)
            (dispatch [:disj-running-worker (worker/cache-key worker)])
            (dispatch [:-workers-step-all pm (next remaining-cache-keys)]))
          (fail-cb [worker]
            (measurement/log-worker-status db worker)
            (dispatch [:reset-running-workers]))]
    (let [worker (worker/start worker db succ-cb fail-cb)]
      (first (dna/add-or-update-worker db worker)))))

(defn- step-sync-worker [pm worker db remaining-cache-keys]
  (let [[db worker] (dna/step-worker db (worker/cache-key worker))
        db (update-worker-progress pm worker db)]
    (case (worker/status worker)
      :running
      (do (dispatch [:-workers-step-all pm remaining-cache-keys])
          db)
      :success
      (do (measurement/log-worker-status db worker)
          (dispatch [:-workers-step-all pm (next remaining-cache-keys)])
          (->> (update db :running-workers disj (worker/cache-key worker))
               (worker/render worker)))
      :failure
      (do (measurement/log-worker-status db worker)
          (->> (assoc db :running-workers (:running-workers db/default-value))
               (worker/render worker)))
      ;; default
      (do (console/warn ::step-sync-worker :unrecognized-status (worker/status worker))
          (update db :running-workers disj (worker/cache-key worker))))))

(reg-event-db
  :-workers-step-all
  middleware
  (fn -workers-step-all [db [pm cache-keys]]
    (if-let [cache-key (first cache-keys)]
      (let [[db worker] (dna/get-worker db cache-key)]
        (cond (satisfies? worker/IAsyncWorker worker)
              (step-async-worker pm worker db cache-keys)

              (satisfies? worker/ISyncWorker worker)
              (step-sync-worker pm worker db cache-keys)

              :else
              (do (console/warn ::-workers-step-all :unknown-protocol cache-key)
                  db)))
      (do (console/debug ::-workers-step-all :complete)
          db))))

(reg-event-db
 :async-worker-complete-success
 middleware
 (fn async-worker-complete-success [db [cache-key payload]]
   #_(console/debug ::async-worker-complete-success cache-key)
   (if-let [worker (second (dna/get-worker db cache-key))]
     (let [worker (worker/complete-success worker payload)
           db (first (dna/add-or-update-worker db worker))
           db (worker/render worker db)
           cumulative-progress (+ (get-in db [:progress :current] 0)
                                  (worker/current-progress worker db))
           status (worker/status worker)
           description (worker/progress-description worker db)]
       (console/debug ::async-worker-complete-success :progress
                      {:algo (:algo cache-key)
                       :cumulative cumulative-progress
                       :max (get-in db [:progress :max])
                       :description description
                       :status status})
       (update db :progress merge
               {:current cumulative-progress
                :status status
                :description description}))
     (do
       (console/error ::async-worker-complete-success (str "No worker with cache-key " cache-key " found"))
       db))))

(reg-event-db
 :async-worker-complete-failure
 middleware
 (fn async-worker-complete-failure [db [cache-key payload]]
   (console/debug ::async-worker-complete-failure {:cache-key cache-key
                                                   :payload payload})
   (if-let [worker (second (dna/get-worker db cache-key))]
     (let [worker (worker/complete-failure worker payload)
           db (first (dna/add-or-update-worker db worker))
           db (worker/render worker db)
           cumulative-progress (+ (get-in db [:progress :current] 0)
                                  (worker/current-progress worker db))
           status (worker/status worker)
           description (worker/progress-description worker db)]
       (console/debug ::async-worker-complete-failure :progress
                      {:algo (:algo cache-key)
                       :cumulative cumulative-progress
                       :max (get-in db [:progress :max])
                       :description description
                       :status status})
       (update db :progress merge
               {:current cumulative-progress
                :status status
                :description description}))
     (do
       (console/error ::async-worker-complete-failure (str "No worker with cache-key " cache-key " found"))
       db))))

(reg-event-db
  :garbage-collect-workers
  middleware
  (fn garbage-collect-workers [db _]
    (first (xy/garbage-collect-workers db))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Token Dashboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(reg-event-db
 :add-token-sample
 middleware
 (fn add-token-sample [db [string]]
   (console/debug ::add-token-sample {:string string})
   (first (xy/add-and-process-token-sample db string))))

(reg-event-db
 :set-token-horizon
 middleware
 (fn set-token-horizon [db [sample-id horizon]]
   (console/debug ::set-token-horizon {:sample-id sample-id :horizon horizon})
   (first (dna/set-token-horizon db sample-id horizon))))

(reg-event-db
 :set-token-package-cache
 middleware
 (fn set-token-package-cache [db [packages]]
   (console/debug ::set-token-package-cache {:count (count packages)})
   (first (dna/set-token-package-cache db packages))))

(reg-event-db
 :delete-token-sample
 middleware
 (fn delete-token-sample [db [sample-id]]
   (console/debug ::delete-token-sample {:sample-id sample-id})
   (first (dna/delete-token-sample db sample-id))))

(reg-event-db
 :add-token-category
 middleware
 (fn add-token-category [db [parent-id]]
   (console/debug ::add-token-category {:parent-id parent-id})
   (hf-> db
         (dna/add-token-category parent-id)
         (dna/token-dashboard-select)
         (<-hf))))

(reg-event-db
 :delete-token-category
 middleware
 (fn delete-token-category [db [element-id]]
   (console/debug ::delete-token-category {:element-id element-id})
   (first (dna/delete-token-category db element-id))))

(reg-event-db
 :rename-token-category
 middleware
 (fn rename-token-category [db [element-id new-name]]
   (console/debug ::rename-token-category {:element-id element-id :new-name new-name})
   (first (dna/rename-token-category db element-id new-name))))

(reg-event-db
 :token-dashboard-toggle
 middleware
 (fn token-dashboard-toggle [db [element-id]]
   (console/debug ::token-dashboard-toggle {:element-id element-id})
   (first (dna/token-dashboard-toggle db element-id))))

(reg-event-db
 :token-dashboard-move
 middleware
 (fn token-dashboard-move [db [source-element-id target-element-id idx]]
   (console/debug ::token-dashboard-move {:source-element-id source-element-id
                                          :target-element-id target-element-id
                                          :idx idx})
   (hf-> db
         (dna/token-dashboard-move source-element-id target-element-id idx)
         (hf-pure target-element-id)
         (dna/token-dashboard-uncollapse-category)
         (<-hf))))

(reg-event-db
 :token-dashboard-deselect-all
 middleware
 (fn token-dashboard-deselect-all [db _]
   (console/debug ::token-dashboard-deselect-all)
   (first (dna/token-dashboard-deselect-all db))))

(reg-event-db
 :token-dashboard-select
 middleware
 (fn token-dashboard-select [db [element-id]]
   (console/debug ::token-dashboard-select {:element-id element-id})
   (first (dna/token-dashboard-select db element-id))))

(reg-event-db
 :fetch-token-example-strings
 middleware
 (fn fetch-token-examples-strings [db [element-id path]]
   (console/debug ::fetch-token-example-strings {:element-id element-id
                                                 :path path})
   (first (dna/fetch-token-example-strings db element-id path))))

(reg-event-db
 :set-token-example-strings
 middleware
 (fn set-token-example-strings [db [element-id path strings]]
   (console/debug ::set-token-example-strings {:element-id element-id
                                               :path path
                                               :num-strings (count strings)})
   (first (dna/set-token-example-strings db element-id path strings))))

(reg-event-db
  :fetch-category-horizon
  middleware
  (fn fetch-category-horizon [db [element-id]]
    (console/debug ::fetch-category-horizon {:elemnt-id element-id})
    (first (dna/fetch-category-horizon db element-id))))

(reg-event-db
  :token-dashboard-save
  middleware
  (fn token-dashboard-save [db _]
    (console/debug ::token-dashboard-save)
    (first (xy/save-and-persist-token-dashboard! db))))

(reg-event-db
  :token-dashboard/accept
  middleware
  (fn token-dashboard-accept [db [path rule-name-str]]
    (console/debug ::token-dashboard-accept {:path path
                                             :rule-name-str rule-name-str})
    (first (xy/token-dashboard-accept db path rule-name-str))))

(reg-event-db
  :token-dashboard/delete-all-samples
  middleware
  (fn token-dashboard-clear [db _]
    (console/debug ::token-dashboard-delete-all-samples)
    (update db :token-dashboard token-dashboard/delete-all-samples)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse Dashboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(reg-event-db
  :parse-dashboard/add-sample
  middleware
  (fn parse-dashboard-add-sample [db [nt-str editor-id char-from char-to negative?]]
    (console/debug ::parse-dashboard-add-sample
                   {:nt-str nt-str
                    :editor-id editor-id
                    :char-from char-from
                    :char-to char-to
                    :negative? negative?})
    (let [[db sample-id] (xy/add-and-process-parse-sample-via-editor db nt-str editor-id char-from char-to negative?)]
      (if sample-id
        (hf-> db
              (dna/clear-editor-selection editor-id)
              (hf-pure sample-id)
              (dna/parse-dashboard-select-sample)
              (hf-pure (:current-workspace db))
              (dna/activate-card-by-key "parse-dashboard-card")
              (<-hf))
        db))))

(reg-event-db
 :parse-dashboard-select-sample
 middleware
 (fn parse-dashboard-select-sample [db [element-id]]
   (console/debug ::parse-dashboard-select-sample {:element-id element-id})
   (first (dna/parse-dashboard-select-sample db element-id))))

(reg-event-db
 :parse-dashboard-delete-sample
 middleware
 (fn parse-dashboard-delete-sample [db [sample-id]]
   (console/debug ::parse-dashboard-delete-sample {:sample-id sample-id})
   (first (xy/annihilate-parse-sample db sample-id))))

(reg-event-db
  :parse-dashboard/delete-all-samples
  middleware
  (fn parse-dashboard-delete-all-samples [db _]
    (console/debug ::parse-dashboard-delete-all-samples)
    (first (xy/annihilate-all-parse-samples db))))

(reg-event-db
  :parse-dashboard-peek-label
  middleware
  (fn parse-dashboard-peek-label [db [sample-id label-id]]
    #_(console/debug ::parse-dashboard-peek-label {:sample-id sample-id
                                                   :label-id label-id})
    (first (dna/parse-dashboard-peek-label db sample-id label-id))))

(reg-event-db
  :parse-dashboard-unpeek-all-labels
  middleware
  (fn parse-dashboard-unpeek-all-labels [db [sample-id]]
    #_(console/debug ::parse-dashboard-unpeek-all-labels {:sample-id sample-id})
    (first (dna/parse-dashboard-unpeek-all-labels db sample-id))))

(reg-event-db
  :parse-dashboard-select-label
  middleware
  (fn parse-dashboard-select-label [db [sample-id label-id]]
    (console/debug ::parse-dashboard-select-label {:sample-id sample-id
                                                   :label-id label-id})
    (first (dna/parse-dashboard-select-label db sample-id label-id))))

(reg-event-db
  :parse-dashboard-delete-label
  middleware
  (fn parse-dashboard-delete-label [db [sample-id label-id]]
    (console/debug ::parse-dashboard-delete-label {:sample-id sample-id
                                                   :label-id label-id})
    (first (xy/annihilate-parse-sample-label-by-id db sample-id label-id))))

(reg-event-db
  :parse-dashboard-move
  middleware
  (fn parse-dashboard-move [db [source-element-id target-element-id idx]]
    (console/debug ::parse-dashboard-move {:source-element-id source-element-id
                                           :target-element-id target-element-id
                                           :idx idx})
    (first (dna/parse-dashboard-move db source-element-id target-element-id idx))))

(reg-event-db
  :parse-dashboard-change-layout
  middleware
  (fn parse-dashboard-change-layout [db [layout-args]]
    (console/debug ::parse-dashboard-change-layout {:layout-args layout-args})
    (first (dna/parse-dashboard-change-layout db layout-args))))

(reg-event-db
  :parse-dashboard-clear
  middleware
  (fn parse-dashboard-clear [db _]
    (console/debug ::parse-dashboard-clear)
    (first (dna/parse-dashboard-clear-all-status db))))

(reg-event-db
  :parse-dashboard-save
  middleware
  (fn parse-dashboard-save [db _]
    (console/debug ::parse-dashboard-save)
    (first (xy/save-and-persist-parse-dashboard! db))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Live Parse View
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; OBSOLETE
(reg-event-db
  :live-parse-view/select-overlay
  middleware
  (fn live-parse-view-select-overlay [db [overlay]]
    (console/debug ::live-parse-view-select-overlay {:overlay overlay})
    (-> db
        (live-parse-view/select-overlay overlay)
        (live-parse-view/refresh))))

(reg-event-db
  :live-parse-view/reset
  middleware
  (fn live-parse-view-reset [db _]
    (live-parse-view/reset db)))

(reg-event-db
  :live-parse-view/refresh
  middleware
  (fn live-parse-view-refresh [db _]
    (console/debug ::live-parse-view-refresh)
    (-> db
        (live-parse-view/reset)
        (live-parse-view/refresh))))

(reg-event-db
 :live-parse-view/add-sample
 middleware
 (fn live-parse-view-add-sample [db [[nt-kw i l :as node] negative?]]
   (if-let [editor-id (focus-ring/last-focused-sample-editor db)]
     (if-let [tokens (second (dna/get-tokens db editor-id))]
       (let [[char-from char-to] (lexer/token-range->char-range tokens i l)
             db (first (xy/add-and-process-parse-sample-via-editor db (name (parser/->nonterminal nt-kw)) editor-id char-from char-to negative?))
             sample-id (live-parse-view/sample-for-editor db editor-id)]
         (console/debug ::live-parse-view-add-sample
                        {:editor-id editor-id
                         :sample-id sample-id
                         :node node
                         :char-from char-from
                         :char-to char-to})
         ;; update sample-id in case this is a newly created sample
         (assoc-in db [:live-parse-view :sample-id] sample-id))
       (do (console/debug ::live-parse-view-add-sample :no-tokens {:editor-id editor-id :node node})
           db))
     (do (console/error ::live-parse-view-add-sample :no-current-editor)
         db))))

(reg-event-db
  :live-parse-view/remove-sample
  middleware
  (fn live-parse-view-remove-sample [db [sample-id tokens [nt-kw i l :as node]]]
    (if (and (some? sample-id) (seq tokens))
      (if-let [[char-from char-to] (lexer/token-range->char-range tokens i l)]
        (do (console/debug ::live-parse-view-remove-sample {:sample-id sample-id :node node})
            (first (xy/annihilate-parse-sample-label db sample-id nt-kw char-from char-to)))
        (do (console/error ::live-parse-view-remove-sample :no-char-range-found-for-node node)
            db))
      (do (console/error ::live-parse-view-remove-sample :missing-required-args
                         {:sample-id sample-id :tokens tokens})
          db))))

(reg-event-db
  :live-parse-view/gen-disambiguations
  middleware
  (fn gen-disambiguations [db _]
    (console/debug ::live-parse-view-gen-disambiguations)
    (let [disambiguations (live-parse-view/synthesize-disambiguations db)]
      (-> db
          (live-parse-view/clear-candidate-id)
          (assoc-in [:live-parse-view :disambiguations] disambiguations)))))

(reg-event-db
  :live-parse-view/preview-disambiguation
  middleware
  (fn preview-disambiguation [db [candidate-id]]
    (console/debug ::live-parse-view-preview-disambiguation {:candidate-id candidate-id})
    (-> db
        (live-parse-view/gen-disambiguation-preview candidate-id)
        (live-parse-view/set-candidate-id candidate-id))))

(reg-event-db
  :live-parse-view/accept-disambiguation
  middleware
  (fn live-parse-view-accept-disambiguation [db [candidate-id]]
    (console/debug ::live-parse-view-accept-disambiguation {:candidate-id candidate-id})
    (hf-> db
          (xy/live-parse-view-accept-disambiguation candidate-id)
          (xy/annihilate-all-parse-samples)
          (<-hf))))

(reg-event-db
  :solver/add-sample
  middleware
  (fn solver-add-sample [db [sample-id tokens [nt-kw i l :as node] negative?]]
    (let [[char-from char-to] (lexer/token-range->char-range tokens i l)]
      (console/debug ::solver/add-sample {:sample-id sample-id
                                                       :node node
                                                       :char-range [char-from char-to]})
      (first (xy/add-and-process-parse-sample-label db sample-id nt-kw char-from char-to negative?)))))
(reg-event-db
  :solver/remove-sample
  middleware
  (fn solver-remove-sample [db [sample-id tokens [nt-kw i l :as node]]]
    (console/debug ::solver/remove-sample {:sample-id sample-id :node node})
    (if-let [[char-from char-to] (lexer/token-range->char-range tokens i l)]
      (first (xy/annihilate-parse-sample-label db sample-id nt-kw char-from char-to))
      (do (console/error ::solver/remove-sample :no-char-range-found-for-node node)
          db))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(reg-event-db
  :solver/set-rhs-choice
  middleware
  (fn set-rhs-choice [db [solution-idx candidate-idx choice-idx choice]]
    (console/debug ::set-rhs-choice {:solution-idx solution-idx
                                     :candidate-idx candidate-idx
                                     :choice-idx choice-idx
                                     :choice choice})
    (-> db
        (update :solver solver/set-rhs-choice solution-idx candidate-idx choice-idx choice)
        (update :solver solver/gen-previews)
        (update :solver solver/clear-disambiguations))))

(reg-event-db
  :solver/toggle-candidate
  middleware
  (fn toggle-candidate [db [solution-idx candidate-idx]]
    (console/debug ::toggle-candidate {:solution-idx solution-idx
                                       :candidate-idx candidate-idx})
    (-> db
        (update :solver solver/toggle-candidate solution-idx candidate-idx)
        (update :solver solver/gen-previews)
        (update :solver solver/clear-disambiguations))))

(reg-event-db
  :solver/gen-disambiguations
  middleware
  (fn gen-disambiguations [db _]
    (console/debug ::gen-disambiguations)
    (update db :solver solver/synthesize-disambiguations (:parse-dashboard db))))

(reg-event-db
  :solver/preview-disambiguation
  middleware
  (fn preview-disambiguation [db [candidate-id]]
    (-> db
        (update :solver solver/gen-disambiguation-previews candidate-id)
        (update :solver solver/select-disambiguation candidate-id))))

(reg-event-db
  :solver/peek-overlay
  middleware
  (fn peek-overlay [db [type tag]]
    (update db :solver solver/peek-overlay type tag)))

(reg-event-db
  :solver/unpeek-overlay
  middleware
  (fn unpeek-overlay [db [type tag]]
    (update db :solver solver/unpeek-overlay type tag)))

(reg-event-db
  :solver/disable-overlay
  middleware
  (fn disable-overlay [db [type tag]]
    (update db :solver solver/disable-overlay type tag)))

(reg-event-db
  :solver/enable-overlay
  middleware
  (fn enable-overlay [db [type tag]]
    (update db :solver solver/enable-overlay type tag)))

(reg-event-db
  :solver/toggle-overlay
  middleware
  (fn toggle-overlay [db [type tag]]
    (update db :solver solver/toggle-overlay type tag)))

(reg-event-db
  :solver/disable-overlays-by-type
  middleware
  (fn solver-disable-overlays-by-type [db [type]]
    (console/debug ::solver-disable-overlays-by-type {:type type})
    (update db :solver solver/disable-overlays-by-type type)))

(reg-event-db
  :solver/enable-overlays-by-type
  middleware
  (fn solver-enable-overlays-by-type [db [type]]
    (console/debug ::solver-enable-overlays-by-type {:type type})
    (update db :solver solver/enable-overlays-by-type type)))

(reg-event-db
  :solver/accept-heuristic
  middleware
  (fn accept-heuristic [db [heuristic]]
    (console/debug ::solver-accept-heuristic {:heuristic heuristic})
    (-> db
        (update :solver solver/accept-heuristic heuristic)
        (xy/run-solver-continuation)
        (first))))

(reg-event-db
  :solver/reject-heuristic
  middleware
  (fn reject-heuristic [{:keys [solver] :as db} [heuristic]]
    (console/debug ::solver-reject-heuristic {:heuristic heuristic})
    (let [db (update db :solver solver/reject-heuristic heuristic)]
      (if (> (count (get-in solver [:ux :heuristics])) 1)
        (update-in db [:solver :ux :heuristics] next)
        (first (xy/run-solver-continuation db))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Log
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(reg-event-db
  :log/reset
  middleware
  (fn log-reset [db _]
    (update db :log log/reset)))

(reg-event-db
  :log/info
  middleware
  (fn log-info [db [value]]
    (update db :log log/info value)))

(reg-event-db
  :log/warn
  middleware
  (fn log-warn [db [value]]
    (update db :log log/warn value)))

(reg-event-db
  :log/error
  middleware
  (fn log-error [db [value]]
    (update db :log log/error value)))

(reg-event-db
  :log/success
  middleware
  (fn log-success [db [value]]
    (update db :log log/success value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connection Status
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(reg-event-db
  :comm/set-open-flag
  middleware
  (fn comm-set-open-flag [db [flag]]
    (console/debug ::comm-set-open-flag {:flag flag})
    (assoc-in db [:comm :open?] flag)))

(reg-event-db
  :comm/register-in-flight
  middleware
  (fn comm-register-in-flight [db [registry-id request-id payload]]
    #_(console/debug ::comm-register-in-flight
                     {:registry-id registry-id
                      :request-id request-id
                      :payload payload})
    (update db :comm comm/register-in-flight registry-id request-id payload)))

(reg-event-db
  :comm/complete-in-flight
  middleware
  (fn comm-complete-in-flight [db [registry-id request-id edn-reply status-kw]]
    #_(console/debug ::comm-complete-in-flight
                     {:registry-id registry-id
                      :request-id request-id
                      :edn-reply edn-reply
                      :status-kw status-kw})
    (update db :comm comm/complete-in-flight registry-id request-id edn-reply status-kw)))

(reg-event-db
  :comm/show-details
  middleware
  (fn comm-show-details [db _]
    (let [db (first (dna/show-modal db (modal/comm-status-modal)))]
      (assoc-in db [:comm :needs-attention?] false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Classroom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(reg-event-db
  :classroom/pause
  middleware
  (fn classroom-pause [db _]
    (console/debug ::classroom-pause)
    (first (dna/show-modal db (classroom.ui/pause-modal)))))

(reg-event-db
  :classroom/start
  middleware
  (fn classroom-start [db _]
    (console/debug ::classroom-start)
    db))

(reg-event-db
  :classroom/maybe-finish
  middleware
  (fn classroom-maybe-finish [db _]
    (console/debug ::classroom-maybe-finish)
    (let [lesson-plan (classroom.api/get-lesson-plan db)]
      (if-let [source-path (last lesson-plan)]
        (let [source-id (:id (first (source/sources-with-path db source-path)))
              status (classroom.api/answer-status db source-id)]
          (if-not (:pass? status)
            ;; final test failing
            (first (dna/show-modal db (classroom.ui/cannot-finish-modal source-path)))
            (do (dispatch [:classroom/finish])
                db)))
        (do (dispatch [:classroom/finish])
            db)))))

(reg-event-db
  :classroom/finish
  middleware
  (fn classroom-finish [db _]
    (console/debug ::classroom-finish)
    (if-let [project-id (:current-project db)]
      (let [project-name (get-in db [:projects project-id :description])
            project-name (if (seq project-name)
                           project-name
                           "<unnamed>")]
        (-> db
            (dna/show-modal (classroom.ui/finish-modal project-name))
            (first)))
      (do (console/warn ::classroom-finish :cannot-finish :not-in-project)
          db))))

(reg-event-db
  :classroom/request-assistance
  middleware
  (fn classroom-request-assistance [db _]
    (console/debug ::classroom-request-assistance)
    (.play (js/Audio. "assets/chime.mp3"))
    db))

(reg-event-db
  :classroom/force-pass
  middleware
  (fn classroom-force-pass [db [source-path]]
    (classroom.api/force-pass db source-path)))

;; ) ;; end trace-forms
