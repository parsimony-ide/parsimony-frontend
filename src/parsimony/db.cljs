(ns parsimony.db
  (:require [goog.events :as events]
            [parsimony.classroom :as classroom]
            [parsimony.comm :as comm]
            [parsimony.commands :as commands]
            [parsimony.models.buffer :as buffer]
            [parsimony.models.colors :as colors]
            [parsimony.models.editor :as editor]
            [parsimony.models.focus-ring :as focus-ring]
            [parsimony.models.live-parse-view :as live-parse-view]
            [parsimony.models.log :as log]
            [parsimony.models.parse-dashboard :as parse-dashboard]
            [parsimony.models.source :as source]
            [parsimony.models.preferences :as preferences]
            [parsimony.models.project :as project]
            [parsimony.models.project-picker :as project-picker]
            [parsimony.models.solver :as solver]
            [parsimony.models.token-dashboard :as token-dashboard]
            [parsimony.views.tree-view :as tree-view]
            [parsimony.views.file-picker :as file-picker]
            [parsimony.views.modal :as modal]
            [parsimony.views.workspace :as workspace]
            [parsimony.worker :as worker]
            [parsimony.workers.common :as workers.common]
            [re-com.validate :refer [string-or-hiccup?]]
            [schema.core :as s :include-macros true]))

(def schema {:buffers {s/Num buffer/schema}
             :editors {s/Num editor/schema}
             :workers {s/Any (s/protocol worker/IWorker)}
             :sources {s/Num source/schema}
             :projects {s/Uuid project/schema}
             :project-picker project-picker/schema
             :file-picker-state tree-view/flat-model-schema ;; XXX: rename this to just :file-picker
             :token-dashboard token-dashboard/schema
             :parse-dashboard parse-dashboard/schema
             :solver solver/schema
             :live-parse-view live-parse-view/schema
             :modal (s/maybe (s/protocol modal/IModal))
             :log log/schema
             :workspaces {s/Num workspace/schema}
             :current-workspace (s/maybe s/Num) ;; the currently activated workspace
             :current-editor (s/maybe s/Num) ;; the most recently focused editor
             :current-card (s/maybe s/Str) ;; the currently focused card key
             :current-mode (s/maybe s/Keyword)
             :current-project (s/maybe s/Uuid) ;; the currently activated project
             :focus-ring focus-ring/schema
             :preferences preferences/schema
             :decoration-affinities colors/decoration-affinities-schema
             :last-command commands/last-command-schema
             :progress (s/maybe {:current s/Num
                                 :max s/Num
                                 (s/optional-key :status) (:-status workers.common/schema)
                                 (s/optional-key :description) (s/pred string-or-hiccup?)})
             :comm comm/schema
             :running-workers #{(s/pred :algo)} ;; set of currently running workers
             :classroom classroom/schema})

(def default-value
  {:buffers (sorted-map)
   :editors (sorted-map)
   :workers (hash-map) ;; must be hash-map because keys are not necessarily comparable (but are expected to be hashable)
   :sources (sorted-map)
   :projects (sorted-map)
   :project-picker project-picker/default-model ;; hierarchical view of projects
   :file-picker-state file-picker/default-model ;; hierarchical view of sources
   :token-dashboard token-dashboard/default-model ;; hierarchical view of samples
   :parse-dashboard parse-dashboard/default-model
   :solver solver/default-model
   :live-parse-view live-parse-view/default-model
   :modal nil ;; an instance of parsimony.views.modal/IModal, or nil if hidden
   :log log/default-model
   :workspaces (sorted-map)
   :current-workspace nil
   :current-card nil
   :current-editor nil
   :current-mode nil
   :current-project nil
   :focus-ring focus-ring/default-model
   :preferences preferences/default-value
   :decoration-affinities (sorted-map)
   :last-command commands/last-command-default-value
   :progress nil
   :comm comm/default-model
   :running-workers #{}
   :classroom classroom/default-model})
