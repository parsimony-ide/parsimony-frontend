(ns parsimony.subs
  (:require [parsimony.commands :as commands]
            [parsimony.models.editor :as editor]
            [parsimony.models.focus-ring :as focus-ring]
            [parsimony.models.overlay-state :as overlay-state]
            [parsimony.models.parse-dashboard :as parse-dashboard]
            [parsimony.models.project :as project]
            [parsimony.models.solver :as solver]
            [parsimony.models.source :as source]
            [parsimony.models.token-dashboard :as token-dashboard]
            [parsimony.query :as q]
            [parsimony.util :refer [find-first overlay-seq]]
            [parsimony.views.cards :as cards]
            [parsimony.views.tree-view :as tree-view]
            [parsimony.views.workspace :as workspace]
            [parsimony.workers.parser :as parser]
            [re-com.util :refer [deref-or-value]]
            [re-frame.core :refer [reg-sub-raw subscribe]]
            [reagent.ratom :refer [make-reaction] :refer-macros [reaction run!]]
            #_[clairvoyant.core :refer-macros [trace-forms]]
            #_[re-frame-tracer.core :refer [tracer]]))

;; (trace-forms {:tracer (tracer :color "brown")}

(reg-sub-raw
 :buffers
 (fn [db _]
   (let [-buffers (reaction (:buffers @db))]
     (make-reaction
      (fn buffers [] @-buffers)))))

(reg-sub-raw
 :editors
 (fn [db _]
   (let [-editors (reaction (:editors @db))]
     (make-reaction
      (fn editors [] @-editors)))))

(reg-sub-raw
 :sources
 (fn [db _]
   (let [-sources (reaction (:sources @db))]
     (make-reaction
      (fn sources [] @-sources)))))

(reg-sub-raw
  :project-with-id
  (fn [db [_ id]]
    (let [project (reaction (get (:projects @db) (deref-or-value id)))]
      (make-reaction
        (fn project-with-id [] @project)))))

(reg-sub-raw
  :project-description
  (fn [db [_ id]]
    (let [project (reaction (get (:projects @db) (deref-or-value id)))
          description (reaction (:description @project))]
      (make-reaction
        (fn project-description [] @description)))))

(reg-sub-raw
  :project-picker
  (fn [db _]
    (let [state (reaction (:project-picker @db))]
      (make-reaction
        (fn project-picker [] @state)))))

(reg-sub-raw
 :file-picker-state
 (fn [db _]
   (let [state (reaction (:file-picker-state @db))]
     (make-reaction
      (fn file-picker-state [] @state)))))

(reg-sub-raw
 :workspaces
 (fn [db [_]]
   (let [-workspaces (reaction (:workspaces @db))]
     (make-reaction
      (fn workspaces [] @-workspaces)))))

(reg-sub-raw
 :workspace-with-id
 (fn [db [_ id]]
   (let [workspace (reaction (get (:workspaces @db) (deref-or-value id)))]
     (make-reaction
      (fn workspace-with-id [] @workspace)))))

(reg-sub-raw
  :active-card-keys
  (fn [db [_]]
    (let [workspaces (reaction (:workspaces @db))
          workspace-id (reaction (:current-workspace @db))
          current-workspace (reaction (get @workspaces @workspace-id))
          active-cards (reaction (workspace/active-cards @current-workspace))
          -active-card-keys (reaction (into (sorted-set)
                                            (map workspace/card-key)
                                            @active-cards))]
      (make-reaction
        (fn active-card-keys []
          @-active-card-keys)))))

(reg-sub-raw
  :current-card
  (fn [db [_]]
    (let [-current-card (reaction (:current-card @db))]
      (make-reaction
        (fn current-card []
          @-current-card)))))

(reg-sub-raw
 :editor-with-id
 (fn [db [_ id]]
   (let [editor (reaction (get (:editors @db) (deref-or-value id)))]
     (make-reaction
      (fn editor-with-id [] @editor)))))

(reg-sub-raw
 :editor-with-tag
 (fn [db [_ tag]]
   (let [editor (find-first #(= (deref-or-value tag) (:tag %)) (vals (:editors @db)))]
     (make-reaction
      (fn editor-with-tag [] @editor)))))

(reg-sub-raw
 :buffer-for-editor
 (fn [db [_ id]]
   (let [editor (reaction (get (:editors @db) (deref-or-value id)))
         buffer (reaction (get (:buffers @db) (:buffer-id @editor)))]
     (make-reaction (fn buffer-for-editor [] @buffer)))))

(reg-sub-raw
 :source-for-editor
 (fn [db [_ id]]
   (let [editor (reaction (get (:editors @db) (deref-or-value id)))
         buffer (reaction (get (:buffers @db) (:buffer-id @editor)))
         source (reaction (get (:sources @db) (:source-id @buffer)))]
     (make-reaction (fn source-for-editor [] @source)))))

(reg-sub-raw
 :modal
 (fn [db [_]]
   (let [-modal (reaction (:modal @db))]
     (make-reaction (fn modal [] @-modal)))))

(reg-sub-raw
 :editor-modified?
 (fn [db [_ id]]
   (let [editor (reaction (get (:editors @db) (deref-or-value id)))
         buffer (reaction (get (:buffers @db) (:buffer-id @editor)))
         source (reaction (get (:sources @db) (:source-id @buffer)))
         buffer-string (reaction (:string @buffer))
         source-string (reaction (:string @source))
         modified? (reaction (not (identical? @buffer-string @source-string)))]
     (make-reaction (fn editor-modified? [] @modified?)))))

(reg-sub-raw
  :editor-auto-parse
  (fn [db [_ id]]
    (let [editor (reaction (get (:editors @db) (deref-or-value id)))
          auto-parse (reaction (:auto-parse @editor))]
      (make-reaction (fn editor-auto-parse? [] @auto-parse)))))

(reg-sub-raw
 :current-editor
 (fn [db [_]]
   (let [editor (reaction (:current-editor @db))]
     (make-reaction (fn current-editor [] @editor)))))

(reg-sub-raw
 :current-editor-selection
 (fn [db [_]]
   (let [editor (reaction (get (:editors @db) (:current-editor @db)))
         buffer (reaction (get (:buffers @db) (:buffer-id @editor)))
         string (reaction (:string @buffer))
         cursor-info (reaction (:cursor-info @editor))
         selection (reaction (when-let [{:keys [selection? anchor head]} @cursor-info]
                               (when (and (some? @string)
                                          selection?)
                                 (let [range (into [] (sort [(:index anchor) (:index head)]))]
                                   {:char-range range
                                    :string (apply subs @string range)}))))]
     (make-reaction (fn current-editor-selection [] @selection)))))

(reg-sub-raw
  :last-focused-sample-editor
  (fn [db [_]]
    (let [focus-ring (reaction (:focus-ring @db))
          workspaces (reaction (:workspaces @db))
          workspace-id (reaction (:current-workspace @db))
          current-workspace (reaction (get @workspaces @workspace-id))
          editor-ids (reaction (focus-ring/previously-focused-editors @focus-ring @current-workspace))
          editors (reaction (:editors @db))
          buffers (reaction (:buffers @db))
          sources (reaction (:sources @db))
          is-sample-editor
          (fn [id]
            (let [editor (get @editors id)
                  buffer (get @buffers (:buffer-id editor))
                  source (get @sources (:source-id buffer))]
              (and (some? source)
                   (not (#{:token :grammar} (:source-type source))))))
          sample-editor-ids (reaction (into []
                                            (filter is-sample-editor)
                                            @editor-ids))
          last-sample-editor-id (reaction (peek @sample-editor-ids))]
      (make-reaction (fn last-focused-sample-editor []
                       @last-sample-editor-id)))))

(reg-sub-raw
 :decorations
 (fn [db [_]]
   (let [-decorations (reaction (get-in @db [:preferences :decorations]))]
     (make-reaction (fn decorations [] @-decorations)))))

(reg-sub-raw
 :current-mode
 (fn [db [_]]
   (let [mode (reaction (:current-mode @db))]
     (make-reaction (fn current-mode [] @mode)))))

(reg-sub-raw
  :current-project
  (fn [db [_]]
    (let [project-id (reaction (:current-project @db))]
      (make-reaction (fn current-project [] @project-id)))))

(reg-sub-raw
  :in-project?
  (fn [db _]
    (let [project-id (subscribe [:current-project])
          result (reaction (some? @project-id))]
      (make-reaction (fn in-project? [] @result)))))

(reg-sub-raw
 :token-dashboard
 (fn [db [_]]
   (let [dashboard (reaction (:token-dashboard @db))]
     (make-reaction (fn token-dashboard [] @dashboard)))))

(reg-sub-raw
  :token-dashboard-modified?
  (fn [db [_]]
    (let [live-dashboard (subscribe [:token-dashboard])
          live-pristine-dashboard (reaction (token-dashboard/pristine @live-dashboard))
          live-pristine-samples (reaction (:samples @live-pristine-dashboard))
          project-id (reaction (:current-project @db))
          project-dashboard (reaction (get-in @db [:projects @project-id :token-dashboard]))
          project-pristine-dashboard (reaction (token-dashboard/pristine @project-dashboard))
          project-pristine-samples (reaction (:samples @project-pristine-dashboard))
          modified? (reaction (not= @live-pristine-samples @project-pristine-samples))]
      (make-reaction (fn token-dashboard-modified? []
                       (and (some? @project-id)
                            @modified?))))))

(reg-sub-raw
 :token-package-at
 (fn [db [_ path]]
   (let [package (reaction (get-in @db (into [:token-dashboard :package-cache] path)))]
     (make-reaction (fn token-package-at [] @package)))))

;; XXX: spurious warning gets thrown that no element-id exists right after the element has been removed
(reg-sub-raw
 :example-strings
 (fn [db [_ id path]]
   (let [samples (reaction (get-in @db [:token-dashboard :samples]))
         example-strings (reaction (tree-view/get-element-attr @samples id [:data :example-strings]))
         res (reaction (get @example-strings path))]
     (make-reaction (fn example-strings [] @res)))))

(reg-sub-raw
  :cursor-info
  (fn [db _ [editor-id]]
    (let [editor (reaction (get (:editors @db) editor-id))
          res (reaction (:cursor-info @editor))]
      (make-reaction (fn cursor-info [] @res)))))

(reg-sub-raw
 :overlays-at-cursor
 (fn [db [_] [editor-id]]
   ;; the same functionality as editor/overlays-at-cursor, but made more efficient by chaining reactions
   (let [editor (reaction (get (:editors @db) editor-id))
         buffer (reaction (get (:buffers @db) (:buffer-id @editor)))
         string (reaction (:string @buffer))
         cursor-info (reaction (:cursor-info @editor))
         res (reaction (when-let [{:keys [head]} @cursor-info]
                         (vec (for [{[start end] :char-range :as ol} (editor/overlays-at-index @db editor-id (:index head))]
                                (assoc ol :string (subs @string start end))))))]
     (make-reaction (fn overlays-at-cursor [] @res)))))

(reg-sub-raw
 :tokens
 (fn [db [_] [editor-id]]
   ;; XXX: use of query causing the DB to refresh on every update to db, not great for performance
   (let [token-editor-id (reaction (first (q/token-editors @db)))
         cfg-editor-id (reaction (first (q/cfg-editors @db)))
         worker (reaction
                 (get-in @db [:workers (parser/->cache-key @token-editor-id
                                                           @cfg-editor-id
                                                           editor-id)]))
         res (reaction (:target-tokens @worker))]
     (make-reaction (fn tokens [] @res)))))

(reg-sub-raw
  :live-parse-view
  (fn [db _]
    (let [state (reaction (:live-parse-view @db))]
      (make-reaction (fn live-parse-view [] @state)))))

(reg-sub-raw
  :live-parse-view/active
  (fn [db _]
    (let [active-card-keys (subscribe [:active-card-keys])
          active (make-reaction (fn [] (contains? @active-card-keys "live-parse-view-card")))]
      (make-reaction (fn live-parse-view-active [] @active)))))

(reg-sub-raw
  :editor-decoration-map
  (fn [db [_] [editor-id]]
    (let [editor (reaction (get (:editors @db) editor-id))
          ;; we split the following into two chained reactions so that changes to editor state that are irrelevant to
          ;; overlays do not trigger a subscription update
          overlay-state (reaction (:overlay-state @editor))
          overlays (reaction (overlay-state/all-overlays @overlay-state))
          overlay-seq (reaction (overlay-seq @overlays))
          res (reaction
                (reduce (fn [acc {:keys [decoration-index type tag]}]
                          (update acc type (fnil merge {}) {tag decoration-index}))
                        {}
                        @overlay-seq))]
      (make-reaction (fn editor-decoration-map [] @res)))))

(reg-sub-raw
  :editor-decoration-map-static
  (fn [db [_ editor-id]]
    (let [editor (reaction (get (:editors @db) editor-id))
          ;; we split the following into two chained reactions so that changes to editor state that are irrelevant to
          ;; overlays do not trigger a subscription update
          overlay-state (reaction (:overlay-state @editor))
          overlays (reaction (overlay-state/all-overlays @overlay-state))
          overlay-seq (reaction (overlay-seq @overlays))
          res (reaction
                (reduce (fn [acc {:keys [decoration-index type tag]}]
                          (update acc type (fnil merge {}) {tag decoration-index}))
                        {}
                        @overlay-seq))]
      (make-reaction (fn editor-decoration-map-static [] @res)))))

(reg-sub-raw
  :affinity-decoration-map
  (fn [db [_]]
    (let [decoration-affinities (reaction (:decoration-affinities @db))
          res (reaction {:parse @decoration-affinities})]
      (make-reaction (fn affinity-decoration-map [] @res)))))

(reg-sub-raw
 :disabled-overlays
 (fn [db [_] [editor-id]]
   (let [editor (reaction (get (:editors @db) editor-id))
         overlay-state (reaction (:overlay-state @editor))
         res (reaction (overlay-state/disabled-overlays @overlay-state))]
     (make-reaction (fn disabled-overlays [] @res)))))

(reg-sub-raw
 :peek
 (fn [db [_] [editor-id]]
   (let [editor (reaction (get (:editors @db) editor-id))
         overlay-state (reaction (:overlay-state @editor))
         res (reaction (overlay-state/all-peek @overlay-state))]
     (make-reaction (fn peek [] @res)))))

(reg-sub-raw
 :progress
 (fn [db [_]]
   (let [res (reaction (:progress @db))]
     (make-reaction (fn progress [] @res)))))

(reg-sub-raw
 :parse-dashboard
 (fn [db [_]]
   (let [dashboard (reaction (:parse-dashboard @db))]
     (make-reaction (fn parse-dashboard [] @dashboard)))))

(reg-sub-raw
  :parse-dashboard-modified?
  (fn [db [_]]
    (let [live-dashboard (subscribe [:parse-dashboard])
          live-pristine-dashboard (reaction (parse-dashboard/pristine @live-dashboard))
          live-pristine-samples (reaction (:samples @live-pristine-dashboard))
          project-id (reaction (:current-project @db))
          project-dashboard (reaction (get-in @db [:projects @project-id :parse-dashboard]))
          project-pristine-dashboard (reaction (parse-dashboard/pristine @project-dashboard))
          project-pristine-samples (reaction (:samples @project-pristine-dashboard))
          modified? (reaction (not= @live-pristine-samples @project-pristine-samples))]
      (make-reaction (fn parse-dashboard-modified? []
                       (and (some? @project-id)
                            @modified?))))))

(reg-sub-raw
 :source-path
 (fn [db [_ source-id]]
   (let [source (reaction (get-in @db [:sources source-id]))
         path (reaction (source/source-path @source))]
     (make-reaction (fn source-path [] @path)))))

(reg-sub-raw
  :editor-for-sample
  (fn [db [_ sample-id]]
    (let [editor-id (reaction
                      (parse-dashboard/get-underlying-sample-editor-id (:parse-dashboard @db) sample-id))]
      (make-reaction (fn editor-for-sample [] @editor-id)))))

(reg-sub-raw
  :sample-for-editor
  (fn [db [_] [editor-id]]
    (let [buffer (subscribe [:buffer-for-editor editor-id])
          parse-dashboard (subscribe [:parse-dashboard])
          samples (reaction (parse-dashboard/all-samples @parse-dashboard))
          matching-samples (reaction
                             (into []
                                   (comp (filter #(= (:source-id %) (:source-id @buffer)))
                                         (filter #(= (:string %) (:string @buffer))))
                                   @samples))
          result (reaction (:sample-id (first @matching-samples)))]
      (make-reaction (fn sample-for-editor [] @result)))))

(reg-sub-raw
  :solver
  (fn [db _]
    (let [state (reaction (:solver @db))]
      (make-reaction (fn solver [] @state)))))

(reg-sub-raw
  :solver/ux-state
  (fn [db _]
    (let [state (subscribe [:solver])
          ux-state (reaction (:ux @state))]
      (make-reaction (fn solver-ux-state [] @ux-state)))))

(reg-sub-raw
  :solver/solution-exists?
  (fn [db _]
    (let [state (subscribe [:solver])
          result (reaction (solver/solution-exists? @state))]
      (make-reaction (fn solver-solution-exists? [] @result)))))

(reg-sub-raw
  :solver/overlay-state
  (fn [db _]
    (let [ux-state (subscribe [:solver/ux-state])
          result (reaction (:overlay-state @ux-state))]
      (make-reaction (fn solver-overlay-state []
                       @result)))))

(reg-sub-raw
  :solver/disabled-overlays
  (fn [db _]
    (let [overlay-state (subscribe [:solver/overlay-state])
          result (reaction (overlay-state/disabled-overlays @overlay-state))]
      (make-reaction (fn solver-disabled-overlays []
                       @result)))))

(reg-sub-raw
  :solver/peek
  (fn [db _]
    (let [overlay-state (subscribe [:solver/overlay-state])
          result (reaction (overlay-state/all-peek @overlay-state))]
      (make-reaction (fn solver-peek []
                       @result)))))

(reg-sub-raw
  :log
  (fn [db _]
    (let [result (reaction (:log @db))]
      (make-reaction (fn log []
                       @result)))))

(reg-sub-raw
  :log/entries
  (fn [db _]
    (let [log (subscribe [:log])
          result (reaction (:entries @log))]
      (make-reaction (fn log-entries []
                       @result)))))

(reg-sub-raw
  :last-command
  (fn [db [_]]
    (let [lc (reaction (:last-command @db))]
      (make-reaction (fn last-command [] @lc)))))

(reg-sub-raw
  :command-enabled?
  (fn [db [_ command-kw command-arg]]
    (let [enabled? (reaction (commands/enabled? @db command-kw command-arg))]
      (make-reaction (fn command-enabled? []
                       @enabled?)))))

(reg-sub-raw
  :db-modified?
  (fn [db [_]]
    (let [modified? (reaction (project/db-modified? @db))]
      (make-reaction (fn db-modified? []
                       @modified?)))))

(reg-sub-raw
  :comm
  (fn [db _]
    (let [state (reaction (:comm @db))]
      (make-reaction (fn comm []
                       @state)))))

;; ) ;; end trace-forms

#_(reg-sub-raw
 :editor-1
 (fn [db _]
   (reaction (find-first #(= "editor-1" (:tag %)) (vals (:editors @db))))))
