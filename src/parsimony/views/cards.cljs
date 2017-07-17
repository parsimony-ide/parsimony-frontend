(ns parsimony.views.cards
  (:require [parsimony.models.overlay-state :as overlay-state]
            [parsimony.models.project :as project]
            [parsimony.models.source :as source]
            [parsimony.views.common :refer [icon]]
            [parsimony.views.editor :refer [editor]]
            [parsimony.views.project-picker :refer [project-picker]]
            [parsimony.views.file-picker :refer [file-picker]]
            [parsimony.views.overlay-picker :refer [overlay-picker]]
            [parsimony.views.live-parse-view :refer [live-parse-view]]
            [parsimony.views.parse-dashboard :refer [parse-dashboard]]
            [parsimony.views.token-dashboard :refer [token-dashboard]]
            [parsimony.views.solver :refer [solver-view]]
            [parsimony.views.disambiguation :refer [disambiguation-view]]
            [parsimony.views.workspace :refer [ICard card-key]]
            [parsimony.views.log :refer [log-view]]
            [re-com.core :refer [h-box label md-icon-button]]
            [re-frame.core :refer [dispatch subscribe]]
            [reagent.ratom :refer-macros [reaction]]))

(defn save-button [on-click]
  [md-icon-button
   :md-icon-name "zmdi-save"
   :tooltip "Save"
   :tooltip-position :below-center
   :size :smaller
   :style {:margin-left "5px"}
   :on-click on-click])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EditorCard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IEditorLink
  (backing-editor-id [this] "Return the backing editor-id of this card"))

(defrecord EditorCard [editor-id]
  IEditorLink
  (backing-editor-id [this] (:editor-id this))
  ICard
  (card-key [this]
    (str "editor-card-" (:editor-id this)))
  (human-label [this]
    (let [source (subscribe [:source-for-editor editor-id])
          editor (subscribe [:editor-with-id editor-id])
          editor-modified? (subscribe [:editor-modified? editor-id])]
      #_(console/debug "human-label source =" (pr-str @source))
      (fn [this]
        (let [error? (overlay-state/has-error? (:overlay-state @editor))]
          [h-box
           :align :center
           :class (case (:source-type @source)
                    :token "token-file"
                    :grammar "grammar-file"
                    "")
           :children [[label :label (source/source-path @source)]
                      (when @editor-modified?
                        [save-button #(dispatch [:editor-save editor-id])])
                      (when error?
                        [icon {:md-icon-name "error-icon zmdi-alert-polygon"
                               :tooltip "This file contains an error"
                               :tooltip-position :below-center
                               :style {:margin-left "5px"}}])]]))))
  (draw-card [this]
    #_(console/debug "draw-card" editor-id)
    ;; XXX: This key is necessary to force React to unmount the old editor and remount a new editor.
    ;;
    ;; Without a key, React will simply rerender the old editor.
    ;; Since editors close over subscriptions based on id, the editor will render based on a new id, but with subscriptions
    ;; based on an old id!
    ^{:key (card-key this)}
    [editor editor-id])
  (confirm-close? [this]
    ;; XXX: kind of dirty to use a subscription here since this isn't a Reagent component
    (deref (subscribe [:editor-modified? editor-id])))
  (closeable? [_]
    true)
  (focusable? [_]
    true)
  (disabled? [_]
    false))

(cljs.reader/register-tag-parser! "parsimony.views.cards.EditorCard" map->EditorCard)

(defn editor-card [editor-id]
  (EditorCard. editor-id))

(defn editor-link? [x]
  (satisfies? IEditorLink x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ProjectPickerCard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord ProjectPickerCard []
  ICard
  (card-key [_]
    "project-picker-card")
  (human-label [_]
    [label :label "Projects"])
  (draw-card [_]
    [project-picker])
  (confirm-close? [_]
    false)
  (closeable? [_]
    false)
  (focusable? [_]
    true)
  (disabled? [_]
    false))

(cljs.reader/register-tag-parser! "parsimony.views.cards.ProjectPickerCard" map->ProjectPickerCard)

(defn project-picker-card []
  (ProjectPickerCard.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FilePickerCard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord FilePickerCard []
  ICard
  (card-key [_]
    "file-picker-card")
  (human-label [_]
    [label :label "Files"])
  (draw-card [_]
    [file-picker])
  (confirm-close? [_]
    false)
  (closeable? [_]
    false)
  (focusable? [_]
    true)
  (disabled? [_]
    (not (project/in-project?))))

(cljs.reader/register-tag-parser! "parsimony.views.cards.FilePickerCard" map->FilePickerCard)

(defn file-picker-card []
  (FilePickerCard.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OverlayPickerCard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord OverlayPickerCard []
  ICard
  (card-key [_]
    "overlay-picker-card")
  (human-label [_]
    [label :label "Legend"])
  (draw-card [_]
    [overlay-picker])
  (confirm-close? [_]
    false)
  (closeable? [_]
    false)
  (focusable? [_]
    false)
  (disabled? [_]
    (not (project/in-project?))))

(cljs.reader/register-tag-parser! "parsimony.views.cards.OverlayPickerCard" map->OverlayPickerCard)

(defn overlay-picker-card []
  (OverlayPickerCard.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Token Dashboard Card
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord TokenDashboardCard []
  ICard
  (card-key [_]
    "token-dashboard-card")
  (human-label [_]
    (let [modified? (subscribe [:token-dashboard-modified?])]
      (fn [this]
        [h-box
         :align :center
         :children [[label :label "Token Labels"]
                    (when @modified?
                      [save-button #(dispatch [:token-dashboard-save])])]])))
  (draw-card [_]
    [token-dashboard])
  (confirm-close? [_]
    false)
  (closeable? [_]
    false)
  (focusable? [_]
    true)
  (disabled? [_]
    (not (project/in-project?))))

(cljs.reader/register-tag-parser! "parsimony.views.cards.TokenDashboardCard" map->TokenDashboardCard)

(defn token-dashboard-card []
  (TokenDashboardCard.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Live Parse View Card
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord LiveParseViewCard []
  ICard
  (card-key [_]
    "live-parse-view-card")
  (human-label [_]
    [label :label "Live Tree View"])
  (draw-card [_]
    [live-parse-view])
  (confirm-close? [_]
    false)
  (closeable? [_]
    false)
  (focusable? [_]
    true)
  (disabled? [_]
    (not (project/in-project?))))

(cljs.reader/register-tag-parser! "parsimony.views.cards.LiveParseViewCard" map->LiveParseViewCard)

(defn live-parse-view-card []
  (LiveParseViewCard.))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse Dashboard Card
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord ParseDashboardCard []
  ICard
  (card-key [_]
    "parse-dashboard-card")
  (human-label [this]
    (let [modified? (subscribe [:parse-dashboard-modified?])]
      (fn [this]
        [h-box
         :align :center
         :children [[label :label "Parse Labels"]
                    (when @modified?
                      [save-button #(dispatch [:parse-dashboard-save])])]])))
  (draw-card [_]
    [parse-dashboard])
  (confirm-close? [_]
    false)
  (closeable? [_]
    false)
  (focusable? [_]
    true)
  (disabled? [_]
    (not (project/in-project?))))

(cljs.reader/register-tag-parser! "parsimony.views.cards.ParseDashboardCard" map->ParseDashboardCard)

(defn parse-dashboard-card []
  (ParseDashboardCard.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solver Card
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord SolverCard []
  ICard
  (card-key [_]
    "solver-card")
  (human-label [_]
    [label :label "Solver"])
  (draw-card [_]
    [solver-view])
  (confirm-close? [_]
    false)
  (closeable? [_]
    false)
  (focusable? [_]
    true)
  (disabled? [_]
    (not (project/in-project?))))

(cljs.reader/register-tag-parser! "parsimony.views.cards.SolverCard" map->SolverCard)

(defn solver-card []
  (SolverCard.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disambiguation Card
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- live-parse-view-human-label []
  (let [state (subscribe [:live-parse-view])
        forest (reaction (:forest @state))]
    [h-box
     :align :center
     :gap "3px"
     :children [[label :label
                 [:span
                  [:span "Disambiguation"]
                  [icon {:md-icon-name "zmdi-caret-right"}]
                  [:span "[ Live Tree View ]"]]]
                (when (seq @forest)
                  [icon
                   {:md-icon-name "zmdi-refresh"
                    :tooltip "Compute disambiguations"
                    :on-click #(dispatch [:live-parse-view/gen-disambiguations])}])]]))

(defn- solver-human-label []
  (let [state (subscribe [:solver])
        solution-exists? (subscribe [:solver/solution-exists?])]
    [h-box
     :align :center
     :gap "3px"
     :children [[label :label
                 [:span
                  [:span "Disambiguation"]
                  [icon {:md-icon-name "zmdi-caret-right"}]
                  [:span "[ Solver ]"]]]
                (when @solution-exists?
                  [icon
                   {:md-icon-name "zmdi-refresh"
                    :tooltip "Compute disambiguations"
                    :on-click #(dispatch [:solver/gen-disambiguations])}])]]))

(defrecord DisambiguationCard [current-card]
  ICard
  (card-key [_]
    "disambiguation-card")
  (human-label [_]
    (case @current-card
      "live-parse-view-card" [live-parse-view-human-label]
      "solver-card" [solver-human-label]
      ;; default
      [label :label "Disambiguation"]))
  (draw-card [_]
    [disambiguation-view])
  (confirm-close? [_]
    false)
  (closeable? [_]
    false)
  (focusable? [_]
    false)
  (disabled? [_]
    (or (not (contains? #{"live-parse-view-card" "solver-card"}
                        @current-card))
        (not (project/in-project?)))))

(defn disambiguation-card []
  (let [current-card (subscribe [:current-card])]
    (DisambiguationCard. current-card)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Log Card
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord LogCard []
  ICard
  (card-key [_]
    "log-card")
  (human-label [_]
    [label :label "Console"])
  (draw-card [_]
    [log-view])
  (confirm-close? [_]
    false)
  (closeable? [_]
    false)
  (focusable? [_]
    true)
  (disabled? [_]
    false))

(cljs.reader/register-tag-parser! "parsimony.views.cards.LogCard" map->LogCard)

(defn log-card []
  (LogCard.))
