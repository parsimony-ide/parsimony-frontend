(ns parsimony.views.file-picker
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [parsimony.classroom.api :as classroom.api]
            [parsimony.com.box :refer [draggy-v-box]]
            [parsimony.com.buttons :refer [row-button button]]
            [parsimony.com.input :refer [input-text]]
            [parsimony.com.label :refer [label]]
            [parsimony.config :as config]
            [parsimony.models.source :as source]
            [parsimony.util :refer [matches-schema? get-drag-files set-drop-effect
                                    js-read-text-file drag-is-external?]]
            [parsimony.views.common :refer [icon]]
            [parsimony.views.tree-view :refer [ITreeLabel] :as tree-view]
            [parsimony.views.tree-view.common :refer [label-and-icon rename-button delete-button add-heading-button]]
            [re-com.core :refer [scroller box gap h-box title v-box p
                                 popover-content-wrapper popover-anchor-wrapper] :refer-macros [handler-fn]]
            [re-com.util :refer [deref-or-value]]
            [re-com.validate :refer [css-style?] :refer-macros [validate-args-macro]]
            [re-frame.core :refer [dispatch subscribe]]
            [reagent.core :as reagent]
            [schema.core :as s :include-macros true]
            [parsimony.console :as console])
  (:require-macros [reagent.ratom :refer [reaction]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Picker
;; - A File Picker is just a tree view in which elements have a :data property
;;   implementing ITreeLabel specifically for file-oriented views
;; - Just create a new type implementing ITreeLabel to create different kinds
;;   of File Picker nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Hover Buttons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn token-button [hover editing? on-click]
  [row-button
   :class "token-file"
   :md-icon-name "zmdi zmdi-star-circle"
   :mouse-over-row? (and @hover (not @editing?))
   :tooltip "Set Default Tokens"
   :on-click on-click])

(defn grammar-button [hover editing? on-click]
  [row-button
   :class "grammar-file"
   :md-icon-name "zmdi zmdi-check-circle"
   :mouse-over-row? (and @hover (not @editing?))
   :tooltip "Set Default Grammar"
   :on-click on-click])

(defn- file-hover-buttons [id hover editing?]
  [h-box
   :margin "0 0 0 15px"
   :attr {:on-click (handler-fn (.stopPropagation event))}
   :children
   [[h-box
     :children
     [[token-button hover editing? #(dispatch [:set-default-tokens id])]
      [grammar-button hover editing? #(dispatch [:set-default-grammar id])]
      [gap :size "5px"]
      [rename-button hover editing? nil]
      [gap :size "5px"]
      [delete-button hover editing? #(dispatch [:file-picker-delete id])]]]]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Heading Hover Buttons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- add-scratch-button [hover editing? on-click]
  [row-button
   :md-icon-name "zmdi zmdi-plus"
   :style {:color "black"}
   :mouse-over-row? (and @hover (not @editing?))
   :tooltip "Add Scratch File"
   :on-click on-click])

(defn- heading-hover-buttons [id hover editing?]
  "Buttons for a normal heading (can be edited or deleted)"
  [h-box
   :margin "0 0 0 15px"
   :attr {:on-click (handler-fn (.stopPropagation event))}
   :children
   [[h-box
     :children
     [[add-heading-button hover editing? #(dispatch [:file-picker-add-heading id])]
      [add-scratch-button hover editing? #(dispatch [:file-picker-add-scratch id])]
      [rename-button hover editing? nil]
      [gap :size "5px"]
      [delete-button hover editing? #(dispatch [:file-picker-delete-heading id])]]]]])

(defn- static-heading-hover-buttons
  "Buttons for a heading that cannot be deleted or renamed"
  [id hover _]
  (let [editing? (reagent/atom false)]
    (fn [id hover]
      [h-box
       :margin "0 0 0 15px"
       :attr {:on-click (handler-fn (.stopPropagation event))}
       :children
       [[h-box
         :children
         [[add-heading-button hover editing? #(dispatch [:file-picker-add-heading id])]
          [add-scratch-button hover editing? #(dispatch [:file-picker-add-scratch id])]]]]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree Element Labels
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn file-label-and-icon [id fname md-icon-name]
  [label-and-icon {:id id
                   :label-str fname
                   :md-icon-name md-icon-name
                   :hover-buttons file-hover-buttons
                   :on-click #(dispatch [:file-picker-click %1])
                   :on-edit-in-place #(dispatch [:rename-source %1 %2])}])

(defn heading-label-and-icon [id fname md-icon-name]
  [label-and-icon {:id id
                   :label-str fname
                   :md-icon-name md-icon-name
                   :hover-buttons heading-hover-buttons
                   :on-edit-in-place #(dispatch [:file-picker-rename-heading %1 %2])}])

(defn static-heading-label-and-icon [id fname md-icon-name]
  [label-and-icon {:id id
                   :label-str fname
                   :md-icon-name md-icon-name
                   :hover-buttons static-heading-hover-buttons}])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ITreeLabel Instances
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IFilePickerElement)
(defprotocol IFileElement
  (backing-source-id [this]))

(defrecord NamedFile [id fname]
  IFilePickerElement
  IFileElement
  (backing-source-id [this]
    id)
  ITreeLabel
  (tree-label [this]
    [box
     :class "named-file"
     :child [file-label-and-icon id fname "zmdi zmdi-file"]]))

(cljs.reader/register-tag-parser! "parsimony.views.file-picker.NamedFile" map->NamedFile)

(defn named-file [id fname]
  (NamedFile. id fname))

(defrecord TokenFile [id fname]
  IFilePickerElement
  IFileElement
  (backing-source-id [this]
    id)
  ITreeLabel
  (tree-label [this]
    [box
     :class "token-file"
     :child [file-label-and-icon id fname "zmdi zmdi-star-circle"]]))

(cljs.reader/register-tag-parser! "parsimony.views.file-picker.TokenFile" map->TokenFile)

(defn token-file [id fname]
  (TokenFile. id fname))

(defrecord GrammarFile [id fname]
  IFilePickerElement
  IFileElement
  (backing-source-id [this]
    id)
  ITreeLabel
  (tree-label [this]
    [box
     :class "grammar-file"
     :child [file-label-and-icon id fname "zmdi zmdi-check-circle"]]))

(cljs.reader/register-tag-parser! "parsimony.views.file-picker.GrammarFile" map->GrammarFile)

(defn grammar-file [id fname]
  (GrammarFile. id fname))

(defrecord ScratchFile [id fname]
  IFilePickerElement
  IFileElement
  (backing-source-id [this]
    id)
  ITreeLabel
  (tree-label [this]
    [box
     :class "scratch-file"
     :child [file-label-and-icon id fname "zmdi zmdi-flip"]]))

(cljs.reader/register-tag-parser! "parsimony.views.file-picker.ScratchFile" map->ScratchFile)

(defn scratch-file [id fname]
  (ScratchFile. id fname))

(defrecord Heading [id fname md-icon-name editable?]
  IFilePickerElement
  ITreeLabel
  (tree-label [this]
    [box
     :class "file-picker-heading"
     :child (if editable?
              [heading-label-and-icon id fname md-icon-name]
              [static-heading-label-and-icon id fname md-icon-name])]))

(cljs.reader/register-tag-parser! "parsimony.views.file-picker.Heading" map->Heading)

(defn heading [id fname md-icon-name editable?]
  (Heading. id fname md-icon-name editable?))

(defn heading-element? [e]
  (instance? Heading (:data e)))

(defrecord NonexistentFile []
  IFilePickerElement
  ITreeLabel
  (tree-label [_]
    [h-box
     :align :baseline
     :gap "5px"
     :children [[icon {:md-icon-name "zmdi zmdi-block"}]
                [label :label "File not found"]]]))

(defn nonexistent-file []
  (NonexistentFile.))

(cljs.reader/register-tag-parser! "parsimony.views.file-picker.NonexistentFile" map->NonexistentFile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Drag and Drop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- drag-indicator
  "The visible area that appears when you drag a file into the external-drag-target"
  [dragging?]
  (when dragging?
    [h-box
     :size "auto"
     :align :center
     :style {:border "5px dashed #eaeaea"
             :justify-content "center"
             :pointer-events "none"}
     :children [[label
                 :style {:font-size "23px"
                         :color "#d0d0d0"}
                 :label "Drag File Here"]]]))

(defn- external-drag-target
  "Component that receives external file drag events."
  [args]
  (let [dragging? (reagent/atom nil)
        read-success-fn (fn [{:keys [filename mime-type content] :as result}]
                          (console/debug :upload-success
                                         {:filename filename
                                          :mime-type mime-type
                                          :content-length (count content)})
                          (dispatch [:file-picker-upload filename mime-type content]))
        read-error-fn (fn [{:keys [filename mime-type reason] :as result}]
                        (console/error :upload-failure
                                       {:filename filename
                                        :mime-type mime-type
                                        :reason reason}))]
    (fn [args]
      [v-box
       :size "auto"
       :class "external-drag-target"
       :min-height "50px"
       :attr {:on-drag-over (handler-fn
                             (when (drag-is-external? event)
                               (set-drop-effect event :copy)
                               (.preventDefault event)))
              :on-drop (handler-fn
                        (.preventDefault event) ;; disable default drop event (.e., open file in browser)
                        (.stopPropagation event)
                        (let [files (get-drag-files event)]
                          (doseq [f files]
                            (js-read-text-file f read-success-fn read-error-fn)))
                        (reset! dragging? nil))
              :on-drag-enter (handler-fn
                              (when (drag-is-external? event)
                                (set-drop-effect event :copy)
                                (reset! dragging? true)))
              :on-drag-leave (handler-fn
                              (reset! dragging? nil))}
       :children [[drag-indicator @dragging?]]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Picker Behavior
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn satisfies-valid-drop-fn
  "Validation function that only allows drops between elements satisfying the IFilePickerElement protocol."
  [data parent _]
  (when-let [tree-elem (get data ::element)]
    (let [res (vector (matches-schema? tree-view/element-schema tree-elem)
                      (matches-schema? tree-view/element-schema parent)
                      (satisfies? IFilePickerElement (:data tree-elem))
                      (satisfies? IFilePickerElement (:data parent)))]
      (if (every? identity res)
        true
        (do (console/warn :invalid-drop res)
            false)))))

(defn satisfies-valid-drag-type-fn
  [drag-types _ _]
  (seq (set/intersection drag-types #{::element})))

;; A SourceLink is a pointer to a source. To resolve each SourceLink, we
;; replace it with a concrete ITreeLabel instance for the corresponding source
;; type via resolve-source-link.
;;
;; The SourceLink tree-label throws an exception, because it's never supposed to be drawn.
;;
;; XXX: this is pretty ugly, and I think unnecessary.  Instead, why not just subscribe to the source in the implementation
;;      of tree-label, and use the subscription to do whatever drawing is necessary?
;;      Alternatively, tree-label can use the subscription to create a local concrete ITreeLabel implementation that
;;      does the heavy lifting. Then we don't need an explicit resolve-source-link step
(defprotocol ISourceLink)
(defrecord SourceLink [source-id]
  ISourceLink
  ITreeLabel
  (tree-label [_] (throw (ex-info (str "Illegal: attempt to draw a SourceLink for " source-id) {:data source-id}))))

(defn- source-link [source-id]
  (SourceLink. source-id))

(defn source-link? [d]
  (satisfies? ISourceLink d))

(defn source-link-element? [e]
  (source-link? (:data e)))

(defn source->tree-label [source]
  (case (:source-type source)
    :scratch (scratch-file (:id source) (source/source-path source))
    :file (named-file (:id source) (source/source-path source))
    :token (token-file (:id source) (source/source-path source))
    :grammar (grammar-file (:id source) (source/source-path source))
    (throw (ex-info (str "Unknown source-type " (:source-type source))
                    {:data source}))))

(defn- resolve-source-link
  [data sources]
  (if (satisfies? ISourceLink data)
    (if-let [source (get sources (:source-id data))]
      (source->tree-label source)
      (nonexistent-file))
    data))

(defn resolve-source-links
  "Convert all instances of SourceLink to a concrete ITreeLabel"
  [model sources]
  (vec
   (for [{:keys [data] :as element} model]
       (assoc element :data (resolve-source-link data sources)))))

(defn- hidden-file? [sources {:keys [data] :as element}]
  (when (satisfies? ISourceLink data)
    (when-let [source (get sources (:source-id data))]
      (or (classroom.api/answer-key? source)
          (classroom.api/lesson-plan? source)))))

(defn- remove-hidden-files
  "Remove hidden files from the file-picker model"
  [model sources]
  (if (not= "dev" config/BUILD)
    (into []
          (remove (partial hidden-file? sources))
          model)
    model))

(defn file-picker []
  (let [sources (subscribe [:sources])
        model (subscribe [:file-picker-state])
        internal-model (reaction
                         (-> @model
                             (remove-hidden-files @sources)
                             (resolve-source-links @sources)))
        ;; prevent external file events from propagating to the wrapped tree-parent
        drag-block-fn (handler-fn
                       (when (drag-is-external? event)
                         (set-drop-effect event :none)
                         (.preventDefault event)
                         (.stopPropagation event)))]
    (fn []
      #_(console/debug "file-picker" (pr-str @internal-model))
      [v-box
       :class "file-picker"
       :size "auto"
       :children [[scroller
                   :h-scroll :off
                   :attr {:on-drag-enter-capture drag-block-fn
                          :on-drag-over-capture drag-block-fn}
                   :child [tree-view/tree-parent
                           {:model internal-model
                            :drag-type ::element
                            :valid-drop-fn satisfies-valid-drop-fn
                            :valid-drag-type-fn satisfies-valid-drag-type-fn
                            :on-toggle #(dispatch [:file-picker-toggle (:id %)])
                            :on-move #(dispatch [:file-picker-move (:id %1) (:id %2) %3])}]]
                  [external-drag-target]]])))


;; the default-model must have a root since a nested-model cannot be constructed from an empty list of nodes
(def default-model
  [{:id 0 :parent nil :draggable false :data (heading 0 "Project" "zmdi-storage" false)}])
