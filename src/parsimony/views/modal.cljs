(ns parsimony.views.modal
  (:require [parsimony.config :as config]
            [parsimony.util :refer [pprint-str]]
            [parsimony.views.common :refer [icon persimmon-logo]]
            [re-com.core :refer [p label gap title h-box v-box modal-panel button scroller]]
            [re-frame.core :refer [dispatch subscribe]]
            [reagent.core :as reagent]))

(defprotocol IModal
  (show-modal [this] "Convert this into a string or hiccup for rendering to a modal panel")
  (backdrop-closeable? [this] "True iff this modal can be closed by clicking on the backdrop."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; About
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord AboutModal []
  IModal
  (show-modal [this]
    [v-box
     :class "about-modal"
     :align :center
     :children [[persimmon-logo {:width "100px" :height "100px"}]
                [gap :size "2px"]
                [title :label "Parsimony" :level :level2 :margin-bottom "0px" :margin-top "0px"]
                [gap :size "10px"]
                [label :label (str "Version " config/VERSION "--" config/BUILD)]
                [:a {:href "https://parsimony-ide.github.io"
                     :target "_blank"}
                 "https://parsimony-ide.github.io"]]])

  (backdrop-closeable? [this]
    true))

(cljs.reader/register-tag-parser! "parsimony.views.modal.AboutModal" map->AboutModal)

(defn about-modal []
  (AboutModal.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; No Token Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord NoTokenDefinitionModal []
  IModal
  (show-modal [this]
    [v-box
     :class "no-default-tokenizer-modal"
     :children
     [[h-box
       :align :center
       :gap "10px"
       :children
       [[icon {:md-icon-name "zmdi-alert-triangle"}] ;; make this button bigger
        [label :label "No token definition has been assigned.  Please mark a token definition in the file viewer."]]]]])

  (backdrop-closeable? [this]
    true))

(cljs.reader/register-tag-parser! "parsimony.views.modal.NoTokenDefinitionModal" map->NoTokenDefinitionModal)

(defn no-token-definition-modal []
  (NoTokenDefinitionModal.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; No CFG Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord NoCfgDefinitionModal []
  IModal
  (show-modal [this]
    [v-box
     :class "no-default-cfg-modal"
     :children
     [[h-box
       :align :center
       :gap "10px"
       :children
       [[icon {:md-icon-name "zmdi-alert-triangle"}] ;; make this button bigger
        [label :label "No CFG definition has been assigned.  Please mark a CFG definition in the file viewer."]]]]])

  (backdrop-closeable? [this]
    true))

(cljs.reader/register-tag-parser! "parsimony.views.modal.NoCfgDefinitionModal" map->NoCfgDefinitionModal)

(defn no-cfg-definition-modal []
  (NoCfgDefinitionModal.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; No Current Editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord NoCurrentEditorModal []
  IModal
  (show-modal [this]
    [v-box
     :class "no-current-editor-modal"
     :children
     [[h-box
       :align :center
       :gap "10px"
       :children
       [[icon {:md-icon-name "zmdi-alert-triangle"}]
        [label :label "No editor is currently selected.  Please focus an editor to select it."]]]]])

  (backdrop-closeable? [this]
    true))

(cljs.reader/register-tag-parser! "parsimony.views.modal.NoCurrentEditorModal" map->NoCurrentEditorModal)

(defn no-current-editor-modal []
  (NoCurrentEditorModal.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Confirm Close Project
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord ConfirmCloseProjectModal []
  IModal
  (show-modal [this]
    [v-box
     :class "confirm-close-project-modal"
     :gap "10px"
     :children
     [[h-box
       :align :center
       :gap "10px"
       :children
       [[icon {:md-icon-name "zmdi-alert-triangle"}]
        [label :label "The current project has been modified since last save."]]]
      [h-box
       :gap "5px"
       :children [[button
                   :label "Save, then close"
                   :class "btn btn-primary"
                   :on-click (fn []
                               (dispatch [:clear-modal])
                               (dispatch [:save-then-close-current-project]))]
                  [button
                   :label "Close without saving"
                   :class "btn btn-danger"
                   :on-click (fn []
                               (dispatch [:clear-modal])
                               (dispatch [:close-current-project-without-saving]))]
                  [button
                   :label "Cancel"
                   :on-click #(dispatch [:clear-modal])]]]]])

  (backdrop-closeable? [this]
    false))

(cljs.reader/register-tag-parser! "parsimony.views.modal.ConfirmCloseProjectModal" map->ConfirmCloseProjectModal)

(defn confirm-close-project-modal []
  (ConfirmCloseProjectModal.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comm Status
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn registry-entry
  [{:keys [registry-id request-id payload start-timestamp end-timestamp status]}]
  [:tr
   [:th (str registry-id)]
   [:td (str request-id)]
   [:td (.format (js/moment start-timestamp) "HH:mm:ss.SSS")]
   (if (some? end-timestamp)
     [:td (.format (js/moment.utc (- end-timestamp start-timestamp)) "ss.SSS")]
     [:td ""])
   [:td
    {:style {:background-color (case status
                                 :failure "red"
                                 :success "green"
                                 :in-flight "#bbb"
                                 "#ffffff")
             :color (if (= :in-flight status)
                      "#000000"
                      "#ffffff")}}
    (name status)]
   [:td (pprint-str payload)]])

(defn comm-detail-pane []
  (let [comm (subscribe [:comm])]
    (fn []
      [scroller
       :child
       [:table.comm-detail-pane
        [:thead
         [:tr
          [:th "registry-id"]
          [:th "request-id"]
          [:th "start-timestamp"]
          [:th "duration"]
          [:th "status"]
          [:th "payload"]]]
        (into [:tbody]
              (map (partial vector registry-entry))
              (reverse (sort-by :registry-id (vals (:registry @comm)))))]])))

(defrecord CommStatusModal []
  IModal
  (show-modal [this]
    [v-box
     :size "auto"
     :class "comm-status-modal"
     :children [[title :level :level3 :label (config/host) :underline? true]
                [comm-detail-pane]]])

  (backdrop-closeable? [this]
    true))

(cljs.reader/register-tag-parser! "parsimony.views.modal.CommStatusModal" map->CommStatusModal)

(defn comm-status-modal []
  (CommStatusModal.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Component
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn global-modal []
  (let [modal-value (subscribe [:modal])]
    (fn []
      (when @modal-value
        [modal-panel
         :class "global-modal"
         :backdrop-opacity 0.15
         :backdrop-on-click (fn []
                              (when (backdrop-closeable? @modal-value)
                                (dispatch [:clear-modal])))
         :child [show-modal @modal-value]]))))
