(ns parsimony.classroom.ui
  (:require [parsimony.views.common :refer [icon]]
            [re-com.core :refer [v-box h-box label] :refer-macros [handler-fn]]
            [re-frame.core :refer [dispatch subscribe]]
            [parsimony.views.modal :as modal]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;------------------------------------------------------------------------------
;; Task Incomplete Modal
;;------------------------------------------------------------------------------

(defrecord TaskIncompleteModal [source-path prerequisite-source-path]
  modal/IModal
  (show-modal [this]
    [v-box
     :class "task-incomplete-modal"
     :children
     [[h-box
       :align :center
       :gap "10px"
       :children
       [[icon {:md-icon-name "zmdi-alert-triangle"
               :style {:font-size "40px"}}]
        [:p {:style {:font-size "16px"
                     :margin "0px"
                     :padding "0px"
                     :width "450px"}}
         "You have not yet completed the prerequisites for " [:code source-path] " yet."
         "  Please ensure all tests are passing for " [:code prerequisite-source-path]
         " first."]]]]])

  (backdrop-closeable? [this]
    true))

(cljs.reader/register-tag-parser! "parsimony.classroom.ui.TaskIncompleteModal" map->TaskIncompleteModal)

(defn task-incomplete-modal [source-path prerequisite-source-path]
  (TaskIncompleteModal. source-path prerequisite-source-path))

;;------------------------------------------------------------------------------
;; Pause Modal
;;------------------------------------------------------------------------------

(defrecord PauseModal []
  modal/IModal
  (show-modal [this]
    [v-box
     :class "pause-modal"
     :style {:cursor "pointer"}
     :attr {:on-click
            (handler-fn
              (dispatch [:classroom/start])
              (dispatch [:clear-modal]))}
     :children
     [[h-box
       :align :center
       :gap "10px"
       :children
       [[icon {:md-icon-name "zmdi-pause"
               :style {:font-size "40px"}}]
        [:p {:style {:font-size "16px"
                     :margin "0px"
                     :padding "0px"
                     :width "500px"}}
         "Your task is currently paused.  When you are ready to begin, click here."]]]]])

  (backdrop-closeable? [this]
    false))

(cljs.reader/register-tag-parser! "parsimony.classroom.ui.PauseModal" map->PauseModal)

(defn pause-modal []
  (PauseModal.))

;;------------------------------------------------------------------------------
;; Cannot Finish Modal
;;------------------------------------------------------------------------------

(defrecord CannotFinishModal [source-path]
  modal/IModal
  (show-modal [this]
    [v-box
     :class "cannot-finish-modal"
     :children
     [[h-box
       :align :center
       :gap "10px"
       :children
       [[icon {:md-icon-name "zmdi-alert-triangle"
               :style {:font-size "40px"}}]
        [:p {:style {:font-size "16px"
                     :margin "0px"
                     :padding "0px"}}
         "You are not ready to finish this task yet. Not all tests are passing for "
         [:code source-path]
         "."]]]]])

  (backdrop-closeable? [this]
    true))

(cljs.reader/register-tag-parser! "parsimony.classroom.ui.CannotFinishModal" map->CannotFinishModal)

(defn cannot-finish-modal [source-path]
  (CannotFinishModal. source-path))

;;------------------------------------------------------------------------------
;; Finish Modal
;;------------------------------------------------------------------------------

(defrecord FinishModal [project-name]
  modal/IModal
  (show-modal [this]
    [v-box
     :class "finish-modal"
     :style {:cursor "pointer"}
     :attr {:on-click
            (handler-fn
              (dispatch [:save-then-close-current-project])
              (dispatch [:clear-modal]))}
     :children
     [[h-box
       :align :center
       :gap "10px"
       :children
       [[icon {:md-icon-name "zmdi-check-circle"
               :style {:font-size "40px"}}]
        [:p {:style {:font-size "16px"
                     :margin "0px"
                     :padding "0px"}}
         "You have completed the " [:code project-name] "task. "
         "Click here to save and close the project."]]]]])

  (backdrop-closeable? [this]
    false))

(cljs.reader/register-tag-parser! "parsimony.classroom.ui.FinishModal" map->FinishModal)

(defn finish-modal [project-name]
  (FinishModal. project-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pause-Finish Bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pause-button []
  (let [enabled? (subscribe [:in-project?])]
    (fn []
      [h-box
       :class (str "pause-button "
                   (when (not @enabled?)
                     "disableme "))
       :attr {:on-click (handler-fn (dispatch [:classroom/pause]))}
       :align :center
       :gap "5px"
       :children [[icon {:md-icon-name "zmdi-pause"}]
                  [label :label "Pause Task"]]])))

(defn request-assistance-button []
  [h-box
   :class "request-assistance-button"
   :attr {:on-click (handler-fn (dispatch [:classroom/request-assistance]))}
   :align :center
   :gap "5px"
   :children [[icon {:md-icon-name "zmdi-pin-help"}]
              [label :label "Request Assistance"]]])

(defn finish-button []
  (let [enabled? (subscribe [:in-project?])]
    (fn []
      [h-box
       :class (str "finish-button "
                   (when (not @enabled?)
                     "disableme "))
       :attr {:on-click (handler-fn (dispatch [:classroom/maybe-finish]))}
       :align :center
       :gap "5px"
       :children [[icon {:md-icon-name "zmdi-stop"}]
                  [label :label "Finish Task"]]])))

(defn pause-finish-bar []
  [h-box
   :class "pause-finish-bar"
   :gap "5px"
   :children [[request-assistance-button]
              [pause-button]
              [finish-button]]])
