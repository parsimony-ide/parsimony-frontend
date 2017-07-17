(ns parsimony.views.admin
  (:require [parsimony.com.buttons :refer [button]]
            [parsimony.com.input :refer [input-text]]
            [parsimony.views.modal :refer [IModal]]
            [reagent.core :as reagent]
            [re-com.core :refer [h-box v-box scroller line title gap] :refer-macros [handler-fn] :as re-com]
            [re-frame.core :refer [dispatch]]
            [parsimony.console :as console]))

(defn admin-pane []
  (let [source-path (reagent/atom "")
        password (reagent/atom "")]
    (fn []
      [v-box
       :gap "5px"
       :children
       [[button
         :label "Reset running workers"
         :on-click (fn []
                     (dispatch [:reset-running-workers])
                     (dispatch [:clear-modal]))]
        [input-text
         :model source-path
         :auto-focus true
         :on-change #(reset! source-path %)]
        [:input
         {:type "password"
          :on-change
          (handler-fn
            (reset! password (-> event .-target .-value)))
          :on-key-up
          (handler-fn
            (case (.-which event)
              13 (do #_(console/debug ::admin-pane-submit {:source-path @source-path :password @password})
                     (when (= @password "socrates")
                       (dispatch [:classroom/force-pass @source-path])
                       (dispatch [:clear-modal])))
              true))}]]])))

(defrecord AdminModal []
  IModal
  (show-modal [this]
    [v-box
     :size "auto"
     :class "admin"
     :children [[admin-pane]]])

  (backdrop-closeable? [this]
    true))

(cljs.reader/register-tag-parser! "parsimony.views.admin.AdminModal" map->AdminModal)

(defn admin-modal []
  (AdminModal.))


