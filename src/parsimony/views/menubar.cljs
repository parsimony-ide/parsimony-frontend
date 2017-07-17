(ns parsimony.views.menubar
  (:require [parsimony.classroom.ui :as classroom.ui]
            [parsimony.com.menu :refer [IMenuLabel] :as menu]
            [parsimony.commands :as commands]
            [parsimony.config :as config]
            [parsimony.views.common :refer [icon]]
            [parsimony.views.keybind :as keybind]
            [parsimony.views.quick-add :as quick-add]
            [re-com.core :refer [gap label h-box v-box] :refer-macros [handler-fn]]
            [re-com.util :refer [deref-or-value]]
            [re-frame.core :refer [dispatch subscribe]]
            [reagent.ratom :refer-macros [reaction]]))

(defrecord Heading [fname md-icon-name]
  IMenuLabel
  (menu-label [this]
    [h-box
     :class "menu-label heading"
     :align :baseline
     :gap "5px"
     :children [[label :label (:fname this)]
                (when md-icon-name [icon {:md-icon-name (:md-icon-name this)}])]]))

(defn heading
  ([fname]
   (heading fname nil))
  ([fname md-icon-name]
   (Heading. fname md-icon-name)))

(defrecord CommandItem [command-kw]
  IMenuLabel
  (menu-label [this]
    (let [{:keys [description keybind]} (commands/get-command command-kw)
          enabled? (subscribe [:command-enabled? command-kw nil])]
      (fn [this]
        (if-not (commands/hidden? command-kw)
          [h-box
           :class (str "menu-label command "
                       (if @enabled?
                         "enableme"
                         "disableme"))
           :attr {:on-click (handler-fn
                              (dispatch [:exec-command-from-menu command-kw nil]))}
           :justify :between
           :gap "15px"
           :children [[label :label description]
                      (when (some? keybind)
                        [keybind/keybind-view keybind])]]
          [:span {:style {:display "none"}}])))))

(defn command-item
  [command-kw]
  (CommandItem. command-kw))

;; radio button-like
(defrecord Choose [fname command-kw md-icon-name choices])

(defn comm-status []
  (let [comm (subscribe [:comm])
        num-requests (reaction (apply max 0 (keys (:registry @comm))))
        num-failures (reaction (:num-failures @comm))
        num-in-flight (reaction (count (:in-flight @comm)))]
    (fn []
      (let [on-click #(dispatch [:comm/show-details])
            tooltip [v-box
                     :style {:padding-right "50px"}
                     :children
                     [[label :label (str @num-in-flight " in flight")]
                      [label :label (str @num-requests " total requests")]
                      [label :label (str @num-failures " failures")]]]]
      [h-box
       :class (str "comm-status "
                   (if (:open? @comm)
                     "online "
                     "offline "))
       :align :center
       :gap "5px"
       :children
       (if (:open? @comm)
         [[label :label "Connection OK"]
          [icon {:md-icon-name "zmdi-portable-wifi"
                 :class (str (if (seq (:in-flight @comm))
                               "in-flight"
                               "idle")
                             " "
                             (if (:needs-attention? @comm)
                               "needs-attention"
                               ""))
                 :on-click on-click
                 :tooltip tooltip
                 :tooltip-position :below-left}]]
         [[label :label "Connection Offline"]
          [icon {:md-icon-name "zmdi-portable-wifi-off"
                 :on-click on-click
                 :tooltip tooltip
                 :tooltip-position :below-left}]])]))))

(defn menubar []
  [h-box
   :class "menubar"
   :justify :between
   :children
   [[menu/menubar
     {:model [[{:id 0 :parent nil :data (heading "Project")}
               {:id 1 :parent 0 :data (command-item :save-project)}
               {:id 2 :parent 0 :data (command-item :save-as-new-project)}
               {:id 3 :parent 0 :data (command-item :close-current-project)}
               #_{:id 4 :parent 0 :data (command-item :export-project)}
               #_{:id 5 :parent 0 :data (command-item :edit-preferences)}]
              [{:id 0 :parent nil :data (heading "File")}
               {:id 1 :parent 0 :data (command-item :save-current-file)}
               {:id 2 :parent 0 :data (command-item :format-current-file)}]
              [{:id 0 :parent nil :data (heading "Build")}
               {:id 1 :parent 0 :data (command-item :compile-lexer)}
               {:id 2 :parent 0 :data (command-item :compile-parser)}]
              [{:id 0 :parent nil :data (heading "Run")}
               {:id 1 :parent 0 :data (command-item :lex-current-file)}
               {:id 2 :parent 0 :data (command-item :parse-current-file)}
               {:id 3 :parent 0 :data (command-item :batch-lex-current-file)}
               {:id 4 :parent 0 :data (command-item :batch-parse-current-file)}]
              [{:id 0 :parent nil :data (heading "Help")}
               {:id 1 :parent 0 :data (command-item :show-parsing-lesson)}
               {:id 2 :parent 0 :data (command-item :show-lexer-reference)}
               {:id 3 :parent 0 :data (command-item :show-parser-reference)}
               {:id 4 :parent 0 :data (command-item :show-operator-reference)}
               {:id 5 :parent 0 :data (command-item :show-important-tips)}
               {:id 6 :parent 0 :data (command-item :install-example-projects)}
               {:id 7 :parent 0 :data (command-item :about)}]
              (when (= "dev" config/BUILD)
                [{:id 0 :parent nil :data (heading "Admin")}
                 {:id 1 :parent 0 :data (command-item :show-admin-panel)}])]}]
    [quick-add/quick-add-bar]
    [h-box
     :gap "10px"
     :children
     [#_[classroom.ui/pause-finish-bar]
      [comm-status]]]]])

