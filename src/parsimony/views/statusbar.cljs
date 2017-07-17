(ns parsimony.views.statusbar
  (:require [re-com.core :refer [box h-box v-box label gap progress-bar popover-content-wrapper popover-anchor-wrapper] :refer-macros [handler-fn]]
            [re-frame.core :refer [dispatch subscribe]]
            [reagent.core :as reagent]
            [reagent.ratom :include-macros true :refer-macros [reaction] :as ratom]
            [parsimony.commands :as commands]
            [parsimony.util :refer [pprint-str]]
            [parsimony.views.common :refer [icon]]
            [parsimony.console :as console]))

(defn progress-monitor []
  (let [progress (subscribe [:progress])
        delay-timeout-id (atom nil) ;; must be a regular atom, not a Reagant atom. Otherwise, the ratio reaction will cycle forever on itself.
        delay-on (reagent/atom false)
        ratio (reaction
                (let [res (min 100 (int (* 100 (/ (:current @progress) (:max @progress)))))]
                  #_(console/debug :progress-ratio-change)
                  (reset! delay-on true)
                  (when-let [timeout-id @delay-timeout-id]
                    (.clearTimeout js/window timeout-id))
                  (reset! delay-timeout-id
                          (.setTimeout js/window #(reset! delay-on false) 5000))
                  res))
        failure? (reaction (= :failure (:status @progress)))
        success? (reaction (and (= :success (:status @progress))
                                (>= @ratio 100)))
        hidden? (reaction (or (not (seq @progress))
                              (and (>= @ratio 100)
                                   (not @delay-on)
                                   (not @failure?))))
        popover-showing? (reaction (and @failure? @delay-on))]
    (fn []
      (let [status-class
            (cond
              @success? "success"
              @failure? "failure"
              :else "")]
        [h-box
         :class (str "progress-monitor "
                     (if @hidden?
                       "hideme "
                       "showme "))
         :align :center
         :gap "10px"
         :children [[progress-bar
                     :class status-class
                     :striped? (not @failure?)
                     :style {:margin "0px"
                             :border-radius "0px"}
                     :width "200px"
                     :model ratio]
                    [popover-anchor-wrapper
                     :showing? popover-showing?
                     :position :above-right
                     :anchor (cond
                               @failure? [icon {:md-icon-name "failure zmdi-alert-polygon"
                                                :on-click #(reset! delay-on false)}]
                               @success? [icon {:md-icon-name "success zmdi-check-circle"}]
                               :else [gap :size "20px"])
                     :popover [popover-content-wrapper
                               :showing? popover-showing?
                               :position :above-left
                               :popover-color "#f2dede"
                               :padding "20px"
                               :body
                               [:span {:style {:font-weight "lighter"
                                               :font-size "22px"}
                                       :on-click (handler-fn
                                                   (reset! delay-on false))}
                                [:span "Attention Required!"]]]]
                    [label
                     :class "progress-description"
                     :label (:description @progress)]]]))))

(defn last-command-monitor []
  (let [last-command (subscribe [:last-command])
        dispose-on-unmount (atom []) ;; holds all references that must be disposed on unmount
        delay-timeout-id (atom nil)  ;; must be a regular atom, not a Reagant atom. Otherwise, the reaction below will cycle forever on itself.
        delay-on (reagent/atom false)]
    (reagent/create-class
      {:component-will-unmount
       (fn [_]
         (doseq [x @dispose-on-unmount]
           (ratom/dispose! x)))
       :component-did-mount
       (fn [_]
         (swap! dispose-on-unmount conj
                (ratom/run!
                  (let [lc @last-command]
                    #_(console/debug :last-command-change lc)
                    (reset! delay-on true)
                    (when-let [timeout-id @delay-timeout-id]
                      (.clearTimeout js/window timeout-id))
                    (reset! delay-timeout-id
                            (.setTimeout js/window #(reset! delay-on false) 2000))
                    nil))))
       :display-name "last-command-monitor"
       :reagent-render
       (fn []
         (let [{:keys [origin kw]} @last-command
               hidden? (or (not kw)
                           (not @delay-on))]
           [h-box
            :class (str "last-command-monitor "
                        (if hidden?
                          "hideme"
                          "showme"))
            :gap "3px"
            :align :baseline
            :children [[label
                        :class "origin"
                        :label (case origin
                                 :keyboard "Shortcut action:"
                                 :menu "Menu action:"
                                 :ribbon "Button action:"
                                 ;; default
                                 "Action:")]
                       (when (some? kw)
                         [box
                          :class (str "command "
                                      (when (some? origin)
                                        (name origin)))
                          :child [label :label (:description (commands/get-command kw))]])]]))})))

(defn statusbar
  []
  [v-box
   :class "statusbar"
   :children
   [[gap :size "2px"]
    [h-box
     :justify :between
     :children [[progress-monitor]
                [last-command-monitor]]]
    [gap :size "2px"]]])
