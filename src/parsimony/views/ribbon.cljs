(ns parsimony.views.ribbon
  (:require [parsimony.commands :as commands]
            [re-com.core :refer [h-box label line button md-icon-button gap checkbox] :refer-macros [handler-fn]]
            [re-com.validate :refer [css-style?] :refer-macros [validate-args-macro]]
            [reagent.core :as reagent]
            [re-frame.core :refer [dispatch subscribe]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ribbon Sub-Components
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;------------------------------------------------------------------------------
;; Ribbon Button
;;------------------------------------------------------------------------------

(def ribbon-button-args-desc
  [{:name :text         :required true  :type "string | hiccup"}
   {:name :md-icon-name :required true  :type "md-icon-name string"}
   {:name :icon-size    :required false :type ":regular | :smaller | :larger" :validate-fn keyword?}
   {:name :tooltip      :required false :type "string"}
   {:name :on-click     :required false :type "-> nil"                        :validate-fn fn?}])

(defn ribbon-button [args]
  {:pre [(validate-args-macro ribbon-button-args-desc args "ribbon-button")]}
  (let [hover (reagent/atom nil)]
    (fn [{:keys [text md-icon-name icon-size tooltip on-click] :as args
          :or {icon-size :smaller}}]
      [h-box
       :children [[h-box
                   :class (str "ribbon-button "
                               (if @hover "hover " " "))
                   :justify :center
                   :align :center
                   :gap "5px"
                   :padding "0 0 0 2px"
                   :attr {:on-click (when on-click
                                      (handler-fn (on-click)))
                          :on-mouse-enter (handler-fn
                                            (reset! hover true))
                          :on-mouse-leave (handler-fn
                                            (reset! hover nil))}
                   :children [[md-icon-button
                               :md-icon-name md-icon-name
                               :size icon-size
                               :tooltip tooltip
                               :on-click on-click]
                              [label :label text]
                              [gap :size "3px"]]]
                  [line]]])))

;;------------------------------------------------------------------------------
;; Ribbon Image Button
;;------------------------------------------------------------------------------

(def ribbon-image-button-args-desc
  [{:name :text      :required true  :type "string | hiccup"}
   {:name :image-url :required true  :type "url string"}
   {:name :on-click  :required false :type "-> nil"           :validate-fn fn?}])

(defn ribbon-image-button [args]
  {:pre [(validate-args-macro ribbon-image-button-args-desc args "ribbon-image-button")]}
  (let [hover (reagent/atom nil)]
    (fn [{:keys [text image-url style tooltip on-click] :as args}]
      [h-box
       :children [[h-box
                   :class (str "ribbon-button "
                               (if @hover "hover " " "))
                   :justify :center
                   :align :center
                   :gap "5px"
                   :padding "0 0 0 2px"
                   :attr {:on-click (when on-click
                                      (handler-fn (on-click)))
                          :on-mouse-enter (handler-fn
                                            (reset! hover true))
                          :on-mouse-leave (handler-fn
                                            (reset! hover nil))}
                   :children [[:span
                               {:style
                                (merge {:width "20px"
                                        :height "20px"
                                        :background-image image-url
                                        :background-size "100% 100%"})}]
                              [label :label text]
                              [gap :size "3px"]]]
                  [line]]])))

(defn- ribbon-button-wrapper [args]
  (cond
    (:md-icon-name args)
    [ribbon-button args]
    (:image-url args)
    [ribbon-image-button args]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IRibbonItem
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IRibbonItem
  (ribbon-item [this] "Return string or hiccup for rendering ribbon item"))

;;------------------------------------------------------------------------------
;; Button Item
;;------------------------------------------------------------------------------

(defrecord ButtonItem [args]
  IRibbonItem
  (ribbon-item [this]
    [h-box
     :class "ribbon-item"
     :children
     [(ribbon-button-wrapper args)]]))

(defn button-item [args]
  (ButtonItem. args))

;;------------------------------------------------------------------------------
;; Command Item
;;------------------------------------------------------------------------------

(defrecord CommandItem [command-kw command-arg button-args]
  IRibbonItem
  (ribbon-item [this]
    (if-not (commands/hidden? command-kw)
      (let [{:keys [description keybind]} (commands/get-command command-kw)
            enabled? (subscribe [:command-enabled? command-kw command-arg])]
        (fn [this]
          [h-box
           :class (str "ribbon-item command "
                       (if @enabled?
                         "enableme"
                         "disableme"))
           :children
           [(ribbon-button-wrapper
              (merge button-args
                     {:text description
                      :on-click #(dispatch [:exec-command-from-ribbon command-kw command-arg])}))]]))
      [:span {:style {:display "none"}}])))

(defn command-item
  ([command-kw button-args]
   (command-item command-kw nil button-args))
  ([command-kw command-arg button-args]
   (CommandItem. command-kw command-arg button-args)))

;;------------------------------------------------------------------------------
;; Checkbox Item
;;------------------------------------------------------------------------------

(defrecord CheckboxItem [label model on-change]
  IRibbonItem
  (ribbon-item [this]
    [checkbox
     :model model
     :on-change on-change
     :label label]))

(defn checkbox-item
  [{:keys [label model on-change] :as args}]
  (CheckboxItem. label model on-change))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ribbon
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ribbon [{:keys [model] :as args}]
  [h-box
   :class "ribbon"
   :width "100%"
   :justify :start
   :gap "5px"
   :children (into []
                   (map (partial vector ribbon-item))
                   model)])
