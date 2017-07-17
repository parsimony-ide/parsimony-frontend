(ns parsimony.views.common
  (:require [re-com.core :refer [h-box v-box box gap hyperlink-href title line label hyperlink-href align-style popover-tooltip] :refer-macros [handler-fn]]
            [re-com.validate :refer [css-style? html-attr? string-or-hiccup? position?] :refer-macros [validate-args-macro]]
            [reagent.core :as reagent]))

(defn panel-title
  "Shown across the top of each page"
  [panel-name]
  [v-box
   :children [[h-box
               :margin "0px 0px 9px 0px"
               :height "54px"
               :align :end
               :children [[title
                           :label         panel-name
                           :level         :level2
                           :margin-bottom "0px"
                           :margin-top    "2px"]]]
              [line]]])

(def icon-args-desc
  [{:name :md-icon-name :required true :type "string" :validate-fn string?}
   {:name :class :required false :type "string" :validate-fn string?}
   {:name :style :required false :type "CSS style map" :validate-fn css-style?}
   {:name :attr :required false :type "HTML attr map" :validate-fn html-attr?}
   {:name :on-click :required false :type "-> nil" :validate-fn fn?}
   {:name :on-mouse-over :required false :type "-> nil" :validate-fn fn?}
   {:name :on-mouse-out :required false :type "-> nil" :validate-fn fn?}
   {:name :tooltip :required false :type "string | hiccup" :validate-fn string-or-hiccup?}
   {:name :tooltip-position :required false :type "keyword" :validate-fn position?}])

(defn icon
  "A material design icon with the given classname"
  [args]
  {:pre [(validate-args-macro icon-args-desc args "icon")]}
  (let [showing? (reagent/atom false)]
    (fn [{:keys [md-icon-name class style attr on-click on-mouse-over on-mouse-out tooltip tooltip-position] :as args}]
      (let [the-icon [:i (merge {:class (str "zmdi zmdi-hc-fw-rc " md-icon-name " " class)}
                                (when style {:style style})
                                (when on-click {:on-click (handler-fn (on-click))})
                                (when (or tooltip on-mouse-over on-mouse-out)
                                  {:on-mouse-over (handler-fn
                                                    (when tooltip
                                                      (reset! showing? true))
                                                    (when on-mouse-over
                                                      (on-mouse-over)))
                                   :on-mouse-out (handler-fn
                                                   (when tooltip
                                                     (reset! showing? false))
                                                   (when on-mouse-out
                                                     (on-mouse-out)))}))]]
        (if tooltip
          [popover-tooltip
           :label tooltip
           :position (if tooltip-position tooltip-position :below-center)
           :showing? showing?
           :anchor the-icon]
          the-icon)))))

(defn persimmon-logo
  [args]
  (let [showing? (reagent/atom false)]
    (fn [{:keys [width height] :as args}]
      [popover-tooltip
       :label [:span "Fuyu persimmon "
               [:span {:style {:font-style "italic"}} "(Diospyros kaki)"]]
       :position :above-right
       :showing? showing?
       :anchor
       [:span.persimmon-logo
        {:style {:width (or width "50px")
                 :height (or height "50px")
                 :background-image "url(assets/persimmon.svg)"
                 :background-size "100% 100%"}
         :on-mouse-over (handler-fn (reset! showing? true))
         :on-mouse-out (handler-fn (reset! showing? false))}]])))
