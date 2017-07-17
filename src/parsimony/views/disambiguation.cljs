(ns parsimony.views.disambiguation
  (:require [parsimony.models.colors :as colors]
            [parsimony.parser :as parser]
            [parsimony.util :refer [pprint-str]]
            [parsimony.views.common :refer [icon]]
            [parsimony.views.parse-forest :as parse-forest]
            [re-com.core :refer [gap line scroller box v-box h-box label] :refer-macros [handler-fn]]
            [re-frame.core :refer [dispatch subscribe]]
            [reagent.core :as reagent]
            [reagent.ratom :as ratom :include-macros true :refer-macros [reaction]]
            [parsimony.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Elements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn fragment-nt-view [fragment]
  (let [decoration-map (subscribe [:affinity-decoration-map])
        decorations (subscribe [:decorations])]
    (fn [[nt _ :as fragment]]
      (let [decoration (parse-forest/label->decoration nt @decoration-map @decorations)
            css (colors/decoration->html-css decoration)]
        [v-box
         :class "fragment-nt-view"
         :style (select-keys css [:background])
         :children [[:pre  {:class "overlay-tag"}
                     (name (parser/->nonterminal nt))]]]))))

(defn fragment-piece-view [piece]
  (let [decoration-map (subscribe [:affinity-decoration-map])
        decorations (subscribe [:decorations])]
    (fn [piece]
      (let [decoration (parse-forest/label->decoration piece @decoration-map @decorations)
            decoration-mod (when (parser/terminal? piece)
                             colors/no-outline-mod)
            css (colors/decoration->html-css (colors/mix decoration decoration-mod))]
        [v-box
         :class "fragment-piece-view"
         :style css
         :children [[:pre {:class "string"} (name (parser/->nonterminal piece))]]]))))

(defn fragment-pieces-view [[_ pieces :as fragment]]
  [h-box
   :class "fragment-pieces-view"
   :gap "5px"
   :children (into []
                   (for [piece pieces]
                     [fragment-piece-view piece]))])

(defn close-button [hover on-close tooltip]
  [:div.close-button
   {:style {:display
            (if @hover "inline" "none")
            :position "absolute"
            :right "-8px"
            :top "-4px"}}
   [icon {:md-icon-name "zmdi-close-circle"
          :on-click on-close
          :tooltip tooltip}]])

(defn fragment-view [args]
  (let [decoration-map (subscribe [:affinity-decoration-map])
        decorations (subscribe [:decorations])
        hover (reagent/atom nil)]
    (fn [{[nt _ :as fragment] :fragment
          on-close :on-close
          close-tooltip :close-tooltip
          :as args}]
      (let [decoration (parse-forest/label->decoration nt @decoration-map @decorations)]
        [h-box
         :class "fragment-view"
         :attr {:on-mouse-enter #(reset! hover true)
                :on-mouse-leave #(reset! hover false)
                :on-click (handler-fn
                            (when on-close
                              (on-close)))}
         :style (merge
                  {:position "relative"}
                  (select-keys (colors/decoration->html-css decoration) [:border-color])
                  (when on-close
                    {:cursor "pointer"}))
         :align :stretch
         :gap "5px"
         :children
         [[fragment-nt-view fragment]
          [fragment-pieces-view fragment]
          (when on-close
            [close-button hover on-close close-tooltip])]]))))

(defn prod-group [-label prods]
  [v-box
   :class "prod-group"
   :gap "3px"
   :children
   [[h-box
     :gap "3px"
     :children [[:span -label] [:span "{"]]]
    [v-box
     :style {:margin-left "15px"}
     :gap "2px"
     :children
     (into []
           (map #(vector fragment-view {:fragment %}))
           prods)]
    [:span "}"]]])

(defn lass-view [lass]
  [v-box
   :class "lass-view"
   :children
   (into []
         (map (partial vector prod-group "left"))
         lass)])

(defn rass-view [rass]
  [v-box
   :class "rass-view"
   :children
   (into []
         (map (partial vector prod-group "right"))
         rass)])

(defn priority-view [{:keys [high low]}]
  [h-box
   :align :center
   :children [[fragment-view {:fragment high}]
              [icon {:md-icon-name "zmdi-chevron-right"}]
              [fragment-view {:fragment low}]]])

(defn prio-view [prio]
  (let [priority-group
        [v-box
         :style {:margin-left "15px"}
         :gap "3px"
         :children (into []
                         (map (partial vector priority-view))
                         prio)]]
    [v-box
     :class "prio-view"
     :gap "3px"
     :children [[:span "priorities {"]
                priority-group
                [:span "}"]]]))

(defn pref-prod [prod]
  [h-box
   :align :center
   :gap "5px"
   :children [[fragment-view {:fragment prod}]
              [:span "#{prefer}"]]])

(defn pref-view [pref]
  [v-box
   :class "pref-view"
   :gap "3px"
   :children (into []
                   (map (partial vector pref-prod))
                   pref)])

(defn disambiguation-candidate-view
  "View of a single disambiguation candidate"
  [{:keys [on-click on-double-click on-mouse-over on-mouse-out hovered selected]
    {:keys [lass rass prio pref]} :candidate
    :as args}]
  [h-box
   :children [(if (or (seq lass) (seq rass) (seq prio) (seq pref))
                [v-box
                 :class (str "disambiguation-candidate-view "
                             (when hovered "hoverme ")
                             (when selected "selectme "))
                 :align :start
                 :gap "5px"
                 :attr {:on-mouse-over (handler-fn (on-mouse-over))
                        :on-mouse-out (handler-fn (on-mouse-out))
                        :on-click (handler-fn (on-click))
                        :on-double-click (handler-fn (on-double-click))}
                 :children
                 [(when (seq lass) [lass-view lass])
                  (when (seq rass) [rass-view rass])
                  (when (seq prio) [prio-view prio])
                  (when (seq pref) [pref-view pref])]]
                [h-box
                 :align :center
                 :class "no-disambiguation-view"
                 :gap "5px"
                 :attr {:on-click (handler-fn (on-click))}
                 :children [[icon {:md-icon-name "zmdi-layers-off"}]
                            [label :label "Deselect all"]]])]])

(defn help-view [disambiguations]
  (let [show-help (reagent/atom true)
        hover (reagent/atom nil)]
    (fn [disambiguations]
      (let [help-label-1
            [label
             :class "help-message"
             :label [:span
                     [:span "Examine the example parses. If you see an
                             ambiguity, mark the node(s) that are correct, then
                             click the " [icon {:md-icon-name "zmdi-refresh"}] " button above. "]
                     [:span "Parsimony will then attempt to infer a set of
                             disambiguating filters based on the nodes you have marked."]]]
            help-label-2
            [label
             :class "help-message"
             :label [:span "Parsimony attempted to infer disambiguating filters, but found none.
                            Examine the example parses. If you see an
                            ambiguity, mark the node(s) that are correct, then
                            click the " [icon {:md-icon-name "zmdi-refresh"}] " button above to retry."]]
            help-label-3
            [label
             :class "help-message"
             :label [:span "Parsimony has inferred the disambiguating filters below.
                            To preview the effect of one of these candidates, click
                            on the candidate's box, and the corresponding parse nodes
                            and edges will be highlighted."]]]
        [v-box
         :class "help"
         :gap "5px"
         :children [[h-box
                     :class (str "toggle-button " (when @hover "hoverme"))
                     :align :center
                     :gap "5px"
                     :attr {:on-click (handler-fn
                                        (swap! show-help not))
                            :on-mouse-over (handler-fn
                                             (reset! hover true))
                            :on-mouse-out (handler-fn
                                            (reset! hover false))}
                     :children [[icon {:md-icon-name "zmdi-help"}]
                                (if @show-help
                                  [label :label "Click to hide help"]
                                  [label :label "Click to show help"])]]
                    (when (and @show-help
                               (not (seq disambiguations)))
                      help-label-1)
                    (when (and @show-help
                               (= (count disambiguations) 1))
                      help-label-2)
                    (when (and @show-help
                               (> (count disambiguations) 1))
                      help-label-3)]]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- solver-disambiguation-view []
  (let [state (subscribe [:solver/ux-state])
        candidates (reaction (get-in @state [:disambiguations :candidates]))
        chosen-candidate (reaction (get-in @state [:disambiguations :chosen-candidate]))
        hover-id (reagent/atom nil)]
    (fn []
      (if (seq @candidates)
        [scroller
         :class "disambiguation-view"
         :child [v-box
                 :size "auto"
                 :gap "5px"
                 :children
                 (-> []
                     (conj [help-view @candidates])
                     (conj [line :size "1px"])
                     (into (for [[candidate-id {:keys [candidate]}] @candidates]
                             [disambiguation-candidate-view
                              {:on-click #(dispatch [:solver/preview-disambiguation candidate-id])
                               :on-double-click #(console/debug :TBD)
                               :on-mouse-over #(reset! hover-id candidate-id)
                               :on-mouse-out #(reset! hover-id nil)
                               :hovered (= @hover-id candidate-id)
                               :selected (= @chosen-candidate candidate-id)
                               :candidate candidate}]))
                     (conj [gap :size "5px"]))]]
        [:div.disambiguation-view.empty]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Live Parse View
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- live-parse-view-disambiguation-view []
  (let [state (subscribe [:live-parse-view])
        disambiguations (reaction (:disambiguations @state))
        select-id (reaction (:candidate-id @state))
        hover-id (reagent/atom nil)]
    (fn []
      (if (seq @disambiguations)
        (let [disambiguations @disambiguations]
          [scroller
           :class "disambiguation-view"
           :child [v-box
                   :size "auto"
                   :gap "5px"
                   :children
                   (-> []
                       (conj [help-view disambiguations])
                       (conj [line :size "1px"])
                       (into (for [[candidate-id {:keys [candidate]}] disambiguations]
                               [disambiguation-candidate-view
                                {:on-click #(dispatch [:live-parse-view/preview-disambiguation candidate-id])
                                 :on-double-click #(dispatch [:live-parse-view/accept-disambiguation candidate-id])
                                 :on-mouse-over #(reset! hover-id candidate-id)
                                 :on-mouse-out #(reset! hover-id nil)
                                 :hovered (= @hover-id candidate-id)
                                 :selected (= @select-id candidate-id)
                                 :candidate candidate}]))
                       (conj [gap :size "5px"]))]])
        [:div.disambiguation-view.empty]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disambiguation View
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn disambiguation-view []
  (let [current-card (subscribe [:current-card])]
    (fn []
      (case @current-card
        "live-parse-view-card" [live-parse-view-disambiguation-view]
        "solver-card" [solver-disambiguation-view]
        ;; default
        [:div.disambiguation-view.empty]))))


