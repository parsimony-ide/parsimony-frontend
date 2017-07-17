(ns parsimony.views.live-parse-view
  (:require [parsimony.models.colors :as colors :refer [decoration->html-css]]
            [parsimony.models.parse-dashboard :as parse-dashboard]
            [parsimony.util :refer [pprint-str]]
            [parsimony.views.parse-forest :as parse-forest]
            [reagent.core :as reagent]
            [reagent.ratom :refer [make-reaction] :refer-macros [reaction]]
            [re-com.core :refer [scroller title h-box v-box] :refer-macros [handler-fn]]
            [re-com.validate :refer-macros [validate-args-macro]]
            [re-frame.core :refer [dispatch subscribe]]
            [parsimony.console :as console]
            #_[clairvoyant.core :refer-macros [trace-forms]]
            #_[re-frame-tracer.core :refer [tracer]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; live-parse-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -live-parse-view [args]
  (let [last-change-key (reagent/atom nil)]
    (reagent/create-class
      {:component-will-receive-props
       (fn [c [_ {:keys [active change-key] :as args}]]
         #_(console/debug :will-receive-props (pprint-str {:active active :change-key change-key}))
         (when (and active (not= @last-change-key change-key))
           #_(console/debug :live-parse-view :new-change-key)
           (reset! last-change-key change-key)
           (dispatch [:live-parse-view/refresh])))

       :should-component-update
       (fn [_ _ [_ new-args]]
         (:active new-args))

       :reagent-render
       (fn [{:keys [selected-overlay
                    parse-overlays
                    forest
                    string
                    tokens
                    decoration-map
                    disabled-overlays
                    peek
                    positive-labels
                    negative-labels
                    emphasis-subforest
                    on-click-positive
                    on-click-negative
                    on-click-remove]
             :as args}]
         #_(console/debug :live-parse-view :render)
         (if (seq @parse-overlays)
           ;; parse-overlays exist
           [v-box
            :class "live-parse-view"
            :size "auto"
            :children
            [(if (some? @selected-overlay)
               ;; selected-overlay exists
               [scroller
                :style {:padding "5px"}
                :child
                [parse-forest/parse-forest-view
                 {:forest @forest
                  :string @string
                  :tokens @tokens
                  :decoration-map @decoration-map
                  :disabled-overlays @disabled-overlays
                  :peek @peek
                  :positive-labels @positive-labels
                  :negative-labels @negative-labels
                  :emphasis-subforest @emphasis-subforest
                  :on-click-positive on-click-positive
                  :on-click-negative on-click-negative
                  :on-click-remove on-click-remove}]]
               ;; no selected-overlay
               [v-box
                :size "auto"
                :align :center
                :justify :center
                :children [[title
                            :label
                            [v-box
                             :align :center
                             :gap "10px"
                             :children
                             [[:span "There are multiple options available to visualize."]
                              [:span "Click an option shown at the bottom of the editor to visualize its parse."]]]
                            :level :level2
                            :style {:color "#d0d0d0"}]]])]]
           ;; no parse-overlays exist
           [v-box
            :class "live-parse-view inactive"
            :size "auto"
            :align :center
            :justify :center
            :children [[title
                        :label "Place the editor cursor in a colored region to show its parse."
                        :level :level2
                        :style {:color "#d0d0d0"}]]]))})))

(defn- -change-key [editor-id overlays-at-cursor]
  (let [ol-keys (set (for [{:keys [overlay-type overlay-tag]
                            [from to] :char-range}
                           overlays-at-cursor
                           :when (= :parse overlay-type)]
                       [overlay-type overlay-tag from to]))]
    [editor-id ol-keys]))

;; (trace-forms {:tracer (tracer :color "brown")}
(defn live-parse-view []
  (let [state (subscribe [:live-parse-view])
        editor-id (reaction (:editor-id @state))
        parse-overlays (reaction (:parse-overlays @state))
        token-overlays (reaction (:token-overlays @state))
        selected-overlay (reaction (:selected-overlay @state))
        forest (reaction (:forest @state))
        string (reaction (:string @state))
        tokens (reaction (:tokens @state))
        candidate-id (reaction (:candidate-id @state))
        disambiguations (reaction (:disambiguations @state))
        emphasis-subforest (reaction (get-in @disambiguations [@candidate-id :forest]))
        decoration-map (subscribe [:editor-decoration-map] [editor-id])
        disabled-overlays (subscribe [:disabled-overlays] [editor-id])
        peek (subscribe [:peek] [editor-id])
        sample-id (subscribe [:sample-for-editor] [editor-id])
        parse-dashboard (subscribe [:parse-dashboard])
        positive-labels (reaction
                          (when @sample-id
                            (into #{}
                                  (map (partial parse-dashboard/sample-label->token-indexed-label @tokens))
                                  (parse-dashboard/positive-labels-for @parse-dashboard @sample-id))))
        negative-labels (reaction
                          (when @sample-id
                            (into #{}
                                  (map (partial parse-dashboard/sample-label->token-indexed-label @tokens))
                                  (parse-dashboard/negative-labels-for @parse-dashboard @sample-id))))
        last-focused-sample-editor (subscribe [:last-focused-sample-editor])
        overlays-at-cursor (subscribe [:overlays-at-cursor] [last-focused-sample-editor])
        change-key (reaction (-change-key @last-focused-sample-editor @overlays-at-cursor))
        active (subscribe [:live-parse-view/active])]
    (fn []
      #_(console/debug (pprint-str (select-keys @state [:editor-id :selected-overlay])))
      [-live-parse-view {:selected-overlay selected-overlay
                         :parse-overlays parse-overlays
                         :forest forest
                         :string string
                         :tokens tokens
                         :decoration-map decoration-map
                         :disabled-overlays disabled-overlays
                         :peek peek
                         :positive-labels positive-labels
                         :negative-labels negative-labels
                         :emphasis-subforest emphasis-subforest
                         :on-click-positive #(dispatch [:live-parse-view/add-sample % false])
                         :on-click-negative #(dispatch [:live-parse-view/add-sample % true])
                         :on-click-remove #(dispatch [:live-parse-view/remove-sample @sample-id @tokens %])
                         :active @active
                         :change-key @change-key}])))

;; )
