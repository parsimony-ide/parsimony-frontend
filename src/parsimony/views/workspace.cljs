(ns parsimony.views.workspace
  (:require [cljs.test :refer-macros [is]]
            [clojure.set :as set]
            [parsimony.com.box :refer [draggy-h-box]]
            [parsimony.util :refer [matches-schema? third fourth vec-remove vec-insert get-drag-data get-all-drag-data get-drag-types set-drag-data set-drop-effect find-first-index]]
            [parsimony.views.file-picker :as file-picker]
            [parsimony.views.menubar :refer [menubar]]
            [parsimony.views.statusbar :refer [statusbar]]
            [parsimony.views.ribbon :refer [ribbon]]
            [parsimony.com.splits :refer [h-split v-split]]
            [re-com.core :refer [p button scroller box md-icon-button h-box v-box gap title popover-anchor-wrapper popover-content-wrapper] :refer-macros [handler-fn]]
            [re-com.util :refer [deref-or-value]]
            [re-com.validate :refer [css-style? html-attr?] :refer-macros [validate-args-macro]]
            [re-frame.core :refer [dispatch subscribe]]
            [reagent.core :as reagent]
            [reagent.ratom :refer [cursor] :refer-macros [reaction]]
            [schema.core :as s :include-macros true]
            [parsimony.console :as console]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drag-Drop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare card?)

(def fp-drag-type :parsimony.views.file-picker/element)

(defn valid-drop-fn
  [data i]
  (let [result
        (if-let [tab-data (get data ::tab)]
          (when-let [card (:card tab-data)]
            (card? (:card tab-data)))
          (when-let [fp-data (get data fp-drag-type)]
            true))]
    (when-not result
      (console/debug ::valid-drop-fn :invalid-drop {:data data :i i}))
    result))

(defn valid-drag-type-fn
  [drag-types i]
  (let [result (seq (set/intersection drag-types #{::tab fp-drag-type}))]
    (when-not result
      (console/debug ::valid-drag-type-fn :invalid-drag-type {:types drag-types :i i}))
    result))

(defn on-tabber-drop [workspace-id pile-id idx drag-data]
  (console/debug ::on-tabber-drop {:workspace-id workspace-id
                                   :pile-id pile-id
                                   :idx idx
                                   :drag-data drag-data})
  (if-let [card-info (get drag-data ::tab)]
    (dispatch [:card-move workspace-id pile-id idx card-info])
    (when-let [fp-data (get drag-data fp-drag-type)]
      (let [fp-elem (:data fp-data)]
        (when (satisfies? file-picker/IFileElement fp-elem)
          (dispatch [:tabber-fp-drop workspace-id pile-id idx (file-picker/backing-source-id fp-elem)]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Piles and Cards
;; - a pile is a place to put a card
;; - a card represents one visible element of the interface that should be
;;   freely movable between piles. Examples:
;;   - an editor could be a card, and a pile is a tab pane.
;;   - a log window is a card, and a pile is a minimization area
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ICard
  (card-key [this] "React key for this card")
  (human-label [this] "A human-readable Reagent component that describes the card")
  (draw-card [this] "This is a Reagent component function for drawing the underlying content behind the card")
  (confirm-close? [this] "Return true if the user should confirm before closing, otherwise false")
  (closeable? [this] "Return true if the user can close this card, otherwise false")
  (focusable? [this] "Return true if the user can focus this card")
  (disabled? [this] "Return true if this card is disabled, otherwise false"))

(defn card? [x]
  (satisfies? ICard x))

;; Rules:
;; - A card is not visible by itself. It only becomes visible when placed in a pile.
;; - A card has no notion of which pile it belongs to.
;; - Anything visible on screen belongs to a pile. Some piles are singleton: e.g.,the pile for the menubar.
;; - The pile determines whether or not sub-elements can be moved (e.g., within the pile, or to other piles.)
;; - The same card may potentially be displayed in multiple piles. In general, this is bad idea, so the handler
;;   logic that adds/removes/moves cards between piles should preserve the invariant of at most one pile per card
;; - A workspace is a statically determined collection of piles.  Piles cannot be created or removed dynamically.
;;   To create a new layout, you must create a new workspace and move all cards to the new workspace's piles. This
;;   greatly simplifies the logic for managing pile invariants.
;; - A pile is just an implementation detail.  Outside of this namespace, the only thing known about a pile is its id.

;; XXX: should a pile itself be a card and nestable in other piles? Or should a pile be a top-level entity in a
;;      workspace? do we actually need the expressive power of nested piles? (e.g., what if we want tabs that represent
;;      entire workspaces?)

(def tab-args-desc
  [{:name :pile-id    :required true  :type "integer"      :validate-fn number?}
   {:name :index      :required true  :type "integer"      :validate-fn number?}
   {:name :card       :required true  :type "atom | card"}
   {:name :selected?  :required false :type "boolean"}
   {:name :focused?   :required false :type "boolean"}
   {:name :on-click   :required false :type "-> nil"       :validate-fn fn?}
   {:name :on-close   :required false :type "-> nil"       :validate-fn fn?}
   {:name :on-discard :required false :type "-> nil"       :validate-fn fn?}])

;; XXX: it's unpleasant that the tab needs to be given both the pile-id and idx, but
;;      the on-drag-start handler needs them so that the drop target knows where the card came from
(defn tab [args]
  {:pre [(validate-args-macro tab-args-desc args "tab")]}
  (let [hover (reagent/atom nil)
        popover-showing? (reagent/atom nil)]
    (fn [{:keys [pile-id index card selected? focused? on-click on-close on-discard] :as args}]
      (let [card (deref-or-value card)]
        [h-box
         :size "none"
         ;; :min-width "90px"
         :class (str "tab "
                     (if selected? "selected " "")
                     (if focused? "focused " "")
                     (if @hover "hover " "")
                     (if (disabled? card) "disableme " ""))
         :attr {:draggable true
                :on-mouse-enter (handler-fn
                                 (reset! hover true))
                :on-mouse-leave (handler-fn
                                 (reset! hover nil))
                :on-drag-enter (handler-fn
                                 ;; prevent tabs from interfering with drag interaction
                                 (.preventDefault event)
                                 (.stopPropagation event))
                :on-drag-leave (handler-fn
                                 ;; prevent tabs from interfering with drag interaction
                                 (.preventDefault event)
                                 (.stopPropagation event))
                :on-drag-start (handler-fn
                                (doto event
                                  ;; in addition to the card, we put the pile-id and idx so that the drop target knows where this card came from
                                  (set-drag-data ::tab
                                                 {:card card
                                                  :pile-id pile-id
                                                  :idx index})))}
         :gap "5px"
         :align :center
         :children [(when (closeable? card)
                      [popover-anchor-wrapper
                       :showing? popover-showing?
                       :position :below-center
                       :anchor [md-icon-button
                                :size :smaller
                                :md-icon-name "zmdi-close"
                                :tooltip "Close"
                                :tooltip-position :below-center
                                :on-click (fn []
                                            (if (confirm-close? card)
                                              (reset! popover-showing? true)
                                              (on-close)))]
                       :popover
                       [popover-content-wrapper
                        :showing? popover-showing?
                        :position :below-center
                        :on-cancel #(reset! popover-showing? false)
                        :no-clip? true
                        :width "300px"
                        :body
                        [v-box
                         :size "auto"
                         :children [[p "Save before closing?"]
                                    [gap :size "5px"]
                                    [h-box
                                     :gap "5px"
                                     :children [[button
                                                 :label "Yes"
                                                 :class "btn btn-primary"
                                                 :on-click on-close]
                                                [button
                                                 :label "Discard Changes"
                                                 :class "btn btn-danger"
                                                 :on-click on-discard]
                                                [button
                                                 :label "Cancel"
                                                 :class "btn btn-default"
                                                 :on-click #(reset! popover-showing? false)]]]]]]])
                    [box
                     :style {:overflow "hidden"}
                     :size "auto"
                     ;; :min-width "50px"
                     :attr (when on-click
                             {:on-mouse-up
                              (handler-fn
                               (on-click))})
                     :child [human-label card]]]]))))

(def tabber-args-desc
  [{:name :id :require true :type "integer" :validate-fn number?}
   {:name :pile :require true :type "atom | pile"}
   {:name :on-click :require false :type "idx -> nil" :validate-fn fn?}
   {:name :on-close :require false :type "idx -> nil" :validate-fn fn?}
   {:name :on-discard :require false :type "idx -> nil" :validate-fn fn?}
   {:name :on-drop  :require false :type "idx -> drag-data -> nil" :validate-fn fn?}])

(defn- tabber-parent [args]
  (let [hover (reagent/atom nil)]
    (fn [{:keys [on-drop child]}]
      [h-box
       :class (str "tabber "
                   (if-not (some? child) "empty " "")
                   (if @hover "hover" ""))
       :min-height "20px"
       :attr {:on-drag-over
              (handler-fn
               (.preventDefault event)
               (.stopPropagation event)
               (set-drop-effect event :move))
              :on-drop
              (handler-fn
               (.preventDefault event)
               (.stopPropagation event)
               (reset! hover nil)
               (let [data (get-all-drag-data event)]
                 (when (and (valid-drag-type-fn (get-drag-types event) -1)
                            (valid-drop-fn data -1))
                   (on-drop -1 data))))
              :on-drag-enter
              (handler-fn
                (when (valid-drag-type-fn (get-drag-types event) -1)
                  (set-drop-effect event :move)
                  (reset! hover true)))
              :on-drag-leave
              (handler-fn
                (reset! hover nil))}
       :children
       (if (some? child)
         [child]
         [])])))

;; TODO: ensure the active tab is visible when first updated
(defn tabber [args]
  {:pre [(validate-args-macro tabber-args-desc args "tabber")]}
  (let [current-card-key (subscribe [:current-card])]
    (fn [{:keys [id pile on-click on-close on-discard on-drop] :as args}]
      (let [{:keys [available active]} @pile]
        (if (seq available)
          ;; there are available tabs, so draw the normal tab bar
          [scroller
           :class "tabber-scroller"
           :size "none"
           :h-scroll :on
           :v-scroll :off
           :child
           [tabber-parent
            {:on-drop on-drop
             :child
             [draggy-h-box
              {:class "tabber-drag-box"
               :draggy true
               :halo-before "0px"
               :halo-after "60px"
               :halo-grow? true
               :final-gap? false
               :gap-color "rgba(0,0,0,0.1)"
               ;; :halo-color "rgba(0,0,0,0.1)"
               :min-height "20px" ;; specify a min-height so that even when there are no tabs, this is visible
               :valid-drop-fn valid-drop-fn
               :valid-drag-type-fn valid-drag-type-fn
               :on-gap-drop (when on-drop
                              (fn [data idx]
                                (on-drop idx data)))
               :attr {:on-drag-over (handler-fn
                                      ;; prevent event from propagating to tabber-parent
                                      (.preventDefault event)
                                      (.stopPropagation event)
                                      (set-drop-effect event :none))
                      :on-drop (handler-fn
                                 ;; prevent event from propagating to tabber-parent
                                 (.preventDefault event)
                                 (.stopPropagation event))}
               :gap "6px"
               :children (doall
                           (for [[idx card] (map-indexed vector available)]
                             ^{:key (card-key card)}
                             [tab
                              {:pile-id id
                               :index idx
                               :card card
                               :selected? (= idx active)
                               :focused? (= (card-key card) @current-card-key)
                               :on-click (when on-click
                                           #(on-click idx))
                               :on-close (when on-close
                                           #(on-close idx))
                               :on-discard (when on-discard
                                             #(on-discard idx))}]))}]}]]
          ;; there are no available tabs, so draw a big drag target for the first tab
          [tabber-parent {:on-drop on-drop}]
          )))))

;; TODO: when tabber-content re-renders, give it focus
(defn tabber-content [{:keys [pile on-click on-drop] :as args}]
  (if-let [{:keys [available active]} @pile]
    (let [curr-card (nth available active)]
      #_(console/debug ::tabber-content :rerender {:active active :curr-card curr-card})
      #_[:code (pr-str curr-card)]
      ;; instead of unmounting content on tab switch, we use :display :none to prevent losing card state (e.g., codemirror internal state)
      ;; note that this alone is not enough to prevent state loss, since actions like moving an editor tab to another pile can still cause unmount/remount
      [v-box
       :size "auto"
       :children
       (doall
        (for [[idx card] (map-indexed vector available)]
          ^{:key (card-key card)}
          [box
           :class (str "tab-content "
                       (if (disabled? card)
                         "disableme " ""))
           :size "auto"
           :style (when-not (= idx active) {:display "none"})
           :attr {:on-click-capture (when on-click
                                      (handler-fn
                                        (on-click idx)))}
           :child
           (if-not (disabled? card)
             [draw-card card]
             [:div])]))])
    [v-box
     :class "placeholder"
     :size "auto"
     :attr {:on-drag-over
            (handler-fn
              (.preventDefault event)
              (.stopPropagation event)
              (if (valid-drag-type-fn (get-drag-types event) -1)
                (set-drop-effect event :copy)
                (set-drop-effect event :none)))
            :on-drop
            (handler-fn
              (.preventDefault event)
              (.stopPropagation event)
              (let [data (get-all-drag-data event)]
                (when (and (valid-drag-type-fn (get-drag-types event) -1)
                           (valid-drop-fn data -1))
                  (on-drop -1 data))))}
     :children []]))

;; they're the same args
(def tabbed-pile-args-desc tabber-args-desc)

;; manages the tabs and display/hide of individual cards
(defn tabbed-pile [{:keys [pile] :as args}]
  {:pre [(validate-args-macro tabbed-pile-args-desc args "tabbed-pile")]}
  [v-box
   :class (str "tabbed-pile " (if-not (seq (:available @pile)) "empty" ""))
   :size "auto"
   :children [[tabber args]
              [tabber-content args]]])

;; a pile whose content cannot be changed
(defn static-pile [args])

;; a pile for representing minimized items
(defn min-pile [args])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Layouts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_(defn min-panel [])

(declare pile-kws)

(def pile-site-args-desc
  [{:name :pile-id :require true :type "num" :validate-fn number?}
   {:name :pile-defs :require true :type "map of pile definitions"}
   {:name :cards :require true :type "atom with contents consistent with :pile schema"}
   {:name :on-card-click :require false :type "idx -> nil" :validate-fn fn?}
   {:name :on-card-close :require false :type "idx -> nil" :validate-fn fn?}
   {:name :on-card-discard :require false :type "idx -> nil" :validate-fn fn?}
   {:name :on-tabber-drop  :require false :type "idx -> drag-data -> nil" :validate-fn fn?}])

(defn- pile-site- [{:keys [pile-id pile-defs cards on-card-click on-card-close on-card-discard on-tabber-drop] :as args}]
  {:pre [(validate-args-macro pile-site-args-desc args "pile-site")]}
  (if-let [pile-kw (get pile-defs pile-id)]
    [v-box
     :size "auto"
     :children [#_[title :label (str "pile-" pile-id)]
                #_[:code (pr-str pile-kw)]
                (case pile-kw
                  :tabbed [tabbed-pile
                           {:id pile-id
                            :pile (cursor cards [pile-id])
                            :on-click (when on-card-click on-card-click)
                            :on-close (when on-card-close on-card-close)
                            :on-discard (when on-card-discard on-card-discard)
                            :on-drop (when on-tabber-drop on-tabber-drop)}]
                  ;; implement other kinds of piles here
                  )]]
    [:div (str "no pile-def found for pile-id " (pr-str pile-id))]))

(def ^:private -layout-args-desc
  [{:name :layout-args :require true :type "any"}
   {:name :cards :require true :type "atom with contents consistent with :pile schema"}
   {:name :on-card-click :require false :type "pile-id -> idx -> nil" :validate-fn fn?}
   {:name :on-card-close :require false :type "pile-id -> idx -> nil" :validate-fn fn?}
   {:name :on-card-discard :require false :type "pile-id -> idx -> nil" :validate-fn fn?}
   {:name :on-tabber-drop  :require false :type "pile-id -> idx -> drag-data -> nil" :validate-fn fn?}
   {:name :on-layout-change :require false :type "keyword -> any -> nil" :validate-fn fn?}])

(defn- layout-0 [{:keys [layout-args cards on-card-click on-card-close on-card-discard on-tabber-drop] :as args}]
  {:pre [(validate-args-macro -layout-args-desc args "layout-0")]}
  [box
   :size "auto"
   :child [pile-site-
           {:pile-id 1
            :pile-defs (:piles layout-args)
            :cards cards
            :on-card-click (when on-card-click (partial on-card-click 1))
            :on-card-close (when on-card-close (partial on-card-close 1))
            :on-tabber-drop (when on-tabber-drop (partial on-tabber-drop 1))}]])

(defn- layout-1 [{:keys [layout-args cards on-card-click on-card-close on-card-discard on-tabber-drop on-layout-change] :as args}]
  {:pre [(validate-args-macro -layout-args-desc args "layout-1")]}
  (let [{pile-defs :piles :keys [left top]} layout-args
        pile (fn [pile-id] [pile-site-
                            {:pile-id pile-id
                             :pile-defs pile-defs
                             :cards cards
                             :on-card-click (when on-card-click (partial on-card-click pile-id))
                             :on-card-close (when on-card-close (partial on-card-close pile-id))
                             :on-card-discard (when on-card-discard (partial on-card-discard pile-id))
                             :on-tabber-drop (when on-tabber-drop (partial on-tabber-drop pile-id))}])]
    [h-split
     :margin "0px"
     :on-split-change #(on-layout-change :left %)
     :initial-split left
     :panel-1 [box
               :size "auto"
               :child (pile 1)]
     :panel-2 [v-split
               :margin "0px"
               :on-split-change #(on-layout-change :top %)
               :initial-split top
               :panel-1 [box
                         :size "auto"
                         :child (pile 2)]
               :panel-2 [box
                         :size "auto"
                         :child (pile 3)]]]))

(defn- layout-2 [{:keys [layout-args cards on-card-click on-card-close on-card-discard on-tabber-drop on-layout-change] :as args}]
  {:pre [(validate-args-macro -layout-args-desc args "layout-2")]}
  (let [{pile-defs :piles :keys [left-1 left-2 top]} layout-args
        pile (fn [pile-id] [pile-site-
                            {:pile-id pile-id
                             :pile-defs pile-defs
                             :cards cards
                             :on-card-click (when on-card-click (partial on-card-click pile-id))
                             :on-card-close (when on-card-close (partial on-card-close pile-id))
                             :on-card-discard (when on-card-discard (partial on-card-discard pile-id))
                             :on-tabber-drop (when on-tabber-drop (partial on-tabber-drop pile-id))}])]
    [h-split
     :margin "0px"
     :on-split-change #(on-layout-change :left-1 %)
     :initial-split left-1
     :panel-1 [box
               :size "auto"
               :child (pile 1)]
     :panel-2 [v-split
               :margin "0px"
               :on-split-change #(on-layout-change :top %)
               :initial-split top
               :panel-1 [h-split
                         :margin "0px"
                         :on-split-change #(on-layout-change :left-2 %)
                         :initial-split left-2
                         :panel-1 [box
                                   :size "auto"
                                   :child (pile 2)]
                         :panel-2 [box
                                   :size "auto"
                                   :child (pile 3)]]
               :panel-2 [box
                         :size "auto"
                         :child (pile 4)]]]))

(defn- layout-3 [{:keys [layout-args cards on-card-click on-card-close on-card-discard on-tabber-drop on-layout-change] :as args}]
  {:pre [(validate-args-macro -layout-args-desc args "layout-3")]}
  (let [{pile-defs :piles :keys [left-1 left-2 left-3 top]} layout-args
        pile (fn [pile-id] [pile-site-
                            {:pile-id pile-id
                             :pile-defs pile-defs
                             :cards cards
                             :on-card-click (when on-card-click (partial on-card-click pile-id))
                             :on-card-close (when on-card-close (partial on-card-close pile-id))
                             :on-card-discard (when on-card-discard (partial on-card-discard pile-id))
                             :on-tabber-drop (when on-tabber-drop (partial on-tabber-drop pile-id))}])]
    [h-split
     :margin "0px"
     :on-split-change #(on-layout-change :left-3 %)
     :initial-split left-3
     :panel-1 [h-split
               :margin "0px"
               :on-split-change #(on-layout-change :left-1 %)
               :initial-split left-1
               :panel-1 [box
                         :size "auto"
                         :child (pile 1)]
               :panel-2 [v-split
                         :margin "0px"
                         :on-split-change #(on-layout-change :top %)
                         :initial-split top
                         :panel-1 [h-split
                                   :margin "0px"
                                   :on-split-change #(on-layout-change :left-2 %)
                                   :initial-split left-2
                                   :panel-1 [box
                                             :size "auto"
                                             :child (pile 2)]
                                   :panel-2 [box
                                             :size "auto"
                                             :child (pile 3)]]
                         :panel-2 [box
                                   :size "auto"
                                   :child (pile 4)]]]
     :panel-2 [box
               :size "auto"
               :child (pile 5)]]))

(defn- layout-4 [{:keys [layout-args cards on-card-click on-card-close on-card-discard on-tabber-drop on-layout-change] :as args}]
  {:pre [(validate-args-macro -layout-args-desc args "layout-4")]}
  (let [{pile-defs :piles :keys [left-1 left-2 left-3 top right-top]} layout-args
        pile (fn [pile-id] [pile-site-
                            {:pile-id pile-id
                             :pile-defs pile-defs
                             :cards cards
                             :on-card-click (when on-card-click (partial on-card-click pile-id))
                             :on-card-close (when on-card-close (partial on-card-close pile-id))
                             :on-card-discard (when on-card-discard (partial on-card-discard pile-id))
                             :on-tabber-drop (when on-tabber-drop (partial on-tabber-drop pile-id))}])]
    [h-split
     :margin "0px"
     :on-split-change #(on-layout-change :left-3 %)
     :initial-split left-3
     :panel-1 [h-split
               :margin "0px"
               :on-split-change #(on-layout-change :left-1 %)
               :initial-split left-1
               :panel-1 [box
                         :size "auto"
                         :child (pile 1)]
               :panel-2 [v-split
                         :margin "0px"
                         :on-split-change #(on-layout-change :top %)
                         :initial-split top
                         :panel-1 [h-split
                                   :margin "0px"
                                   :on-split-change #(on-layout-change :left-2 %)
                                   :initial-split left-2
                                   :panel-1 [box
                                             :size "auto"
                                             :child (pile 2)]
                                   :panel-2 [box
                                             :size "auto"
                                             :child (pile 3)]]
                         :panel-2 [box
                                   :size "auto"
                                   :child (pile 4)]]]
     :panel-2 [v-split
               :margin "0px"
               :on-split-change #(on-layout-change :right-top %)
               :initial-split right-top
               :panel-1 [box
                         :size "auto"
                         :child (pile 5)]
               :panel-2 [box
                         :size "auto"
                         :child (pile 6)]]]))

(def ^:private pile-kws
  (sorted-set :tabbed))

(def ^:private pile-def-schema {:piles {s/Num (apply s/enum pile-kws)}})

(def layouts
  (merge
    {:layout-0 {:component layout-0
                :schema pile-def-schema
                :defaults {:piles {1 :tabbed}}
                :primary-pile 1}}
    {:layout-1 {:component layout-1
                :schema (merge {:left s/Num
                                :top s/Num}
                               pile-def-schema)
                :defaults (sorted-map
                            :left 20
                            :top 80
                            :piles {1 :tabbed
                                    2 :tabbed
                                    3 :tabbed})
                :primary-pile 2}}
    {:layout-2 {:component layout-2
                :schema (merge {:left-1 s/Num
                                :left-2 s/Num
                                :top s/Num}
                               pile-def-schema)
                :defaults (sorted-map
                            :left-1 15
                            :left-2 50
                            :top 60
                            :piles {1 :tabbed
                                    2 :tabbed
                                    3 :tabbed
                                    4 :tabbed})
                :primary-pile 2
                :secondary-pile 3}}
    {:layout-3 {:component layout-3
                :schema (merge {:left-1 s/Num
                                :left-2 s/Num
                                :left-3 s/Num
                                :top s/Num}
                               pile-def-schema)
                :defaults (sorted-map
                            :left-1 20
                            :left-2 50
                            :left-3 80
                            :top 80
                            :piles {1 :tabbed
                                    2 :tabbed
                                    3 :tabbed
                                    4 :tabbed
                                    5 :tabbed})
                :primary-pile 2
                :secondary-pile 3}}
    {:layout-4 {:component layout-4
                :schema (merge {:left-1 s/Num
                                :left-2 s/Num
                                :left-3 s/Num
                                :top s/Num
                                :right-top s/Num}
                               pile-def-schema)
                :defaults (sorted-map
                            :left-1 15
                            :left-2 50
                            :left-3 85
                            :top 50
                            :right-top 50
                            :piles {1 :tabbed
                                    2 :tabbed
                                    3 :tabbed
                                    4 :tabbed
                                    5 :tabbed
                                    6 :tabbed})
                :primary-pile 2
                :secondary-pile 3}}))

(def layout-kws (into (sorted-set)
                      (map first)
                      layouts))

(defn- layout-attr [layout-kw attr-path]
  {:pre [(contains? layout-kws layout-kw)]}
  (get-in layouts (into [layout-kw] attr-path)))

(defn layout-component [layout-kw]
  (layout-attr layout-kw [:component]))

(defn layout-schema [layout-kw]
  (layout-attr layout-kw [:schema]))

(defn layout-defaults [layout-kw]
  (layout-attr layout-kw [:defaults]))

(defn layout-primary-pile [layout-kw]
  (layout-attr layout-kw [:primary-pile]))

(def layout-args-desc
  [{:name :cards :require true :type "atom with contents consistent with pile schema"}
   {:name :layout-kw :require true :type "layout-kw" :validate-fn #(contains? layout-kws %)}
   {:name :layout-args :require true :type "layout-args"}
   {:name :on-card-click :require false :type "pile-id -> idx -> nil" :validate-fn fn?}
   {:name :on-card-close :require false :type "pile-id -> idx -> nil" :validate-fn fn?}
   {:name :on-card-discard :require false :type "pile-id -> idx -> nil" :validate-fn fn?}
   {:name :on-tabber-drop  :require false :type "pile-id -> idx -> drag-data -> nil" :validate-fn fn?}
   {:name :on-layout-change :require false :type "keyword -> any -> nil"}])

(defn layout [{:keys [layout-kw layout-args cards on-card-click on-card-close on-card-discard on-tabber-drop on-layout-change] :as args}]
  {:pre [(validate-args-macro layout-args-desc args "layout")
         (contains? layout-kws layout-kw)
         (matches-schema? (layout-schema layout-kw) layout-args)]}
  [box
   :class "layout"
   :size "auto"
   :child
   [(layout-component layout-kw)
    {:layout-args layout-args
     :cards cards
     :on-card-click on-card-click
     :on-card-close on-card-close
     :on-card-discard on-card-discard
     :on-tabber-drop on-tabber-drop
     :on-layout-change on-layout-change}]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workspaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; workspace schema
(def schema {:id s/Num
             :layout-kw (apply s/enum layout-kws)
             :layout-args s/Any
             :cards {s/Num {:available [(s/protocol ICard)]
                            :active s/Num}}})

(def -workspace-args-desc
  [{:name :model :require true :type "atom with workspace-schema instance"}
   {:name :on-card-click :require false :type "pile-id -> idx -> nil" :validate-fn fn?}
   {:name :on-card-close :require false :type "pile-id -> idx -> nil" :validate-fn fn?}
   {:name :on-card-discard :require false :type "pile-id -> idx -> nil" :validate-fn fn?}
   {:name :on-tabber-drop  :require false :type "pile-id -> idx -> drag-data -> nil" :validate-fn fn?}
   {:name :on-layout-change :require false :type "layout-kw -> any -> nil" :validate-fn fn?}])

(defn- -workspace
  "Internal implementation of workspace, not visible and should not be used outside this namespace."
  [{:keys [model] :as args}]
  {:pre [(validate-args-macro -workspace-args-desc args "-workspace")
         (is (matches-schema? schema @model)
             (str "Workspace model does not match schema: " (pr-str @model)))]}
  (let [cards (reaction (:cards @model))] ;; this is where the workspace-model gets linked to the view
    (fn [{:keys [model on-card-click on-card-close on-card-discard on-tabber-drop on-layout-change] :as args}]
      (let [{:keys [id layout-kw layout-args]} @model]
        [v-box
         :class "workspace"
         :size "auto"
         :children [[menubar]
                    [gap :size "10px"]
                    [layout
                     {:cards cards
                      :layout-kw layout-kw
                      :layout-args layout-args
                      :on-card-click on-card-click
                      :on-card-close on-card-close
                      :on-card-discard on-card-discard
                      :on-tabber-drop on-tabber-drop
                      :on-layout-change (when on-layout-change
                                          (fn [kw new-val]
                                            (on-layout-change layout-kw (assoc layout-args kw new-val))))}]
                    [statusbar]]]))))

(defn workspace [workspace-id]
  {:pre [(number? workspace-id)]}
  (let [model (subscribe [:workspace-with-id workspace-id])]
    (if-not (some? @model)
      [:div "Initializing..."]
      [-workspace
       {:model model
        :on-card-click #(dispatch [:card-click workspace-id %1 %2])
        :on-card-close #(dispatch [:card-close workspace-id %1 %2])
        :on-card-discard #(dispatch [:card-discard workspace-id %1 %2])
        :on-tabber-drop (partial on-tabber-drop workspace-id)
        :on-layout-change #(dispatch [:change-workspace-layout workspace-id %1 %2])}])))

(defn init-model
  "Create an empty initial workspace model"
  [id layout-kw]
  {:pre [(contains? layout-kws layout-kw)]
   :post [(matches-schema? schema %)]}
  (sorted-map
   :id id
   :layout-kw layout-kw
   :layout-args (layout-defaults layout-kw)
   :cards (sorted-map)))

(defn num-available
  "Return the number of available cards in the given pile"
  [workspace-model pile-id]
  (count (get-in workspace-model [:cards pile-id :available])))

(defn get-card
  "Return the card at the given pile index. If idx is less than 0, then
   return last card in pile. If no card is found, return nil"
  [workspace-model pile-id idx]
  (let [idx (if (< idx 0)
              (dec (num-available workspace-model pile-id))
              idx)]
    (get-in workspace-model [:cards pile-id :available idx])))

(defn active-cards
  "Return lazy seq of active cards"
  [workspace-model]
  (for [[_ pile] (:cards workspace-model)
        :let [active-id (:active pile)
              active-card (get (:available pile) active-id)]
        :when (some? active-card)]
    active-card))

(defn add-card
  "Add a card to the specified pile. If idx is less than 0, then add to end of
   pile.  If (:background options), then do not activate the card unless there
   is no currently active card."
  [workspace-model pile-id card idx & {:as options}]
  (console/debug ::add-card {;; :workspace-model workspace-model
                             :pile-id pile-id
                             :card card
                             :idx idx
                             :options options})
  (let [num-available (count (get-in workspace-model [:cards pile-id :available]))
        idx (if (< idx 0)
              num-available
              (min idx num-available))
        ;; add card to the pile
        workspace-model (update-in workspace-model [:cards pile-id :available] (fnil vec-insert []) idx card)]
    (if (or (not (get-in workspace-model [:cards pile-id :active]))
            (not (:background options)))
      ;; activate card if there is no current active card in this pile, otherwise only when :background option is not specified
      (assoc-in workspace-model [:cards pile-id :active] idx)
      workspace-model)))

(defn remove-card [workspace-model pile-id idx]
  (if-let [available (get-in workspace-model [:cards pile-id :available])]
    (let [num-available (count available)
          idx (if (< idx 0)
                (dec num-available)
                (min idx (dec num-available)))
          curr-active (get-in workspace-model [:cards pile-id :active])
          next-active (cond
                        (nil? curr-active) 0
                        (and (> curr-active 0)
                             (<= idx curr-active)) (dec curr-active)
                        :else curr-active)]
      (if (> (count available) 1)
        ;; there will still be other cards in this pile, so just change the :active card
        (do
          (console/debug ::remove-card {:num-available (count available)
                                        :pile-id pile-id
                                        :idx idx})
          (-> workspace-model
              (update-in [:cards pile-id :available] vec-remove idx)
              (assoc-in [:cards pile-id :active] next-active)))
        ;; there are no cards left in this pile, so remove the pile from :cards
        (do
          (console/debug ::remove-card :empty-pile {:pile-id pile-id :idx idx})
          (update-in workspace-model [:cards] dissoc pile-id))))
    (do
      (console/debug ::remove-card :no-op)
      workspace-model)))

(defn card-locs
  "Return map from card to {:pile-id :idx}"
  [workspace-model]
  (into {}
        (for [[pile-id pile] (:cards workspace-model)
              [idx card] (map-indexed vector (:available pile))]
          [card {:pile-id pile-id :idx idx}])))

(defn card-key->card-loc
  "Return the loc for the card with the given key, if it exists"
  [workspace-model key]
  (first
    (for [[card {:keys [pile-id idx]}]
          (card-locs workspace-model)
          :when (= key (card-key card))]
      {:pile-id pile-id :idx idx})))

(defn card-key->card
  "Return the card with the given key, if it exists"
  [workspace-model key]
  (when-let [{:keys [pile-id idx]} (card-key->card-loc workspace-model key)]
    (get-card workspace-model pile-id idx)))

(defn remove-cards-with-matching-key
  "Remove any card with a card-key matching the given regular expression"
  [workspace-model re]
  (loop [workspace-model workspace-model]
    (if-let [[card {:keys [pile-id idx]}]
             (first (filter (fn [[card _]]
                              (re-seq re (card-key card)))
                            (card-locs workspace-model)))]
      (do (console/debug ::remove-cards-with-matching-key {:card-key (card-key card)
                                                           :pile-id pile-id
                                                           :idx idx})
          (recur (remove-card workspace-model pile-id idx)))
      workspace-model)))

;; Idea: transform indices like so
;; - map gaps to odd indices
;; - map existing card indices to even indices
;; - insert using transformed indices
;; - then normalize indices to original basis
;; - This is essentially the same idea used for moving tree-view items
(defn- move-card-within-pile [workspace-model pile-id from-idx to-idx card]
  (console/debug ::move-card-within-pile {:from-idx from-idx :to-idx to-idx})
  (let [available (get-in workspace-model [:cards pile-id :available])
        indexed-available (map (fn [n a] [(inc (* 2 n)) a]) (range) available)
        num-available (count available)
        to-idx (if (< to-idx 0)
                 num-available
                 (min to-idx num-available))
        from-idx (if (< from-idx 0)
                   (dec num-available)
                   (min from-idx (dec num-available)))
        new-from-idx (inc (* 2 from-idx))
        new-to-idx (* 2 to-idx)
        new-available (->> (conj indexed-available [new-to-idx card])
                           (remove #(= (first %) new-from-idx))
                           (sort-by first)
                           (map second)
                           (vec))
        new-card-idx (find-first-index #(= % card) new-available)
        curr-active (get-in workspace-model [:cards pile-id :active])
        override-active (fn [workspace-model]
                          ;; "active" status moves with card
                          (if (= curr-active from-idx)
                            (assoc-in workspace-model [:cards pile-id :active] new-card-idx)
                            workspace-model))]
    (-> workspace-model
        (assoc-in [:cards pile-id :available] new-available)
        (override-active))))

(defn activate-card [workspace-model pile-id idx]
  (let [idx (if (< idx 0)
              (dec (num-available workspace-model pile-id))
              idx)]
    (assoc-in workspace-model [:cards pile-id :active] idx)))

(defn- move-card-between-piles [workspace-model from-pile-id from-idx to-pile-id to-idx card]
  (-> workspace-model
      (remove-card from-pile-id from-idx)
      (add-card to-pile-id card to-idx)
      ;; when moving card to new pile, the moved card should become active
      (activate-card to-pile-id to-idx)))

(defn move-card [workspace-model from-pile-id from-idx to-pile-id to-idx card]
  (if (= from-pile-id to-pile-id)
    (move-card-within-pile workspace-model from-pile-id from-idx to-idx card)
    (move-card-between-piles workspace-model from-pile-id from-idx to-pile-id to-idx card)))

(defn migrate-layouts
  "Convert workspace-model to a new layout"
  [workspace-model layout-kw]
  {:pre [(matches-schema? schema workspace-model)
         (contains? layout-kws layout-kw)]}
  ;; TODO: implement this
  (comment TBD))
