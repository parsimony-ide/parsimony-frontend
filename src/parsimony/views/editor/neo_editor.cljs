(ns parsimony.views.editor.neo-editor
  (:require [parsimony.com.neo-codemirror :as codemirror :refer [ICodeMirrorHandler]]
            [parsimony.models.colors :as colors]
            [parsimony.models.editor :as editor]
            [parsimony.models.overlay-state :as overlay-state]
            [parsimony.util :refer [non-input-keycode?]]
            [parsimony.views.neo-overlay :as neo-overlay]
            [reagent.core :as reagent]
            [reagent.ratom :refer [cursor] :refer-macros [reaction]]
            [re-com.core :refer [v-box]]
            [re-com.util :refer [deref-or-value]]
            [re-com.validate :refer-macros [validate-args-macro]]
            [re-frame.core :refer [dispatch subscribe]]
            [parsimony.console :as console]))

(defn all-overlays [{:keys [overlay-state
                            string
                            viewport-info
                            scroll-info]
                     :as args}]
  (let [decorations (subscribe [:decorations])
        normal-overlays (reaction (overlay-state/normal-overlays @overlay-state))
        error-overlays (reaction (overlay-state/error-overlays @overlay-state))
        disabled-overlays (reaction (overlay-state/disabled-overlays @overlay-state))
        peek (reaction (overlay-state/all-peek @overlay-state))
        char-origin-left (reaction (get-in @viewport-info [:char-coords :left] 0))
        char-origin-top (reaction (get-in @viewport-info [:char-coords :top] 0))
        char-width (reaction (get @viewport-info :char-width 0))
        line-height (reaction (get @viewport-info :line-height 0))
        gutter-width (reaction (get @viewport-info :gutter-width 0))
        scroll-origin-left (reaction (get @scroll-info :left 0))
        scroll-origin-top (reaction (get @scroll-info :top 0))]
    (letfn [(-overlays-view [overlay-view-fn overlays]
              [:div.overlays
               {:style {:left (- (+ @gutter-width @char-origin-left) @scroll-origin-left)
                        :top  (- @char-origin-top @scroll-origin-top)}}
               (doall
                 (for [[tag _] @overlays]
                   ^{:key tag}
                   [overlay-view-fn
                    {:overlay (cursor overlays [tag])
                     :string string
                     :char-width char-width
                     :line-height line-height
                     :decorations decorations
                     :disabled-overlays disabled-overlays
                     :peek peek}]))])
            (normal-overlays-view [overlays]
              (-overlays-view neo-overlay/dynamic-overlay-view overlays))
            (error-overlays-view [overlays]
              (-overlays-view neo-overlay/error-overlay-view overlays))
            (gutter-overlays-view [overlays]
              #_(console/debug ::gutter-overlays-view {:overlays overlays})
              [:div.gutter-overlays
               {:style {:left 0
                        :top (- @char-origin-top @scroll-origin-top)
                        :width (str @gutter-width "px")}}
               (doall
                 (for [[tag _] @overlays]
                   ^{:key tag}
                   [neo-overlay/gutter-overlay-view
                    {:overlay (cursor overlays [tag])
                     :string string
                     :char-width gutter-width
                     :line-height line-height
                     :decorations decorations
                     :disabled-overlays disabled-overlays
                     :peek peek}]))])]
      (fn [args]
        (-> [:div.all-overlays]
            ;; normal overlays
            (into (for [[overlay-type _] @normal-overlays]
                    ^{:key (str "normal-" (name overlay-type))}
                    [normal-overlays-view (cursor normal-overlays [overlay-type])]))
            ;; error overlays
            (into (for [[overlay-type _] @error-overlays]
                    ^{:key (str "error-" (name overlay-type))}
                    [error-overlays-view (cursor error-overlays [overlay-type])]))
            ;; gutter overlays
            (into (for [[overlay-type _] @error-overlays]
                    ^{:key (str "gutter-" (name overlay-type))}
                    [gutter-overlays-view (cursor error-overlays [overlay-type])])))))))

(defn line-col-indicator [cursor-info]
  (let [{:keys [line ch]} (:head @cursor-info)]
    (if (and line ch)
      [:div.line-col-indicator
       (str (inc line) ":" (inc ch))]
      [:span {:style {:display "none"}}])))

(defn gen-handler [editor string cm-atom viewport-info scroll-info]
  (let [editor-id (:id @editor)]
    (letfn [(-history-add [value cursor-info]
              (dispatch [:editor-history-add editor-id {:string value :cursor-info cursor-info}]))
            (-internal-change [value]
              (when-not (identical? @string value)
                (dispatch [:remove-all-overlays editor-id])
                (dispatch [:internal-editor-change editor-id value])
                true))
            (handle-internal-change [this cm value cursor-info update-history?]
              (when (and (-internal-change value)
                         update-history?)
                (-history-add value cursor-info)))]

      (reify ICodeMirrorHandler

        (on-initialize [_ cm]
          #_(console/debug ::on-initialize)
          (reset! cm-atom cm)
          (dispatch [:editor-init-codemirror editor-id cm]))

        (on-viewport-change [_ cm]
          #_(console/debug ::on-viewport-change)
          (reset! viewport-info (codemirror/get-viewport-info cm)))

        (on-scroll [_ cm]
          #_(console/debug ::on-scroll)
          (reset! scroll-info (codemirror/get-scroll-info cm)))

        (on-external-change [this cm value cursor-info update-history?]
          (dispatch [:external-editor-change editor-id value])
          (when-not (:read-only @editor)
            ;; XXX: Hack. The above check for :read-only blocks a race
            ;; condition where this handler fires after overlays have been
            ;; injected into a parse-dashboard sample editor on project load.
            ;; We prevent the dispatch to :remove-all-overlays to prevent the
            ;; newly added overlays from being cleared.  Preferably, we would
            ;; somehow guarantee that overlays are injected only after
            ;; codemirror has initialized.
            ;; Note that this check only works because parse-dashboard editors
            ;; are the only editors with :read-only true.
            (dispatch [:remove-all-overlays editor-id]))
          (when update-history?
            (-history-add value cursor-info)))

        (on-internal-change [this cm value cursor-info update-history?]
          (handle-internal-change this cm value cursor-info update-history?))

        (on-keydown [this cm event]
          #_(console/debug ::on-keydown)
          ;; XXX: kind of a hack. Forcing propagate on metaKey and altKey ensures that
          ;; changes are available for subsequent worker invoked by keyboard shortcut
          (when (and (.-metaKey event)
                     (.-altKey event))
            (handle-internal-change this cm (codemirror/get-value cm) (codemirror/get-cursor-info cm) true)))

        (on-keyup [_ _ _]
          #_(console/debug ::on-keyup)
          (when (and (not (identical? @string (codemirror/get-value @cm-atom)))
                     (seq (get-in @editor [:overlay-state :overlays])))
            (dispatch [:remove-all-overlays editor-id])))

        (on-cursor-activity [_ cm]
          #_(console/debug ::on-cursor-activity)
          (dispatch [:editor-cursor editor-id (codemirror/get-cursor-info cm)]))

        (on-blur [_ cm]
          #_(console/debug ::on-blur)
          (dispatch [:editor-blur editor-id]))

        (on-focus [_ cm]
          #_(console/debug ::on-focus)
          (dispatch [:editor-focus editor-id]))))))

(defn- indent-fn
  "Custom indentation function"
  [cm-instance]
  (if (.somethingSelected cm-instance)
    (.indentSelection cm-instance "add")
    (.replaceSelection cm-instance
                       (if (.getOption cm-instance "indentWithTabs")
                         "\t"
                         (apply str (repeat (or (.getOption cm-instance "indentUnit") 2) " "))))))

(def base-editor-args-desc
  [{:name :autofocus :required false :default true :type "boolean" :validate-fn boolean? :description "If true, then the editor will steal focus whenever it becomes visible"}])

(defn base-editor
  "Returns markup for an editor with backing model :id. Multiple editor views
   can correspond to the same backing editor model."
  [id {:keys [autofocus]
       :or {autofocus true}
       :as args}]
  {:pre [(validate-args-macro base-editor-args-desc args "base-editor")]}
  (let [project-id (subscribe [:current-project])
        editor (subscribe [:editor-with-id id])
        buffer (subscribe [:buffer-for-editor id])
        current-editor (subscribe [:current-editor])
        is-current (reaction (= (deref-or-value id) @current-editor))
        ;; IMPORTANT: string must have empty string default. Otherwise, there is a race condition when
        ;; a backing buffer is deleted, and this editor rerenders with null string, causing
        ;; codemirror to throw a null pointer exception.
        string (reaction (get @buffer :string ""))
        cursor-info (reaction (:cursor-info @editor))
        read-only (reaction (:read-only @editor))
        overlay-state (reaction (:overlay-state @editor))
        cm-instance (reagent/atom nil)
        viewport-info (reagent/atom nil)
        scroll-info (reagent/atom nil)
        handler (gen-handler editor string cm-instance viewport-info scroll-info)]
    (console/debug ::remount)
    (fn [id args]
      (let [id (deref-or-value id)]
        (if (and @editor @buffer)
          [v-box
           :class (str "editor "
                       (if @is-current
                         "focused "
                         " "))
           :size "auto"
           :children
           (-> []
               (conj [line-col-indicator cursor-info])
               (conj [all-overlays {:overlay-state overlay-state
                                    :string string
                                    :viewport-info viewport-info
                                    :scroll-info scroll-info}])
               (conj [codemirror/codemirror
                      (-> args
                          (assoc :model string
                                 :key (str @project-id "-editor-" id)
                                 :read-only read-only
                                 :options {:showInvisibles true
                                           :maxInvisibles 16
                                           :lineNumbers true
                                           :dragDrop false
                                           :indentWithTabs false
                                           :indentUnit 2
                                           :extraKeys #js {"Tab" indent-fn}
                                           :editorID id
                                           :autofocus autofocus}
                                 :handler handler))]))]
          [:p (str "Initializing editor...")])))))
