(ns parsimony.com.neo-codemirror
  (:require [dommy.core :refer [clear! set-style!] :refer-macros [sel sel1]]
            [parsimony.util :refer [dom-visible?]]
            [re-com.core :refer [p]]
            [re-com.box :refer [v-box box gap line flex-child-style align-style]]
            [re-com.util :refer [deref-or-value px]]
            [re-com.validate :refer [number-or-string? css-style? html-attr?]]
            [re-frame.db]
            [re-frame.core :refer [dispatch]]
            [reagent.core :as reagent]
            [reagent.ratom :as ratom :include-macros true]
            [schema.core :as s :include-macros true]
            [parsimony.console :as console :include-macros true])
  (:require-macros [re-com.core :refer [handler-fn]]
                   [re-com.validate :refer [validate-args-macro]]))

(defn install-commands! []
  (letfn [(on-undo [cm]
            (when-let [editor-id (.-editorID (.-options cm))]
              #_(console/debug ::undo {:editor-id editor-id})
              (dispatch [:editor-undo editor-id])))
          (on-redo [cm]
            (when-let [editor-id (.-editorID (.-options cm))]
              #_(console/debug ::redo {:editor-id editor-id})
              (dispatch [:editor-redo editor-id])))]
    (doto (.-commands js/CodeMirror)
      (aset "undo" on-undo)
      (aset "redo" on-redo))))

(defn- line-ch->clj
  "Convert a codemirror line-ch object to a map"
  [cm line-ch]
  (let [line (.-line line-ch)
        ch   (.-ch line-ch)]
    {:line line
     :ch ch
     :index (-> cm
                (.getDoc)
                (.indexFromPos line-ch))}))

(defn- clj->line-ch
  "Convert a :line :ch map to a line-ch js object"
  [{:keys [line ch]}]
  (CodeMirror.Pos. line ch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CodeMirror Wrapper Protocols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ICodeMirror
  (get-value [this])
  (set-value [this v])
  (set-value-silently [this v] "Set value without firing a change event")
  (get-viewport-info [this])
  (get-scroll-info [this])
  (set-scroll [this scroll-info])
  (get-cursor-info [this])
  (set-cursor [this cursor-info])
  (refresh [this])
  (focus [this]))

(defprotocol ICodeMirrorDOM
  (visible? [this]))

(defprotocol ICodeMirrorWatch
  (start-watches! [this external-model read-only])
  (clear-watches! [this]))

(defprotocol ICodeMirrorHandler
  (on-initialize [this cm])
  (on-external-change [this cm value cursor-info update-history?])
  (on-internal-change [this cm value cursor-info update-history?])
  (on-blur [this cm])
  (on-focus [this cm])
  (on-keydown [this cm keycode])
  (on-keyup [this cm keycode])
  (on-scroll [this cm])
  (on-cursor-activity [this cm])
  (on-viewport-change [this cm]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CodeMirror Wrapper Private Implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- refresh! [cm]
  (let [enabled (get-in cm [:watches :refresh :enabled])]
    (if (visible? cm)
      (when @enabled
        #_(console/debug ::refresh!)
        (refresh cm)
        (when (get-in cm [:options :autofocus])
          (focus cm))
        (reset! enabled false))
      (reset! enabled true))))

(defn- clear-refresh-interval! [cm]
  (let [refresh-interval-id (get-in cm [:watches :refresh :interval-id])]
    (when @refresh-interval-id
      (.clearInterval js/window @refresh-interval-id))))

(defn- start-refresh-interval! [cm]
  (clear-refresh-interval! cm)
  (reset! (get-in cm [:watches :refresh :interval-id])
          (.setInterval js/window #(refresh! cm) 100)))

(defn- clear-propagate-delay! [cm]
  (let [propagate-timeout-id (get-in cm [:watches :propagate :timeout-id])
        cursor-info (get-in cm [:watches :propagate :cursor-info])]
    (when @propagate-timeout-id
      (.clearInterval js/window @propagate-timeout-id)
      (reset! cursor-info nil)
      (reset! propagate-timeout-id nil))))

(defn propagate! [cm update-history?]
  #_(console/debug ::propagate! {:update-history? update-history?})
  (let [cursor-info (or (deref (get-in cm [:watches :propagate :cursor-info]))
                        (get-cursor-info cm))]
    (clear-propagate-delay! cm)
    (on-internal-change (:handler cm) cm (get-value cm) cursor-info update-history?)))

(defn- restart-propagate-delay! [cm]
  #_(console/debug ::restart-propagate-delay!)
  (clear-propagate-delay! cm)
  (reset! (get-in cm [:watches :propagate :cursor-info])
          (get-cursor-info cm))
  (reset! (get-in cm [:watches :propagate :timeout-id])
          (.setTimeout js/window #(propagate! cm true) 1250)))

(defn- propagate-delay-on? [cm]
  (some? (deref (get-in cm [:watches :propagate :timeout-id]))))

(defn- set-cursor-visible! [cm]
  ;; Make the cursor visible. Used to keep the cursor visible even when codemirror does not have focus.
  ;; See https://discuss.codemirror.net/t/how-to-always-display-cursor/227/8
  (set-style! (sel1 (:dom-root cm) ".CodeMirror-cursors") :visibility "visible"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CodeMirror Wrapper Public Implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord CodeMirror [cm-instance
                       dom-root
                       options
                       handler
                       watches]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ICodeMirror
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ICodeMirror

  (get-value [_]
    (.getValue cm-instance))

  (set-value [_ v]
    (.setValue cm-instance v))

  (set-value-silently [{:keys [cm-instance] :as cm} v]
    (aset cm-instance "silent" true)
    (.setValue cm-instance v)
    (aset cm-instance "silent" false))

  (get-viewport-info [_]
    {:line-height (.defaultTextHeight cm-instance)
     :char-width  (.defaultCharWidth cm-instance)
     :gutter-width (.-width (.getBoundingClientRect (sel1 dom-root ".CodeMirror-gutters")))
     :char-coords (js->clj (.charCoords cm-instance (js/CodeMirror.Pos 0 0) "local") :keywordize-keys true)})

  (get-scroll-info [_]
    (js->clj (.getScrollInfo cm-instance) :keywordize-keys true))

  (set-scroll [_ {:keys [left top] :as scroll-info}]
    (.scrollTo cm-instance left top))

  (get-cursor-info [_]
    (let [head (line-ch->clj cm-instance (.getCursor cm-instance "head"))
          anchor (line-ch->clj cm-instance (.getCursor cm-instance "anchor"))]
      {:selection? (not= (:index head) (:index anchor))
       :anchor anchor
       :head head}))

  (set-cursor [_ {:keys [head] :as cursor-info}]
    (.setCursor cm-instance (clj->line-ch head)))

  (refresh [this]
    (when (visible? this)
      (console/debug ::refresh)
      (.refresh cm-instance)))

  (focus [this]
    (when (visible? this)
      (console/debug ::focus)
      (.focus cm-instance)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ICodeMirrorDOM
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ICodeMirrorDOM

  (visible? [_]
    (dom-visible? dom-root))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ICodeMirrorWatch
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ICodeMirrorWatch

  (start-watches! [this external-model read-only]
    (let [{:keys [dispose-on-unmount]} watches]
      ;; watch for external value changes
      (swap! dispose-on-unmount conj
             (ratom/run!
               (when-let [external-value @external-model]
                 (when-not (= external-value (.getValue cm-instance))
                   (let [scroll-info (get-scroll-info this)
                         cursor-info (get-cursor-info this)]
                     (console/debug ::external-change)
                     (on-external-change (:handler this) this external-value cursor-info true)
                     (set-value-silently this external-value)
                     (set-scroll this scroll-info)
                     (set-cursor this cursor-info))))))

      ;; watch for read-only flag
      (swap! dispose-on-unmount conj
             (ratom/run!
               (if @read-only
                 (.setOption cm-instance "readOnly" true)
                 (.setOption cm-instance "readOnly" false)))))

    ;; start refresh interval
    (refresh! this)
    (start-refresh-interval! this))

  (clear-watches! [this]
    (clear-refresh-interval! this)
    (clear-propagate-delay! this)
    (doseq [x (deref (:dispose-on-unmount watches))]
      (ratom/dispose! x))))

(defn- install-handler! [cm handler]
  (install-commands!)
  (doto (:cm-instance cm)
    (.on "change"
         (fn [_ _]
           (when-not (aget (:cm-instance cm) "silent")
             (restart-propagate-delay! cm))))
    (.on "blur" (fn [_]
                  (set-cursor-visible! cm)
                  (on-blur handler cm)
                  (propagate! cm true)))
    (.on "focus" #(on-focus handler cm))
    (.on "keydown" #(on-keydown handler cm %2))
    (.on "keyup" #(on-keyup handler cm %2))
    (.on "scroll" #(on-scroll handler cm))
    (.on "cursorActivity" #(on-cursor-activity handler cm))
    (.on "viewportChange" #(on-viewport-change handler cm))))

(defn new-codemirror [dom-root options handler]
  (let [cm-instance (js/CodeMirror dom-root (clj->js options))
        cm (map->CodeMirror
             {:cm-instance cm-instance
              :dom-root dom-root
              :options options
              :handler handler
              :watches {:dispose-on-unmount (atom [])
                        :refresh {:enabled (atom false)
                                  :interval-id (atom nil)}
                        :propagate {:timeout-id (atom nil)
                                    :cursor-info (atom nil)}}})]
    (install-handler! cm handler)
    cm))

(defn codemirror [{:keys [model options read-only key handler]}]
  (let [cm (atom nil)]
    (letfn [(will-unmount! [e]
              #_(console/debug ::will-unmount!)
              (clear-watches! @cm)
              (reset! cm nil)
              (clear! (reagent/dom-node e)))

            (did-mount! [e]
              #_(console/debug ::did-mount!)
              (let [-cm (new-codemirror (reagent/dom-node e) options handler)]
                (reset! cm -cm)
                (set-value-silently -cm @model)
                (let [handler (:handler -cm)]
                  (on-initialize handler -cm)
                  (on-viewport-change handler -cm)
                  (on-scroll handler -cm))
                ;; start watches at the end of mount to prevent spurious
                ;; watches from firing due to initialization sequence
                (start-watches! -cm model read-only)))]
      (reagent/create-class
        {:component-will-unmount will-unmount!
         :component-did-mount did-mount!
         :display-name "neo-codemirror"
         :reagent-render
         (fn [_]
           ^{:key key}
           [:div
            {:class (str "codemirror-parent")}])}))))
