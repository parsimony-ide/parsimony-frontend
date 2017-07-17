(ns parsimony.views.editor.sample-editor
  (:require [parsimony.models.colors :as colors]
            [parsimony.views.editor.neo-editor :refer [base-editor]]
            [parsimony.views.ribbon :as ribbon]
            [parsimony.parser :as parser]
            [parsimony.util :refer [pprint-str]]
            [re-com.core :refer [label checkbox gap h-box v-box] :refer-macros [handler-fn]]
            [re-com.util :refer [deref-or-value]]
            [re-frame.core :refer [dispatch subscribe]]
            [parsimony.console :as console]))

(defn ribbon [id]
  (let [auto-parse (subscribe [:editor-auto-parse id])]
    (fn [id]
      [ribbon/ribbon
       {:model [(ribbon/command-item :lex-file (deref-or-value id) {:md-icon-name "zmdi-brush"})
                (ribbon/command-item :parse-file (deref-or-value id) {:md-icon-name "zmdi-format-paint"})
                (ribbon/command-item :batch-lex-file (deref-or-value id) {:md-icon-name "zmdi-code"})
                (ribbon/command-item :batch-parse-file (deref-or-value id) {:md-icon-name "zmdi-code"})
                #_(ribbon/command-item :test-answer (deref-or-value id) {:md-icon-name "zmdi-check-square"})
                (ribbon/button-item
                  {:text "Clear Overlays"
                   :md-icon-name "zmdi-format-color-reset"
                   :on-click #(dispatch [:remove-all-overlays (deref-or-value id)])})
                #_(ribbon/checkbox-item
                  {:label "Auto-parse"
                   :model auto-parse
                   :on-change #(dispatch [:editor-toggle-auto-parse (deref-or-value id)])})]}])))

(defn contextbar-element [id overlay]
  (let [decoration-map (subscribe [:editor-decoration-map-static id])
        decorations (subscribe [:decorations])]
    (fn [id {:keys [overlay-type overlay-tag decoration-index decoration-mod char-range] :as overlay}]
      (let [[char-from char-to _] char-range
            decoration (colors/lookup-decoration @decorations decoration-index)
            css (colors/decoration->html-css (colors/mix decoration decoration-mod))]
        [label
         :class "string"
         :style css
         :attr {:on-click (handler-fn
                            (when (= :parse overlay-type)
                              (dispatch [:activate-live-parse-view overlay])))
                :on-mouse-enter (handler-fn
                                  (dispatch [:contextbar-emphasize-overlay id overlay-type overlay-tag char-from char-to]))
                :on-mouse-leave (handler-fn
                                  (dispatch [:contextbar-clear-all-emphasis id]))}
         :label (name (parser/->nonterminal overlay-tag))]))))

(defn contextbar [id]
  (let [current-editor-id (subscribe [:current-editor])
        overlays-at-cursor (subscribe [:overlays-at-cursor] [current-editor-id])]
    (fn []
      (let [id (deref-or-value id)]
        (if (and (= id @current-editor-id)
                 (seq @overlays-at-cursor))
          [h-box
           :class "contextbar"
           :gap "5px"
           :children (into []
                           (for [[idx o] (map-indexed vector @overlays-at-cursor)]
                             ^{:key (str idx)}
                             [contextbar-element id o]))]
          [h-box
           :class "contextbar empty"
           :children []])))))

(defn sample-editor
  [id]
  (console/debug :render :sample-editor)
  [v-box
   :class "sample-editor"
   :size "auto"
   :children [[ribbon id]
              [base-editor id {:autofocus true}]
              [contextbar id]]])
