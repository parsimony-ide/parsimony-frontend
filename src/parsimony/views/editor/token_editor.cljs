(ns parsimony.views.editor.token-editor
  (:require [parsimony.views.editor.neo-editor :refer [base-editor]]
            [parsimony.views.ribbon :refer [ribbon] :as ribbon]
            [re-com.core :refer [h-box v-box]]
            [re-com.util :refer [deref-or-value]]
            [re-frame.core :refer [dispatch]]
            [parsimony.console :as console]))

(defn token-editor
  [id]
  (console/debug :render :token-editor)
  [v-box
   :class "token-editor"
   :size "auto"
   :children [[ribbon {:model [(ribbon/command-item :compile-lexer {:md-icon-name "zmdi-camera-alt"})
                               (ribbon/button-item
                                 {:text "Clear Overlays"
                                  :md-icon-name "zmdi-format-color-reset"
                                  :on-click #(dispatch [:remove-all-overlays (deref-or-value id)])})]}]
              [base-editor id {:autofocus true}]]])
