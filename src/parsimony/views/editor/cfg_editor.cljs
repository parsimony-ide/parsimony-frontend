(ns parsimony.views.editor.cfg-editor
  (:require [parsimony.views.editor.neo-editor :refer [base-editor]]
            [parsimony.views.ribbon :refer [ribbon] :as ribbon]
            [re-com.core :refer [h-box v-box]]
            [re-com.util :refer [deref-or-value]]
            [re-frame.core :refer [dispatch]]
            [parsimony.console :as console]))

(defn cfg-editor
  [id]
  (console/debug :render :cfg-editor)
  [v-box
   :class "cfg-editor"
   :size "auto"
   :children [[ribbon {:model [(ribbon/command-item :compile-parser {:md-icon-name "zmdi-camera-alt"})
                               (ribbon/command-item :format-file (deref-or-value id) {:md-icon-name "zmdi-format-align-left"})
                               (ribbon/button-item
                                 {:text "Clear Overlays"
                                  :md-icon-name "zmdi-format-color-reset"
                                  :on-click #(dispatch [:remove-all-overlays (deref-or-value id)])})]}]
              [base-editor id {:autofocus true}]]])
