(ns parsimony.views.editor.generic-editor
  (:require [parsimony.views.editor.neo-editor :refer [base-editor]]
            [re-com.core :refer [v-box]]
            [parsimony.console :as console]))

(defn generic-editor
  [id]
  #_(console/debug :render :generic-editor)
  [v-box
   :class "generic-editor"
   :size "auto"
   :children [[base-editor id {:autofocus false}]]])
