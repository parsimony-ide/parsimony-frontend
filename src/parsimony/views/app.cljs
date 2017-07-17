(ns parsimony.views.app
  (:require [parsimony.views.modal :as modal]
            [parsimony.views.modebar :as modebar]
            [parsimony.views.workspace :as workspace]
            [re-com.core :refer [h-box]]))

(def workspace-id 1)

(defn main []
  [h-box
   :height "100%"
   :size "auto"
   :children [#_[modebar/modebar]
              [workspace/workspace workspace-id]
              [modal/global-modal]]])
