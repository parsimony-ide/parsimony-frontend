(ns parsimony.views.editor
  (:require [parsimony.views.editor.sample-editor :refer [sample-editor]]
            [parsimony.views.editor.token-editor :refer [token-editor]]
            [parsimony.views.editor.cfg-editor :refer [cfg-editor]]
            [parsimony.views.editor.generic-editor :refer [generic-editor]]
            [parsimony.util :refer [pprint-str]]
            [re-frame.core :refer [subscribe]]
            [parsimony.console :as console]))

(defn editor [id]
  (let [source (subscribe [:source-for-editor id])]
    #_(console/debug :source (pprint-str @source))
    (fn [id]
      (case (:source-type @source)
        :file [sample-editor id]
        :scratch [sample-editor id]
        :token [token-editor id]
        :grammar [cfg-editor id]
        ;; default
        [generic-editor id]))))
