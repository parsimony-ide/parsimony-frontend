(ns parsimony.models.buffer
    (:require [schema.core :as s :include-macros true]))

(def schema {:id s/Num
             :string s/Str
             :source-id (s/maybe s/Num) ;; a buffer can have nil source-id, in which case it's just a scratch buffer
             })

;; Not quite a default, because it's missing an :id and :source-id
(def default-value
  {:string ""})
