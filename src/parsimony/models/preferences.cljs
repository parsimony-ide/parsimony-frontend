(ns parsimony.models.preferences
  (:require [parsimony.models.colors :as colors]
            [schema.core :as s :include-macros true]))

(def schema {:decorations [colors/decoration-schema]})

(def default-value
  {:decorations colors/default-decorations})
