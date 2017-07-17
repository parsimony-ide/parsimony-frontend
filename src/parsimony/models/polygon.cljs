(ns parsimony.models.polygon
  (:require [schema.core :as s :include-macros true]))

;; A polygon is a vector of coordinate pairs describing a path
(def schema {:coords [(s/pair s/Num "x" s/Num "y")]})

(def default-value
  {:coords []})
