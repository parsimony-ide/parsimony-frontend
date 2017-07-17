(ns parsimony.models.lexer
  (:require [schema.core :as s :include-macros true]))

(def token-schema {:string s/Str
                   :label s/Keyword
                   :start s/Num
                   :end s/Num})
