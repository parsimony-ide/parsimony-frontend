(ns parsimony.user-agent
  (:import [goog.userAgent product])
  (:require [goog.userAgent.product.isVersion]))

(defn is-supported? []
  (and (.-CHROME product)
       (.isVersion product 45)))
