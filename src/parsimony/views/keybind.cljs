(ns parsimony.views.keybind
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [keybind.core :as key]
            [parsimony.commands :as commands]
            [re-com.core :refer [h-box]]
            [re-frame.core :refer [dispatch]]))

(defn shift-view []
  [:span \u21e7])

(defn ctrl-view []
  [:span \u2303])

(defn alt-view []
  [:span \u2325])

(defn meta-view []
  [:span \u2318])

(defn key-view [key]
  [:span (str/upper-case (str key))])

(defn segment-view [{:keys [shift ctrl alt meta key] :as segment}]
  [h-box
   :children [(when shift [shift-view])
              (when ctrl [ctrl-view])
              (when alt [alt-view])
              (when meta [meta-view])
              [key-view key]]])

(defn keybind-view [keybind-str]
  (let [keybind-segments (commands/parse-keybind-str keybind-str)]
    [h-box
     :gap "3px"
     :children (into []
                     (map (partial vector segment-view))
                     keybind-segments)]))
