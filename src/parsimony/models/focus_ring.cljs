(ns parsimony.models.focus-ring
  (:require [parsimony.models.editor :as editor]
            [parsimony.views.workspace :as workspace]
            [parsimony.views.cards :as cards]
            [schema.core :as s :include-macros true]))

(def MAX_SIZE 200)

(def schema [s/Str]) ;; sequence of card-keys

(def default-model
  [])

(defn add [focus-ring key]
  (cond
    (= key (peek focus-ring)) focus-ring
    (< (count focus-ring) MAX_SIZE) (conj focus-ring key)
    :else (-> []
              (into (drop 1) focus-ring)
              (conj key))))

(defn previously-focused-editors [focus-ring workspace]
  (let [cards (map (partial workspace/card-key->card workspace) focus-ring)]
    (into []
          (comp (filter some?)
                (filter cards/editor-link?)
                (map cards/backing-editor-id))
          cards)))

(defn last-focused-sample-editor
  [{:keys [focus-ring workspaces current-workspace] :as db}]
  (->> (get workspaces current-workspace)
       (previously-focused-editors focus-ring)
       (filter (partial editor/sample-editor? db))
       (last)))


