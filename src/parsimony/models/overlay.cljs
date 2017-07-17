(ns parsimony.models.overlay
  (:require [medley.core :refer [dissoc-in]]
            [parsimony.models.colors :as colors]
            [parsimony.views.info :refer [IDetail]]
            [schema.core :as s :include-macros true]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def char-range-schema
  [(s/one s/Num "start")                           ;; start character index
   (s/one s/Num "end")                             ;; end character index
   (s/one (s/maybe (s/protocol IDetail)) "info")]) ;; descriptive info

(def emphasis-map-schema
  ;; map from
  ;; - index of the polygon to emphasize, to
  ;; - decoration-mod to apply for emphasis
  {s/Num colors/decoration-mod-schema})

(def schema {:char-ranges [char-range-schema]                       ;; vector of character ranges sorted by start
             :active-infos #{s/Num}                                 ;; a set of char-ranges whose descriptive info should be visible
             :emphasis-mods {s/Keyword emphasis-map-schema}         ;; map from emphasis keyword to corresponding emphasis map
             :decoration-index s/Num
             :decoration-mod (s/maybe colors/decoration-mod-schema) ;; merged into decoration to modify appearance (i.e., a mixin for style)
             :type s/Keyword
             :tag s/Str})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defaults
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def default-value
  {:char-ranges []
   :active-infos (sorted-set)
   :emphasis-mods (sorted-map)
   :decoration-mod nil})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn polygon-index
  "Return the index of the polygon with the given char-from and char-to, if any"
  [{:keys [char-ranges] :as overlay} char-from char-to]
  (first (keep-indexed (fn [i [from to _]]
                         (when (and (= char-from from)
                                    (= char-to to))
                           i))
                       char-ranges)))

(defn apply-emphasis [overlay key index mod]
  (assoc-in overlay [:emphasis-mods key index] mod))

(defn apply-all-emphasis [overlay key mod]
  (reduce #(assoc-in %1 [:emphasis-mods key %2] mod)
          overlay
          (range (count (:char-ranges overlay)))))

(defn clear-emphasis [overlay key index]
  (update overlay :emphasis-mods dissoc-in [key index]))

(defn clear-all-emphasis [overlay key]
  (assoc-in overlay [:emphasis-mods key] (:emphasis-mods default-value)))
