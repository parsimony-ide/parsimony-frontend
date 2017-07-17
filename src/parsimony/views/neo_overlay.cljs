(ns parsimony.views.neo-overlay
  (:require [parsimony.com.svg :refer [svg]]
            [parsimony.models.colors :as colors]
            [parsimony.util :refer [split-on-char split-on-newline debug pprint-str]]
            [parsimony.views.info :as info]
            [reagent.ratom :refer-macros [reaction]]
            [parsimony.console :as console]
            [clojure.string :as str])
  (:require-macros [re-com.validate :refer [validate-args-macro]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Geometry Computation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; # Definitions:
;;
;; - index      : an integer denoting a position between characters. Starts at 0
;; - coord      : a 2-vector [l j] where j is an index into the string at line l
;; - rowindex   : a 2-vector [i j], where i and j are the start and end indices for a single line
;; - rowcoord   : a 2-vector [m n], where m and n are the start and end coords for a single line
;; - region     : a vector of rowcoords
;; - char-range : a 3-vector [i j x] where i and j are indices, and x is nil or an instance of IDetail
;;
;; - grid-point : a 2-vector [i j] corresponding to a position in the cartesian plane where a unit is the width/height of a character
;; - pixel      : a 2-vector [x y] corresponding to a position in the cartesian plane where a unit is a single pixel
;; - polygon    : a vector of pixels denoting vertices

(defn rowindices
  "Return a vector of rowindices for the given string"
  [string]
  (let [splits (split-on-newline string)
        last-idx (count string)]
    (->> splits
         (map first)
         (partition 2 1 (repeat last-idx))
         (map vec)
         (vec))))

(defn- overlap? [[char-start char-end] [row-start row-end]]
  (or ;; straddle start
      (and (< char-start row-start)
           (> char-end row-start))
      ;; straddle end
      (and (< char-start row-end)
           (> char-end row-end))
      ;; fully contained
      (and (>= char-start row-start)
           (<= char-end row-end))))

(defn- char-range->region [rs c rownum]
  (loop [c c rs (apply list rs) rownum rownum region []]
    #_(do (console/debug "c" (str c))
          (console/debug "rs" (str rs))
          (console/debug "region" (str region)))
    (let [[bci eci] c]
      ;; if there are no rowindices left, but there are still char-ranges, then just keep padding until we finish
      ;; this handles corner cases where the char range starts at the end of the string
      (if-let [[bri eri] ((fnil first [[bci eci]]) (seq rs))]
        (cond
          ;; fully contained within row
          (and (>= bci bri)
               (<= eci eri))
          (do #_(console/debug "fully contained")
              (conj region [[rownum (- bci bri)]
                            [rownum (- eci bri)]]))
          ;; split between rows
          (and (>= bci bri)
               (<  bci eri)) ;; <, not <=, because the range has to actually straddle the boundary
          (let [segment [[rownum (- bci bri)]
                         [rownum (- eri bri)]]
                remainder [eri eci]]
            #_(console/debug "split between")
            (recur remainder
                   (next rs)
                   (inc rownum)
                   (conj region segment)))
          ;; fully before row
          (and (< bci bri)
               (< eci bri))
          (do #_(console/debug "fully before")
              (recur c
                     (next rs)
                     (inc rownum)
                     region))
          ;; fully after row
          (> bci bri)
          (do #_(console/debug "fully after")
              region)
          ;; otherwise
          :else
          (assert false (str/join \newline
                                  ["This should never happen:"
                                   (str "bci=" bci)
                                   (str "eci=" eci)
                                   (str "bri=" bri)
                                   (str "eri=" eri)
                                   (str "c=" c)
                                   (str "rs=" rs)
                                   (str "region=" region)])))
        (do (console/error "No row indices remaining, yet char-ranges not exhausted.  This should not happen.")
            region)))))

(defn char-ranges->regions [rs cs]
  (let [rs (apply list (sort-by first rs))
        cs (apply list (sort-by first cs))
        ;; associate each element c of cs with a subsequence rs' of rs
        ;; such that the first element of rs' is the first row containing c
        rcs (second (reduce
                      (fn [[cs acc] [rownum rs]]
                        (if-let [r (first rs)]
                          (let [matches (take-while #(overlap? % r) cs)]
                            [(drop (count matches) cs)
                             (into acc
                                   (map #(vector % [rownum rs]) matches))])
                          [cs acc]))
                      [cs []]
                      (map #(vector % (drop % rs)) (range (count rs)))))]
    (into []
          (map (fn [[c [rownum rs]]]
                 (char-range->region rs c rownum)))
          rcs)))

(defn coord->grids [[row col]]
  (let [y (* row 1)
        x (* col 1)]
    ;; shrink by a facter of 0.15 so an overlay does not fill the entire line height
    (vector [x (+ 0.15 y)]
            [x (- (inc y) 0.15)])))

;; -----------------------------------------------------------------------------
;; Standard Geometry
;; -----------------------------------------------------------------------------

;; convert coords to grid points
;; a grid point marks the corner next to a character
(defn region->grids [region]
  (let [left-grids (->> region
                        (map first)
                        (mapcat coord->grids))
        right-grids (->> region
                         (map second)
                         (mapcat coord->grids))]
    (into (vec left-grids) (reverse right-grids))))

(defn grids->pixels [pixels-per-x-grid pixels-per-y-grid grids]
  (for [[x y] grids]
    [(* x pixels-per-x-grid) (* y pixels-per-y-grid)]))

(defn- add-trailing-newline
  "Add a trailing newline if one does not exist"
  [string]
  (if-not (.endsWith string \newline)
    (str string \newline)
    string))

(defn char-ranges->polygons [string cs char-width line-height]
  (let [string (add-trailing-newline string) ;; adding a trailing newline allows polygons to be computed at the end of the string
        pixels (->> cs
                    (char-ranges->regions (rowindices string))
                    (map region->grids)
                    (map (partial grids->pixels char-width line-height)))
        polygons (map #(hash-map :coords %) pixels)]
    (vec polygons)))

;; -----------------------------------------------------------------------------
;; Gutter Geometry
;; -----------------------------------------------------------------------------

(defn region->gutter-grids [region]
  (let [affected-lines (into (hash-set)
                             (for [region-line region coord region-line]
                               (first coord)))
        min-line (apply min affected-lines)
        max-line (apply max affected-lines)
        gutter-region [[[min-line 0] [min-line 1]]
                       [[max-line 0] [max-line 1]]]
        gutter-region (if (= min-line max-line)
                          (pop gutter-region))]
    (region->grids gutter-region)))

(defn char-ranges->gutter-polygons
  "A specialized variant of char-ranges->polygons that pushes all shapes to the
   leftmost gutter area"
  [string cs gutter-width line-height]
  (let [string (add-trailing-newline string) ;; adding a trailing newline allows polygons to be computed at the end of the string
        pixels (->> cs
                    (char-ranges->regions (rowindices string))
                    (map region->gutter-grids)
                    (map (partial grids->pixels gutter-width line-height)))
        polygons (map #(hash-map :coords %) pixels)]
    (vec polygons)))

;; -----------------------------------------------------------------------------
;; Visibility
;; -----------------------------------------------------------------------------

(defn- hidden? [overlay disabled-overlays peek]
  (let [{:keys [type tag]} overlay
        disabled? (contains? disabled-overlays [type tag])]
    (or (and disabled?
             (not (contains? peek [type tag]))) ;; if i'm disabled and not being peeked, then hide me
        (and peek
             (seq peek)
             (not (contains? peek [type tag])))))) ;; if something is being peeked, and it's not me, then hide me

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base Component
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -overlay-view
  "Pure, prop-only overlay component. Any atom dereferences should occur in a
   higher level component."
  ([polygons tag decoration emphasis-mods]
   (-overlay-view polygons tag decoration emphasis-mods false nil nil))
  ([polygons tag decoration emphasis-mods hidden]
   (-overlay-view polygons tag decoration emphasis-mods hidden nil nil))
  ([polygons tag decoration emphasis-mods hidden infos active-infos]
   #_(console/debug :render :-overlay-view (keyword tag))
   (into [:div.overlay
          (merge
            {:key tag}
            (when hidden
              {:style {:display "none"}}))
          [svg {:model polygons
                :decoration decoration
                :emphasis-mods emphasis-mods}]]
         (when (some? infos)
           [[info/details-view
             {:polygons polygons
              :infos infos
              :active-infos active-infos
              :decoration decoration}]]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic Overlay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dynamic-overlay-view
  [{:keys [overlay string char-width line-height] :as args}]
  (let [char-ranges (reaction (:char-ranges @overlay))
        polygons (reaction
                   ;; this should only be executed once char-ranges actually changes
                   (char-ranges->polygons @string
                                          @char-ranges
                                          @char-width
                                          @line-height))]
    (fn [{:keys [overlay decorations disabled-overlays peek] :as args}]
      (let [{:keys [type tag active-infos decoration-index decoration-mod emphasis-mods]} @overlay
            decoration (colors/mix (colors/lookup-decoration @decorations decoration-index)
                                   decoration-mod)]
        [-overlay-view
         @polygons
         tag
         decoration
         emphasis-mods
         (hidden? @overlay @disabled-overlays @peek)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Static Overlay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn static-overlay-view
  "Overlays whose underlying char-ranges never change. Input arguments must not
   be atoms."
  [{:keys [string char-width line-height decorations disabled-overlays peek]
    {:keys [char-ranges tag decoration-index decoration-mod emphasis-mods] :as overlay} :overlay
    :as args}]
  (let [polygons (char-ranges->polygons string
                                        char-ranges
                                        char-width
                                        line-height)
        decoration (colors/mix (colors/lookup-decoration decorations decoration-index)
                               decoration-mod)]
    #_(console/debug :static-overlay-view
                     (pprint-str
                       {:string string
                        :char-width char-width
                        :line-height line-height
                        :overlay overlay
                        :decorations decorations
                        :polygons polygons
                        :decoration decoration}))
    [-overlay-view
     polygons
     tag
     decoration
     emphasis-mods
     (hidden? overlay disabled-overlays peek)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Error Overlay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn error-overlay-view
  [{:keys [overlay string char-width line-height] :as args}]
  (let [char-ranges (reaction (:char-ranges @overlay))
        infos (reaction (map #(get % 2) @char-ranges))
        polygons (reaction
                   ;; this should only be executed once char-ranges actually changes
                   (char-ranges->polygons @string
                                          @char-ranges
                                          @char-width
                                          @line-height))]
    (fn [{:keys [overlay decorations disabled-overlays peek] :as args}]
      (let [{:keys [type tag active-infos decoration-index decoration-mod emphasis-mods]} @overlay
            decoration (colors/mix (colors/lookup-decoration @decorations decoration-index)
                                   decoration-mod)]
        [-overlay-view
         @polygons
         tag
         decoration
         emphasis-mods
         (hidden? @overlay @disabled-overlays @peek)
         @infos
         active-infos]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gutter Overlay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn gutter-overlay-view
  [{:keys [overlay string char-width line-height] :as args}]
  (let [char-ranges (reaction (:char-ranges @overlay))
        polygons (reaction
                   ;; this should only be executed once char-ranges actually changes
                   (char-ranges->gutter-polygons @string
                                                 @char-ranges
                                                 @char-width
                                                 @line-height))]
    (fn [{:keys [overlay decorations disabled-overlays peek] :as args}]
      (let [{:keys [type tag active-infos decoration-index decoration-mod emphasis-mods]} @overlay
            decoration (colors/mix (colors/lookup-decoration @decorations decoration-index)
                                   decoration-mod)]
        [-overlay-view
         @polygons
         tag
         decoration
         emphasis-mods
         (hidden? @overlay @disabled-overlays @peek)]))))
