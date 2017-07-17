(ns parsimony.models.colors
  (:require [clojure.string :as str]
            [schema.core :as s :include-macros true]
            [parsimony.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def rgba-schema [(s/one s/Num "r") (s/one s/Num "g") (s/one s/Num "b") (s/one s/Num "a")])

(def decoration-schema {:stroke {:color rgba-schema
                                 :width s/Num
                                 :opacity s/Num
                                 :pattern (s/enum nil)}
                        :fill {:color rgba-schema
                               :opacity s/Num
                               :pattern (s/enum nil
                                                "hatch-0"
                                                "hatch-45"
                                                "hatch-90"
                                                "hatch-135")}
                        :filter {:effect (s/enum nil "glow")}})

(def decoration-mod-schema {s/Keyword s/Any})

(def decoration-affinities-schema {s/Str s/Num}) ;; map from tag string to color index

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;------------------------------------------------------------------------------
;; Kelly Colors
;;------------------------------------------------------------------------------

;; See http://stackoverflow.com/q/470690/128927
(def kelly-colors [#_[255 179 0 1]   ;; Vivid Yellow (excluded since yellow is hard to see)
                   [128 62 117 1]    ;; Strong Purple
                   [255 104 0 1]     ;; Vivid Orange
                   [166 189 215 1]   ;; Very Light Blue
                   [193 0 32 1]      ;; Vivid Red
                   [206 162 98 1]    ;; Grayish Yellow
                   #_[129 112 102 1] ;; Medium Gray (excluded since it is too similar to other grays)

                   ;; The following don't work well for people with defective color vision
                   [0 125 52 1]    ;; Vivid Green
                   [246 118 142 1] ;; Strong Purplish Pink
                   [0 83 138 1]    ;; Strong Blue
                   [255 122 92 1]  ;; Strong Yellowish Pink
                   [83 55 122 1]   ;; Strong Violet
                   [255 142 0 1]   ;; Vivid Orange Yellow
                   [179 40 81 1]   ;; Strong Purplish Red
                   [244 200 0 1]   ;; Vivid Greenish Yellow
                   [127 24 13 1]   ;; Strong Reddish Brown
                   [147 170 0 1]   ;; Vivid Yellowish Green
                   [89 51 21 1]    ;; Deep Yellowish Brown
                   [241 58 19 1]   ;; Vivid Reddish Orange
                   [35 44 22 1]    ;; Dark Olive Green
                   ])

;;------------------------------------------------------------------------------
;; Boynton Colors
;;------------------------------------------------------------------------------

(def boynton-colors [[0 0 255 1]     ;; Blue
                     #_[255 0 0 1]   ;; Red (excluded since it's used for error coloring)
                     [0 255 0 1]     ;; Green
                     #_[255 255 0 1] ;; Yellow (excluded since yellow is hard to see)
                     [255 0 255 1]   ;; Magenta
                     [255 128 128 1] ;; Pink
                     [128 128 128 1] ;; Gray
                     [128 0 0 1]     ;; Brown
                     [255 128 0 1]]) ;; Orange

;;------------------------------------------------------------------------------
;; CMC l:c Colors
;; Generated from http://phrogz.net/css/distinct-colors.html
;;------------------------------------------------------------------------------

(def visually-distinct-colors-105
  [#_[255 0 0 1]
   [229 153 0 1]
   [0 102 27 1]
   #_[45 68 89 1]
   #_[46 26 51 1]
   #_[115 29 29 1]
   [217 199 163 1]
   #_[67 89 73 1]
   #_[13 28 51 1]
   [242 191 255 1]
   #_[76 19 19 1]
   [255 204 0 1]
   [70 140 98 1]
   [0 44 166 1]
   [242 0 226 1]
   [153 77 77 1]
   [127 102 0 1]
   [0 191 102 1]
   [121 153 242 1]
   [128 0 119 1]
   [204 153 153 1]
   [229 207 115 1]
   [19 77 50 1]
   [163 177 217 1]
   [191 96 185 1]
   [140 19 0 1]
   [115 103 57 1]
   [124 166 146 1]
   [64 89 255 1]
   [255 0 170 1]
   [191 86 48 1]
   [178 167 0 1]
   #_[13 51 38 1]
   [105 110 140 1]
   [166 0 111 1]
   [255 162 128 1]
   #_[51 49 26 1]
   [57 230 195 1]
   #_[0 0 89 1]
   #_[102 0 68 1]
   #_[140 89 70 1]
   [255 251 191 1]
   [41 166 157 1]
   #_[38 38 77 1]
   #_[64 0 34 1]
   #_[76 48 38 1]
   #_[102 100 77 1]
   [22 89 85 1]
   [31 0 230 1]
   #_[115 57 88 1]
   #_[115 94 86 1]
   [226 242 0 1]
   [172 230 226 1]
   #_[18 13 51 1]
   [128 96 113 1]
   [217 87 0 1]
   #_[71 77 0 1]
   [0 226 242 1]
   [87 77 153 1]
   [153 0 61 1]
   [127 51 0 1]
   [161 242 0 1]
   [0 60 64 1]
   [145 115 230 1]
   [255 64 140 1]
   #_[51 28 13 1]
   [86 115 29 1]
   [115 207 230 1]
   #_[56 26 102 1]
   [204 102 143 1]
   [204 173 153 1]
   [170 204 102 1]
   [77 138 153 1]
   [82 0 153 1]
   [242 182 206 1]
   #_[64 54 48 1]
   [152 166 124 1]
   [0 136 204 1]
   #_[79 67 89 1]
   #_[51 38 43 1]
   [217 116 0 1]
   #_[33 51 13 1]
   [38 115 153 1]
   [170 0 255 1]
   [204 0 27 1]
   #_[76 41 0 1]
   [48 179 0 1]
   #_[38 47 51 1]
   [153 51 204 1]
   #_[51 0 7 1]
   [127 83 32 1]
   [208 255 191 1]
   [0 88 166 1]
   [152 124 166 1]
   [255 64 89 1]
   [191 147 96 1]
   [127 255 128 1]
   #_[0 61 115 1]
   #_[41 0 51 1]
   [255 128 145 1]
   #_[102 78 51 1]
   [0 255 34 1]
   [128 196 255 1]
   #_[80 45 89 1]
   #_[89 45 51 1]])

(def visually-distinct-colors-71
  [[191 96 96 1]
   [255 162 128 1]
   [191 163 143 1]
   [76 69 38 1]
   [194 242 0 1]
   [51 191 0 1]
   [48 191 143 1]
   [96 185 191 1]
   [32 83 128 1]
   [124 141 166 1]
   [200 191 255 1]
   [170 0 255 1]
   [242 0 194 1]
   [217 0 87 1]
   [64 32 36 1]
   [102 51 51 1]
   [51 32 26 1]
   [127 68 0 1]
   [229 214 0 1]
   [133 166 0 1]
   [200 255 191 1]
   [64 128 106 1]
   [0 92 115 1]
   [0 82 204 1]
   [48 52 64 1]
   [120 115 153 1]
   [222 115 230 1]
   [51 0 41 1]
   [153 0 41 1]
   [204 153 153 1]
   [89 36 0 1]
   [255 196 128 1]
   [217 210 108 1]
   [105 115 86 1]
   [26 102 26 1]
   [38 77 64 1]
   [0 51 77 1]
   [0 26 64 1]
   [0 25 191 1]
   [20 0 51 1]
   [89 0 83 1]
   [115 86 105 1]
   [76 0 20 1]
   [229 61 0 1]
   [217 119 54 1]
   [191 128 0 1]
   [83 89 0 1]
   [195 255 128 1]
   [83 166 83 1]
   [64 255 242 1]
   [64 191 255 1]
   [64 140 255 1]
   [0 0 89 1]
   [125 89 179 1]
   [140 35 133 1]
   [217 54 141 1]
   [255 128 162 1]
   [140 63 35 1]
   [115 80 57 1]
   [140 117 70 1]
   [201 204 153 1]
   [36 51 26 1]
   [64 255 166 1]
   [38 51 50 1]
   [191 234 255 1]
   [108 152 217 1]
   [65 57 115 1]
   [68 0 128 1]
   [204 153 201 1]
   [128 32 83 1]
   [229 0 31 1]])

(def visually-distinct-colors-50
  [[102 0 0 1]
   [166 152 124 1]
   [0 179 71 1]
   [191 217 255 1]
   [80 45 89 1]
   [242 121 121 1]
   [255 230 128 1]
   [0 51 20 1]
   [32 57 128 1]
   [182 143 191 1]
   [178 24 0 1]
   [255 238 0 1]
   [0 153 122 1]
   [86 94 115 1]
   [121 32 128 1]
   [191 121 96 1]
   [85 89 22 1]
   [38 77 69 1]
   [64 89 255 1]
   [204 0 136 1]
   [115 73 57 1]
   [49 51 26 1]
   [64 255 242 1]
   [128 145 255 1]
   [166 0 66 1]
   [51 20 0 1]
   [143 191 48 1]
   [51 173 204 1]
   [0 0 242 1]
   [255 128 179 1]
   [217 119 54 1]
   [177 217 163 1]
   [191 242 255 1]
   [0 0 204 1]
   [255 0 68 1]
   [230 195 172 1]
   [94 115 86 1]
   [0 136 204 1]
   [51 0 128 1]
   [89 22 40 1]
   [89 48 0 1]
   [0 255 0 1]
   [0 68 102 1]
   [34 0 64 1]
   [230 172 180 1]
   [191 143 48 1]
   [0 102 27 1]
   [13 28 51 1]
   [206 61 242 1]
   [140 105 110 1]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Decoration Mods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn mix [decoration decoration-mod]
  (merge-with merge decoration decoration-mod))

(def no-mod
  {})

(def black-outline-mod
  {:stroke {:color [0 0 0 1]}})

(def red-outline-mod
  {:stroke {:color [255 0 0 1]}})

(def green-outline-mod
  {:stroke {:color [0 255 0 1]}})

(def dashed-outline-mod
  {:stroke {:pattern "dashed"}})

(def no-outline-mod
  {:stroke {:opacity 0}})

(def hatch-45-mod
  {:fill {:pattern "hatch-45"}})

(def hatch-90-mod
  {:fill {:pattern "hatch-90"}})

(def hatch-135-mod
  {:fill {:pattern "hatch-135"}})

(def invisible-mod
  {:stroke {:opacity 0}
   :fill {:opacity 0}})

(def glow-mod
  {:filter {:effect "glow"}})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predefined Decorations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def SEVERE -1)

(def WHITESPACE -2)

(defn- scale-opacity [color ratio]
  (update color 3 * ratio))

(defn- color->decoration [color]
  {:stroke {:color color
            :width 1
            :opacity 1
            :pattern nil}
   :fill {:color (scale-opacity color 0.4)
          :opacity 1
          :pattern nil}
   :filter {:effect nil}})

(def default-plain-decorations
  (into []
        (map color->decoration)
        visually-distinct-colors-105))

(def default-hatch-45-decorations
  (into []
        (map #(mix % hatch-45-mod))
        default-plain-decorations))

(def default-hatch-90-decorations
  (into []
        (map #(mix % hatch-90-mod))
        default-plain-decorations))

(def default-hatch-135-decorations
  (into []
        (map #(mix % hatch-135-mod))
        default-plain-decorations))

(def default-decorations
  (-> default-plain-decorations
      (into default-hatch-45-decorations)
      (into default-hatch-90-decorations)
      (into default-hatch-135-decorations)))

#_(def missing-decoration (mix (color->decoration [0 0 0 1])
                               hatch-90-mod))

(def missing-decoration (mix (color->decoration [255 255 255 1])
                             black-outline-mod))

(def error-decoration (color->decoration [255 0 0 1]))

(def ws-decoration (mix (color->decoration [0 191 255 0.4])
                        dashed-outline-mod))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn lookup-decoration [decorations decoration-index]
  (cond
    (= SEVERE decoration-index)
    error-decoration
    (= WHITESPACE decoration-index)
    ws-decoration
    (>= decoration-index (count decorations))
    missing-decoration
    :else
    (get decorations decoration-index missing-decoration)))

(defn color->str [color]
  (str "rgba(" (str/join "," color) ")"))

(defn stroke-decoration->svg-css [{:keys [color pattern width opacity] :as stroke}]
  (merge {:stroke (color->str color)
          :stroke-width (str width)
          :stroke-opacity (str opacity)}
         (when (some? pattern)
           (case pattern
             "dashed" {:stroke-dasharray "3,3"}
             nil))))

(defn fill-decoration->svg-css [{:keys [color pattern opacity] :as fill}]
  (merge (if (some? pattern)
           {:fill (str "url(#" pattern "-" (str/join "-" color) ")")}
           {:fill (color->str color)})
         {:fill-opacity (str opacity)}))

(defn filter-decoration->svg-css [filter]
  (when-let [effect (:effect filter)]
    {:filter (str "url(#" effect ")")}))

(defn decoration->svg-css [{:keys [stroke fill filter] :as decoration}]
  (merge (stroke-decoration->svg-css stroke)
         (fill-decoration->svg-css fill)
         (filter-decoration->svg-css filter)))

(defn stroke-decoration->html-css [{:keys [color opacity pattern] :as stroke}]
  (merge {:border-color (color->str (assoc color 3 opacity))
          :border-width "1px"
          :border-style "solid"}
         (when (some? pattern)
           (case pattern
             "dashed" {:border-style "dashed"}
             nil))))

(defn fill-pattern->css-str [{:keys [color pattern] :as fill}]
  (let [angle (case pattern
                "hatch-0" 90  ;; XXX: yes, 90 indicates horizontal lines in repeating-linear-gradient
                "hatch-45" 45
                "hatch-90" 0  ;; XXX: yes, 0 indicates vertical lines in repeating-linear-gradient
                "hatch-135" 135)]
    (str "repeating-linear-gradient("
         (str/join ","
                   [(str "-" angle "deg")
                    "transparent"
                    "transparent 2px"
                    (str (color->str color) " 2px")
                    (str (color->str color) " 3px")])
         ")")))

(defn fill-decoration->html-css [{:keys [color pattern] :as fill}]
  (if (some? pattern)
    {:background (fill-pattern->css-str fill)}
    {:background (color->str color)}))

(defn decoration->html-css [{:keys [stroke fill] :as decoration}]
  (merge (stroke-decoration->html-css stroke)
         (fill-decoration->html-css fill)))

(defn add-affinity [affinities tag color-index]
  (if-let [existing (get affinities tag)]
    (do (console/warn :add-affinity :tag-already-exists tag existing color-index)
        affinities)
    (assoc affinities tag color-index)))

(defn get-affinity [affinities tag]
  (when-let [existing (get affinities tag)]
    existing))

(defn next-available-affinity [affinities]
  (first (remove #(contains? (set (vals affinities)) %) (range))))

(defn allocate-affinity [affinities tag]
  (if-let [color-index (get-affinity affinities tag)]
    [affinities color-index]
    (let [color-index (if (= tag "ws")
                        -2
                        (next-available-affinity affinities))]
      [(add-affinity affinities tag color-index) color-index])))

(defn allocate-affinities [affinities tags]
  (reduce #(first (allocate-affinity %1 %2)) affinities tags))
