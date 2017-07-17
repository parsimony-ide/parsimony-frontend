(ns parsimony.styles
  (:require [garden.color :as color]
            [garden.def :refer [defstyles]]
            [garden.units :refer [px]]
            [clojure.string :as str]))

(def variable-width-fonts
  (str/join "," ["Segoe UI"
                 "Roboto"
                 "sans-serif"]))

(def monospace-fonts
  (str/join "," ["Source Code Pro"
                 "Consolas"
                 "Inconsolata"
                 "Menlo"
                 "Lucida Console"
                 "Monaco"
                 "Courier New"
                 "Courier"
                 "monospace"]))

(def noselect
  {:-webkit-user-select :none
   :-moz-user-select :none
   :-ms-user-select :none
   :user-select :none})

(def select
  {:-webkit-user-select :text
   :-moz-user-select :text
   :-ms-user-select :text
   :user-select :text})

(def animated
  {:-moz-transition "all 0.1s"
   :-o-transition "all 0.1s"
   :-ms-transition "all 0.1s"
   :-webkit-transition "all 0.1s"
   :transition "all 0.1s"})

(def animated-slow
  {:-moz-transition "all 1.5s"
   :-o-transition "all 1.5s"
   :-ms-transition "all 1.5s"
   :-webkit-transition "all 1.5s"
   :transition "all 1.5s"})

(defstyles screen
  [

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; General Styling
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   [:.draggy-gap animated]

   [:.halo animated]

   [:.draggy-gap.hover
    {:background-color "black"}]

   [:.rc-popover-point
    {:z-index "7 !important" ;; z-index needs to be greater than 6 to avoid codemirror line numbers and card tabs from overlapping
     }]

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Navigation
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   [:#app
    [:div.nav-item
     {:width "150px"
      :line-height "1.3em"
      :padding-left "32px"}

     [:span {:cursor "default"}]

     [:&.major
      {:padding-left "24px"
       :padding-top "6px"
       :font-size "15px"
       :font-weight "bold"}]

     [:&.selected
      {:color "#111"
       :border-right "4px #d0d0d0 solid"
       :background-color "#eaeaea"}]

     [:&.mouseover
      {:background-color "#eaeaea"}]]]

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Tree View
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   [:div.tree-view-header
    (merge noselect {:cursor :pointer})]
   [:div.tree-view-arrow
    (merge
     noselect
     animated
     {:cursor :pointer
      :margin-right "5px"})]

   [:div.tree-view-arrow.collapsed
    {:-webkit-transform "rotate(-90deg)"
     :-moz-transform "rotate(-90deg)"
     :-ms-transform "rotate(-90deg)"
     :transform "rotate(-90deg)"}]

   [:div.tree-view-element
    {:margin-left "3px"
     :padding "2px"}]

   [:div.tree-view-element.selected
    {:background-color "#99b3ff"}]

   [:div.tree-view-element.drag-hovered
    {:border "2px solid black"
     :padding "0px"}]

   [:div.tree-view-children.collapsed
    {:display :none}]

   [:div.tree-view-children.open
    {:border-left "1px dotted #bbb"}]

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Project Picker
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   [:.project-picker
    {:border "1px solid #bbb"}

    [:.loaded-icon
     {:padding "5px"
      :color "green"}]

    [:.project-tree-label
     {:margin "0px 5px 5px 0px"}]

    [:.project-tree-label-details
     {:padding-left "18px"}]

    [:.project-tree-label-details.active
     {:border-left "3px solid green"
      :margin-left "5px"
      :padding-left "10px"}]

    [:.open-project-button
     {:padding "5px"}]


    [:.open-project-button:hover
     :.btn.open
     {:color "white"
      :background-color "rgb(33,150,243)"}]

    [:.detail-tooltip
     {:border "0px"
      :color "inherit"
      :background-color "inherit"
      :text-align "left"
      :padding "0px"
      :margin "0px"
      :font-family monospace-fonts
      :font-size "12px"}]

    [:.source-list-view
     {:background-color "#efefef"
      :font-family monospace-fonts
      :font-size "12px"
      :padding-left "5px"}]]


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; File Picker
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   [:.file-picker
    {:border "1px solid #bbb"}]

   [:div.token-file
    {:color "blue"}]

   [:div.grammar-file
    {:color "purple"}]

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Overlay Picker
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   [:.overlay-picker
    {:border "1px solid #bbb"
     :padding "5px"}

    [:.overlay-tag-item-label
     (merge noselect
            {:cursor "pointer"
             :padding "0 5px 0 5px"})]]

   [:.overlay-picker.empty
    {:background-color "#efefef"
     :flex "auto"}]

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Token Dashboard
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   [:.token-dashboard
    {:border "1px solid #bbb"}

    ;; special styling for code listings inside the token dashboard
    [:code
     {:border "none"
      :font-family monospace-fonts
      :background-color "inherit"}]
    [:pre
     {:border-radius "0px"
      :padding "2px"
      :padding-left "5px"
      :margin "0px"
      :font-family monospace-fonts
      }]

    [:.token-selection-preview
     {:padding "5px"}]

    [:.token-selection-preview.empty
     {:padding "0px"}]

    [:.token-sample-picker
     {:border-right "1px solid #efefef"}]

    [:.token-sample-label
     [:pre
      {:border "none"
       :background-color "inherit"}]]

    [:.token-detail-pane
     {:border-left "1px solid #efefef"
      :padding "5px"}]

    ;; path-view
    [:.token-path-view
     {:border-top "2px solid #efefef"
      :padding-top "10px"
      :position "relative"}

     [:.token-definition
      {:font-family monospace-fonts
       :border-left "3px solid #0072bb"
       :background-color "#e2f4ff"
       :padding-left "5px"}
      [:pre {:border "none"
             :background-color "inherit"}]]

     [:.token-definition-lang
      (merge
       noselect
       {:cursor "default"}
       {:position "absolute"
        :right "0px"
        :top "-2px"
        :color "#0072bb"
        :opacity "0.7"
        })]

     [:.token-examples-view
      {:border-left "3px solid #bbb"
       :padding-left "5px"}

      [:.example-string
       {:margin "2px 0px 2px 0px"}]]

     [:.accept-button
      {:margin-top "2px"
       :padding "3px"
       :cursor "pointer"
       :border "1px solid green"
       :background-color "#d2f1df"}]

     [:.accept-button.hoverme
      {:background-color "#37b56b"
       :color "white"}]]]

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Parse Dashboard
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   [:.parse-dashboard
    {:border "1px solid #bbb"}

    [:.parse-selection-preview
     {:padding "5px"}

     [:.positive-label-button
      {:background-color "green"}]

     [:.positive-label-button.disableme
      {:background-color "#bbb"}]

     [:.negative-label-button
      {:background-color "red"}]

     [:.negative-label-button.disableme
      {:background-color "red"}]

     [:.rc-input-text
      {:flex "auto !important"} ;; override flex: none that is hardcoded on re-com input-text
      ]

     [:pre
      {:border-radius "0px"
       :padding "2px"
       :padding-left "5px"
       :margin "0px"
       :font-family monospace-fonts
       :font-size "12px"}]]

    [:.parse-selection-preview.empty
     {:padding "0px"}]

    [:.parse-sample-picker
     {:border-right "1px solid #efefef"}]

    [:.parse-sample-label
     [:pre
      {:border "none"
       :padding "2px"
       :padding-left "5px"
       :margin "0px"
       :font-family monospace-fonts
       :font-size "12px"
       :background-color "inherit"}]]

    [:.parse-label-picker
     {:border-right "1px solid #efefef"
      :border-left "1px solid #efefef"
      :padding-left "5px"
      }]

    [:.parse-label
     (merge noselect
            {:cursor :pointer
             :font-family monospace-fonts
             :font-size "12px"
             :padding-top "2px"})]

    [:.parse-label.active
     {:background-color "#efefef"}]

    [:.forest-view
     {:padding "5px"}]]

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Quick Add Bar
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   [:.quick-add-bar
    [:.solve-button
     (merge noselect
            {:cursor "pointer"
             :border "1px solid #bbb"
             :border-radius "3px"
             :padding "0px 5px 0px 5px"
             :background-color "white"})]

    [:.solve-button.disableme
     {:pointer-events "none"
      :background-color "#efefef"
      :color "#bbb"}]

    [:.sample-text
     {:flex "auto"
      :background-color "inherit"
      :font-weight "normal"
      :font-size "90%"
      :text-align "left"
      :color "white"
      :padding "0px 0px 0px 5px"
      :margin "5px 0px 0px 10px"
      :border-width "0px 0px 0px 1px"}]]

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Pause Finish Bar
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   [:.pause-finish-bar
    [:.pause-button :.finish-button :.request-assistance-button
     (merge noselect
            {:cursor "pointer"
             :border "1px solid #bbb"
             :border-radius "3px"
             :padding "0px 5px 0px 5px"
             :background-color "white"})]

    [:.disableme
     {:pointer-events "none"
      :background-color "#efefef"
      :color "#bbb"}]]

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Parse Forest View
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   [:.parse-forest-view

     [:.overlay-tag
      {:font-family variable-width-fonts
       :border-radius "0px"
       :border "0px"
       :background-color "transparent"
       :padding "2px"
       :padding-right "8px"
       :margin "0px"
       }]

     [:.string
      {:font-family monospace-fonts
       :font-size "12px"
       :padding "0px"
       :border-radius "0px"
       :border "0px"
       :background-color "inherit"
       :margin "0px"}]

     ;;------------------------------------------------------------------------------
     ;; Forest Pane
     ;;------------------------------------------------------------------------------

     [:.forest-pane

      [:.node-label-buttons
       {:background-color "white"
        :font-size "16px"
        :color "white"
        :padding "2px"
        :opacity "0"}

       [:.positive-label-button
        {:background-color "#7eb046"}]

       [:.negative-label-button
        {:background-color "#de5233"}]]

      [:.node-label-buttons.hoverme :.node-label-buttons.flairme :.node-label-buttons.lockme
       {:opacity "1"}]

      [:.node-label-buttons.lockme
       {:pointer-events "none"}]

      [:.node-label-button
       {:border "2px solid rgba(0,0,0,0)"
        :border-radius "3px"
        :width "20px"
        :opacity "0"}]

      [:.node-label-buttons.hoverme
       [:.node-label-button.idleme
        {:opacity "0.3"}]

       [:.node-label-button.hoverme
        {:opacity "1"
         :margin "0"
         :border "2px solid #3498db"}]]

      [:.node-label-button.flairme
       {:opacity "1"}]

      [:.examine-button
       {:cursor :pointer
        :margin "0"
        :padding "0"}
       [:i
        {:background-color "white"
         :color "#3498db"
         :font-size "20px"}]]

      [:.node
       (merge noselect
              animated
              {:cursor :pointer
               :border-radius "0px"
               :border-width "1px"
               :border-style "solid"
               :background "white"})]

      [:.node.ambiguous
       {:border-radius "5px"
        :border-width "2px"
        :border-style "dashed"}]

      [:.node.inactive
       {:pointer-events "none"
        :opacity "0.3"}]

      [:.node.dummy
       {:border-radius "8px"
        :border-width "2px"
        :border-style "solid"
        :width "16px"
        :height "16px"
        }]

      [:.node-text-view
       {:padding-right "9px"}]

      [:.edge
       (merge
        animated
        {:fill "none"
         ;; :stroke "black"
         :stroke-width "3px"
         :stroke-linecap "square"
         })]

      [:.edge.inactive
       {:stroke-opacity "0.1"}
       ]]

     ;;------------------------------------------------------------------------------
     ;; Forest Stack View
     ;;------------------------------------------------------------------------------

     [:.forest-stack-view

      [:.option-view
       {:padding-bottom "5px"
        :border-bottom "1px solid #efefef"}]

      [:.forest-crumbs
       {:padding-bottom "5px"
        :border-bottom "1px solid #efefef"
        :opacity "0.7"}]

      [:.forest-crumbs.hoverme
       {:opacity "1"}]

      [:.crumb
       (merge noselect
              {:cursor :pointer
               :border-width "1px"
               :border-style "solid"
               :background "white"})]

      [:.crumb-text
       {:padding-right "2px"}]

      [:.crumb-separator
       {:font-size "20px"}]]]

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Live Parse View
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   [:.live-parse-view.inactive
    (merge noselect
           {:cursor "default"})]

   [:.live-parse-view
    {:border "1px solid #bbb"}]

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Solver View
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   [:.solver-view

    [:.help
     {:padding "0px 0px 10px 0px"}]

    [:.help-toggle-button
     {:cursor "pointer"}]

    [:.help-toggle-button.hoverme
     {:color "rgb(33,150,243)"}]

    [:.precedence-help
     {:color "#999"
      :margin "0px"
      :padding "0px 0px 0px 15px"
      :border-left "2px solid #bbb"
      :border-radius "15px"}]

    [:.sym
     (merge noselect
            {:font-family monospace-fonts
             :font-weight "normal"
             :cursor "pointer"
             :padding "2px"
             :min-width "30px"
             :text-align "center"
             :border-width "1px"
             :margin "1px"
             :color "black"})]

    [:.lhs-sym
     (merge noselect
            {:font-family variable-width-fonts
             :cursor "pointer"
             :padding "2px"
             :min-width "30px"
             :text-align "center"
             :border-right-width "1px"})]

    [:.rhs-view
     {:padding "3px"}]

    [:.production-view
     {:border-width "1px"
      :border-style "solid"
      :color "black"}]

    [:.inset
     {:border-left "2px solid #bbb"
      :margin-left "10px"
      :padding-left "5px"}]

    [:.inset-more
     {:border-left "2px solid #bbb"
      :margin-left "40px"
      :padding-left "5px"}]

    [:.heading-label
     {:font-size "110%"
      :font-weight "bold"
      :line-height "150%"
      :padding "2px"}]

    [:.solutions-view
     [:.accepted-heuristics-view
      {:background-color "#efefef"
       :padding "0px 5px 10px 5px"}]

     [:.production-candidates-view
      {:padding "5px"}]

     [:.preview-pane
      {:padding "5px"}]]

    [:.heuristics-view
     {:border "1px solid #bbb"
      :padding "5px"}]

    [:.idle-view
     {:border "1px solid #bbb"}]]

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Disambiguation
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   [:.fragment-view
    (merge noselect
           {:cursor "default"
            :border-width "1px"
            :border-style "solid"
            :padding-right "5px"
            :background-color "white"})
 
    [:.overlay-tag
     {:font-family variable-width-fonts
      :border-radius "0px"
      :border "0px"
      :background-color "transparent"
      :padding "2px"
      :padding-right "8px"
      :margin "0px"
      }]
 
    [:.string
     {:font-family monospace-fonts
      :font-size "12px"
      :padding "0px"
      :border-radius "0px"
      :border "0px"
      :background-color "transparent"
      :margin "0px"}]
 
    [:.fragment-piece-view
     {:border-width "1px"
      :border-style "solid"
      :margin "5px 0px 5px 0px"
      :padding-left "5px"
      :padding-right "5px"}]]

   [:.disambiguation-view

    {:border "1px solid #bbb"
     :padding "5px"}

    [:.help

     [:.help-message
      {:border-left "2px solid #bbb"
       :padding-left "10px"
       :margin-left "5px"}]

     [:.toggle-button
      {:cursor "pointer"}]

     [:.toggle-button.hoverme
      {:color "rgb(33,150,243)"}]]

    [:.disambiguation-candidate-view
     (merge
       noselect
       {:cursor "pointer"
        :border "2px solid pink"
        :padding "5px"})]

    [:.disambiguation-candidate-view.hoverme
     {:border "2px solid red"
      :padding "5px"}]

    [:.disambiguation-candidate-view.selectme
     {:background-color "pink"}]

    [:.lass-view :.rass-view :.prio-view :.pref-view
     {:border-left "2px solid #bbb"
      :padding-left "10px"}
     [:span
      {:font-family monospace-fonts
       :font-size "12px"
       :color "black"}]]

    [:.no-disambiguation-view
     (merge noselect
            {:cursor :pointer
             :padding "10px"})]

    [:.no-disambiguation-view:hover
     {:color "red"}]]

   [:.disambiguation-view.empty
    {:background-color "#efefef"
     :flex "auto"}]

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Log
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   [:.log-view

    {:border "1px solid #bbb"}

    [:.entries-view
     {:font-family monospace-fonts
      :font-size "12px"}]

    [:.glyph
     {:position "absolute"
      :left "0px"
      :top "0px"
      :bottom "0px"
      :width "20px"}
     [:i
      {:font-size "18px"
       :padding-top "2px"
       :padding-left "2px"}]]

    [:.glyph.success
     {:background-color "#3bab4a"}
     [:i
      {:color "#def4e1"}]]

    [:.glyph.error
     {:background-color "#a94442"}
     [:i
      {:color "#f2dede"}]]

    [:.glyph.warn
     {:background-color "#8a6d3b"}
     [:i
      {:color "#fcf8e3"}]]

    [:.glyph.info
     {:background-color "#bbb"}
     [:i
      {:color "#ffffff"}]]

    [:.timestamp-view
     (merge noselect
            {:position "absolute"
             :right "2px"
             :top "2px"
             :font-family variable-width-fonts
             :font-size "11px"
             :color "#555"})]

    [:.entry-view
     {:padding-left "25px"
      :padding-top "5px"
      :position "relative"
      :border-bottom-width "1px"
      :border-style "solid"}]

    [:.entry-view.success
     {:background-color "#def4e1"
      :border-color "#c1e9c6"}]

    [:.entry-view.error
     {:background-color "#f2dede"
      :border-color "#ebccd1"}]

    [:.entry-view.warn
     {:background-color "#fcf8e3"
      :border-color "#faebcc"}]

    [:.entry-view.info
     {:background-color "#ffffff"
      :border-color "#efefef"}]

    [:div.lexer-failure
     {:padding-left "15px"}]

    [:div.token-list
     {:padding "5px 0px 5px 15px"
      :display "flex"
      :flex-wrap "wrap"}]

    [:span.token
     {:border "1px solid #bbb"
      :margin "1px"
      :display "flex"}

     [:span.token-label
      {:font-family monospace-fonts
       :padding "2px"
       :background-color "#efefef"
       :border-right "1px solid #bbb"}]

     [:span.token-string
      {:padding "2px"
       :background-color "white"}]]

    [:div.parser-failure
     {:padding-left "15px"}

     [:ul
      {:padding-left "15px"}]]

    [:pre.parse-forest
     {:font-size "11px"
      :border-radius "0px"
      :margin "5px 0px 5px 15px"}]

    [:code
     {:padding "1px"
      :background-color "white"}]

    [:div.test-failure
     [:pre
      {:margin-left "15px"
       :font-size "11px"
       :margin-top "5px"}]]]

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Workspace
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   [:.workspace
    [:.layout
     {:margin "0px 5px 0px 5px"}]]

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Menu
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   [:.menubar
    {:background-color "#efefef"
     :border-bottom "1px solid #bbb"
     :padding "5px 0px 5px 10px"}]


   [:div.menu-view-header
    (merge noselect {:cursor :pointer})]

   [:.menu-view
    {:z-index "7"}
    [:.popover
     {:border-radius "0px"}]

    [:.menu-view-header
     :.menu-view-element
     {:width "100%"}]]

   [:.menu-label
    [:i {:color "#a0a0a0"}]]

   [:.menu-label.disableme
    {:pointer-events "none"
     :color "#bbb"}]

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Comm Status
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   [:.comm-status
    (merge noselect
           {:cursor "default"
            :padding "0px 5px 0px 5px"
            :margin-right "5px"})

    [:.in-flight
     ;; purple
     {:color "#af00af"}]

    [:.needs-attention
     {:border-bottom "1px solid red"}]]

   [:.comm-status.offline
    {:background-color "#a94442"
     :color "#f2dede"}]

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Comm Status Modal
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   [:.comm-status-modal
    {:max-height "600px"}

    [:.comm-detail-pane
     {:font-family monospace-fonts
      :font-size "12px"}

     [:th
      {:font-weight "bold"}]
     [:th :td
      {:padding "2px"
       :text-align "left"
       :border "1px solid #bbb"}]]]

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Ribbon
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   [:.ribbon
    {:border-style "solid"
     :border-color "#eee"
     :border-width "1px 1px 1px 0px"
     :background-color "white"}]

   [:.ribbon-button
    (merge noselect
           {:cursor :pointer
            :background-color "white"})]

   [:.ribbon-button.hover
    {:color "rgb(33,150,243)"}]

   [:.ribbon-item.disableme
    {:pointer-events "none"
     :color "#bbb"}]

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Modebar
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   ;; using box-shadow to lighten an image
   ;; see http://stackoverflow.com/a/26073962/128927
   [:.mode-button
    (merge noselect
           {:cursor :pointer
            :color "#ffffff"
            :border "none"
            :border-radius "0px"
            :height "35px"
            :width "110px"
            :font-size "17px"
            :font-weight "300"
            :transform "translateY(100px) rotate(270deg)"
            :transform-origin "left top 0"
            :box-shadow "inset 0 0 100px 100px rgba(255,255,255,0.7)"
            })]

   [:.mode-button.active
    {:box-shadow "none"}
    ]

   [:.mode-button.hover
    {:color "black"}]

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Statusbar
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   [:.statusbar
    (merge noselect
           {:cursor "default"
            :font-family monospace-fonts
            :font-size "12px"
            :margin "0px 5px 0px 5px"})

    [:.progress-monitor.showme
     (merge animated
            {:opacity "1"})]

    [:.progress-monitor.hideme
     (merge animated-slow
            {:opacity "0"})]

    [:.progress-bar
     {:background-color "#2c8fe4"}]

    [:.progress.success
     [:.progress-bar
      {:background-color "#3bab4a"}]]

    [:.progress.failure
     [:.progress-bar
      {:background-color "#ea595f"}]]

    [:i.success
     {:color "#3bab4a"
      :font-size "20px"}]

    [:i.failure
     {:color "#ea595f"
      :font-size "20px"}]

    [:.progress-monitor
     [:.progress-description
      [:a {:text-decoration "underline"
           :cursor "pointer"}]]]

    [:.last-command-monitor
     [:.origin
      {:font-size "12px"
       :color "#bbb"}]

     [:.command
      {:color "white"
       :text-shadow "0 1px 1px #111111"
       :background-color "#bbb"
       :padding "0px 3px 0px 3px"
       :border-radius "3px"
       :border "1px solid #777"}]]

    [:.last-command-monitor.showme
     (merge animated
            {:opacity "1"})]

    [:.last-command-monitor.hideme
     (merge animated-slow
            {:opacity "0"})]]

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Tabs
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   [:.tabber
    {:z-index "6"}] ;; needs to be higher than 5 to prevent CodeMirror line numbers from bleeding through

   [:.tabber.empty
    (merge animated
           {:height "21px"
            :border-bottom "1px solid #d0d0d0"})]

   [:.tabber.hover
    {:background-color "#eaeaea"}]

   [:.tabber-drag-box
    {:background-color "white"}]

   [:.tabbed-pile.empty
    {:border "1px solid #d0d0d0"}

    [:.placeholder
     (merge
      noselect
      {:cursor "default"
       :background-color "#efefef"})]]

   ;; scrollbar styling, only works in webkit

   [".tabber-scroller::-webkit-scrollbar"
    {:width "4px"
     :border-radius "0px"
     :background-color "inherit"}]

   [".tabber-scroller::-webkit-scrollbar:horizontal"
    {:height "4px"}]

   [".tabber-scroller::-webkit-scrollbar:hover"
    {:background-color "rgba(0,0,0,0.05)"}]

   [".tabber-scroller::-webkit-scrollbar-thumb:horizontal"
    ".tabber-scroller::-webkit-scrollbar-thumb:vertical"
    {:background "rgba(0,0,0,0.25)"
     :border-radius "0px"}]

   [".tabber-scroller::-webkit-scrollbar-thumb:horizontal:active"
    ".tabber-scroller::-webkit-scrollbar-thumb:vertical:active"
    {:background "rgba(0,0,0,0.45)"}]

   [:.tab
    (merge {:cursor :pointer
            :padding "0 5px 0 7px"
            :background-color "#eaeaea"
            :color "#a0a0a0"
            :border-top "1px solid #c0c0c0"
            :border-left "1px solid #a0a0a0"
            :border-right "1px solid #b0b0b0"
            }
           animated
           noselect)]

   [:.tab.selected
    {:color "black"
     :background-color "white"}]

   [:.tab.focused
    {:color "black"
     :background-color "#b4d7f6"}]

   [:.tab.hover
    {:border-top "1px solid blue"}]

   [:.tab.disableme
    {:pointer-events "none"
     :color "#c0c0c0"
     :background "repeating-linear-gradient(
                  45deg,
                  #e5e5e5,
                  #e5e5e5 2px,
                  #fafafa 2px,
                  #fafafa 4px)"
     :border-top "1px solid #d0d0d0"
     :border-left "1px solid #d0d0d0"
     :border-right "1px solid #d0d0d0"}]

    [:.tab
     [:.error-icon
      {:color "rgb(193,0,32)"
       :font-size "16px"}]]

    [:.tab-content.disableme
     {:pointer-events "none"
      :border "1px solid #bbb"
      :background-color "#efefef"}]

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Overlays
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   ;; :right and :bottom must be specified to prevent scroll offset from causing
   ;; the right and bottom edges bleed out past the editor bounding box
   [:div.overlays
    {:position "absolute"
     :right "0px"
     :bottom "0px"
     :z-index "3" ;; higher than the z-index of textbox content
     :pointer-events "none"}]

   ;; this is the overlay that sits over the codemirror gutter
   ;; it is essentially the same thing as an overlay, but positioned differently
   [:div.gutter-overlays
    {:position "absolute"
     :bottom "0px"
     :z-index "5"}] ;; needs to be higher than z-index of any codemirror gutter component

   [:div.overlay
    {:position "absolute"
     :top "0px"
     :left "0px"
     :right "0px"
     :bottom "0px"
     }]

   [:div.svg-parent
    [:svg
     {:width "100%"
      :height "100%"}

     [:polygon
      {:stroke-width "1px"}
      ]]]

   [:div.detail-parent
    {:position "absolute"
     :top "0px"
     :left "0px"
     :right "0px"
     :bottom "0px"}]

   [:div.detail-content
    {:font-family monospace-fonts
     :font-size "12px"
     :pointer-events "auto"}]

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Editor
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   ;; The container for a codemirror component and its corresponding overlays.
   [:div.editor
    {:position "relative" ;; ensures that child div.codemirror-parent is positioned relative to this instead of any ancestor
     :overflow "hidden" ;; ensures that the overlays do not appear to float outside the editor bounding box when scrolling.
     }]

   #_[:div.editor.focused
    {:border "solid rgb(33,150,243)"
     :border-width "0px 4px 4px 0px"}]

   [:.line-col-indicator
    (merge noselect
           {:cursor "default"
            :position "absolute"
            :bottom "2px"
            :left "2px"
            :z-index "5" ;; ensure visibility above gutter
            :font-family monospace-fonts
            :font-size "10px"
            :padding "2px"
            :background-color "#efefef"
            :border-radius "3px"
            :border "1px solid #bbb"})]


   [:.token-editor
    :.cfg-editor
    :.sample-editor
    {:border "1px solid #bbb"}]

   [:.contextbar.empty
    {:padding "0"
     :border "0"}]

   [:.contextbar
    {:border-top "1px solid #bbb"
     :padding "2px"}

    [:.string
     (merge noselect
            {:cursor :pointer
             :padding "0px 5px 0px 5px"})]]

   ;; This is the direct parent of a .CodeMirror div. It is positioned
   ;; absolutely and is NOT a flexbox because CodeMirror sizing doesn't
   ;; work correctly with flexbox in Chrome 46.0.2490.86. This style forces
   ;; codemirror-parent to fill its direct parent.
   ;; **Do not mess with any of the positioning specified by this style.**
   [:div.codemirror-parent
    {:position "absolute"
     :left 0
     :top 0
     :right 0
     :bottom 0}

    ;; grey special characters
    [:span.cm-whitespace
     {:color "#00bfff"}]
    [:div.CodeMirror-code
     ["pre.CodeMirror-line:not(:last-child)::after"
      "pre.CodeMirror-line > span::after"
      {:color "#00bfff"}]]

    ;; block-style cursor
    ;; see https://groups.google.com/forum/#!topic/light-table-discussion/kDUmS_h9BF0
    [:.CodeMirror-cursor
     {:width "auto"
      :border "1px solid black"
      :background "rgba(0, 0, 0, 0.2)"}]

    [:.CodeMirror-focused
     [:.CodeMirror-cursor
      {:width "0"
       :border-left "2px solid black"
       :border-right "none"}]]

    ;; the codemirror divs always fill 100% of their parent element all sizing
    ;; is done by resizing the parent, not these divs
    [:.CodeMirror
     {:font-family monospace-fonts
      :font-size "12px"
      :width "100%"
      :height "100%"}]

    [:.CodeMirror
     [:pre
      {:line-height "1.8"}]]

    [:&.disabled
     [:.CodeMirror
      {:background-color "#eee"}]]]])

(defstyles reset
  [[:html :body :div :span :applet :object :iframe :h1 :h2 :h3 :h4 :h5 :h6 :p
    :blockquote :pre :a :abbr :acronym :address :big :cite :code :del :dfn :em
    :img :ins :kbd :q :s :samp :small :strike :strong :sub :sup :tt :var :b :u :i
    :center :dl :dt :dd :ol :ul :li :fieldset :form :label :legend :table
    :caption :tbody :tfoot :thead :tr :th :td :article :aside :canvas :details
    :embed :figure :figcaption :footer :header :hgroup :menu :nav :output :ruby
    :section :summary :time :mark :audio :video
    {:margin 0
     :padding 0
     :border 0
     :font-size "100%"
     :font :inherit
     :vertical-align :baseline
     }]

   [:article :aside :details :figcaption :figure
    :footer :header :hgroup :menu :nav :section
    {:display :block}]

   [:body
    {:line-height 1}]

   [:ol :ul
    {:list-style :none}]

   [:blockquote :q
    {:quotes :none}
    [:&:before :&:after
     {:content :none}
     {:content "\"\""}]]

   [:table
    {:border-collapse :collapse
     :border-spacing 0}]])
