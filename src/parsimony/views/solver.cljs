(ns parsimony.views.solver
  (:require [re-com.core :refer [gap hyperlink line label alert-box h-box v-box v-split scroller popover-tooltip] :refer-macros [handler-fn]]
            [re-frame.core :refer [dispatch subscribe]]
            [reagent.core :as reagent]
            [reagent.ratom :refer-macros [reaction]]
            [parsimony.com.box :as box]
            [parsimony.com.buttons :refer [button]]
            [parsimony.heuristic :as heuristic]
            [parsimony.inference :as inference]
            [parsimony.lexer :as lexer]
            [parsimony.models.colors :refer [decoration->html-css]]
            [parsimony.models.parse-dashboard :as parse-dashboard]
            [parsimony.models.solver :as solver]
            [parsimony.parser :as parser]
            [parsimony.util :refer [pprint-str set-drag-data get-drag-data set-drop-effect]]
            [parsimony.views.common :refer [icon]]
            [parsimony.views.parse-forest :refer [label->decoration] :as parse-forest]
            [parsimony.views.ribbon :refer [ribbon] :as ribbon]
            [parsimony.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; debug-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn debug-view []
  (let [state (subscribe [:solver/ux-state])]
    (fn []
      [scroller
       :size "auto"
       :style {:border "1px solid red"}
       :child [:pre {:style {:font-size "10px"
                             :border "0px"
                             :background-color "white"}}
               (pprint-str @state)]])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; idle-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn idle-view []
  [v-box
   :size "auto"
   :class "idle-view"
   :children []])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nothing-to-solve-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn nothing-to-solve-view []
  [alert-box
   :style {:flex "auto"
           :margin "0px"
           :border-radius "0px"}
   :alert-type :info
   :heading [:span
             [icon {:md-icon-name "zmdi-check-circle"}]
             " Grammar satisfies all parse labels"]
   :body [:span "No additional modification to the grammar is necessary."]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; heuristics-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn unknown-heuristic [heuristic]
  [v-box
   :children [[label :label "Unknown heuristic:"]
              [:pre (pprint-str heuristic)]]])

(defn choice-view [sym choice on-click]
  (let [decoration-map (subscribe [:affinity-decoration-map])
        decorations (subscribe [:decorations])]
    (fn [sym choice on-click]
      (let [css (-> sym
                    (label->decoration @decoration-map @decorations)
                    (decoration->html-css))
            chosen? (= sym choice)]
        [:span.sym {:style (merge css
                                     (if chosen?
                                       {:font-weight "bold"
                                        :border-width "2px"
                                        :margin "0px"}
                                       {:border-width "1px"
                                        :margin "1px"
                                        :opacity "0.5"}))
                       :on-click (handler-fn
                                   (when-not chosen?
                                     (on-click sym)))}
         (name (parser/->nonterminal sym))]))))

(defn- comma-separate
  ([xs]
   (comma-separate "," "or" xs))
  ([comma-str conjunction-str xs]
   (if (> (count xs) 1)
     (-> []
         (into (interpose [:span comma-str]) (drop-last xs))
         (conj [:span conjunction-str])
         (conj (last xs)))
     (vec xs))))

(defn sym-choice-selector [syms choice on-click]
  [h-box
   :align :baseline
   :gap "2px"
   :children
   (->> (sort syms)
        (map #(vector choice-view % choice on-click))
        (comma-separate))])

(defn encloser-choice-selector [enclosers choice on-click]
  (letfn [(pair-elem [encloser]
            [h-box
             :gap "5px"
             :children
             [[choice-view (first encloser) (first choice) #(on-click encloser)]
              [choice-view (second encloser) (second choice) #(on-click encloser)]]])]
    [h-box
     :gap "2px"
     :children (into []
                 (map (partial vector pair-elem))
                 (sort enclosers))]))

(defn sym-view [sym string]
  (let [decoration-map (subscribe [:affinity-decoration-map])
        decorations (subscribe [:decorations])
        tooltip-showing? (reagent/atom false)]
    (fn [sym string]
      (let [css (-> sym
                    (label->decoration @decoration-map @decorations)
                    (decoration->html-css))]
        [popover-tooltip
         :label (name (parser/->nonterminal sym))
         :position :below-center
         :showing? tooltip-showing?
         :anchor [:span.sym {:style css
                             :on-mouse-over (handler-fn (reset! tooltip-showing? true))
                             :on-mouse-out (handler-fn (reset! tooltip-showing? false))}
                  string]]))))

(defn static-sym-view [sym]
  (let [decoration-map (subscribe [:affinity-decoration-map])
        decorations (subscribe [:decorations])]
    (fn [sym]
      (let [css (-> sym
                    (label->decoration @decoration-map @decorations)
                    (decoration->html-css))]
        [:span.sym {:style css}
         (name (parser/->nonterminal sym))]))))

(defn concise-static-sym-list [syms]
  [h-box
   :align :baseline
   :gap "2px"
   :children
   (->> (sort syms)
        (map (partial vector static-sym-view)))])

(defn verbose-static-sym-list [syms]
  [h-box
   :align :baseline
   :gap "2px"
   :children
   (->> (sort syms)
        (map (partial vector static-sym-view))
        (comma-separate "," "and"))])

;;------------------------------------------------------------------------------
;; List Heuristics
;;------------------------------------------------------------------------------

(defn enclosed-list-previewer [elem sep encloser strings]
  (letfn [(string->sym-view [elem sep encloser max-idx idx string]
            (cond
              ;; left encloser
              (= idx 0)
              [sym-view (first encloser) string]

              ;; right encloser
              (= idx max-idx)
              [sym-view (second encloser) string]

              ;; elem
              (odd? idx)
              [sym-view elem string]

              ;; separator
              (even? idx)
              (if sep
                [sym-view sep string]
                [sym-view elem string])))]
    [h-box
     :class "inset"
     :children
     (into []
       (map-indexed (partial string->sym-view elem sep encloser (dec (count strings))))
       strings)]))

(defn unenclosed-list-previewer [elem sep strings]
  (letfn [(string->sym-view [elem sep max-idx idx string]
            (cond
              ;; elem
              (even? idx)
              [sym-view elem string]

              ;; separator
              (odd? idx)
              (if sep
                [sym-view sep string]
                [sym-view elem string])))]
    [h-box
     :class "inset"
     :children
     (into []
       (map-indexed (partial string->sym-view elem sep (dec (count strings))))
       strings)]))

(defn list-heuristic-previewer [elem sep encloser heuristic]
  (if encloser
    [v-box
     :gap "3px"
     :children (into []
                     (map (partial vector enclosed-list-previewer elem sep encloser))
                     (:path-strings heuristic))]
    [v-box
     :gap "3px"
     :children (into []
                     (map (partial vector unenclosed-list-previewer elem sep))
                     (:path-strings heuristic))]))

(defn- list-heuristic-view [heuristic]
  (let [elem-choice (reagent/atom (first (get-in heuristic [:params :elem])))
        sep-choice (reagent/atom (first (get-in heuristic [:params :sep])))
        encloser-choice (reagent/atom (first (get-in heuristic [:params :encloser])))]
    (fn [{:keys [provenance] :as heuristic}]
      (let [{:keys [elem sep encloser]} (:params heuristic)
            lhs-nt (str (name (first (inference/extract-provenance-syms provenance))))]
        [v-box
         :gap "5px"
         :children
         [[h-box
           :gap "3px"
           :align :baseline
           :children
           [[label
             :class "heading-label"
             :label
             [:span
              "You applied label "
              [static-sym-view lhs-nt]
              (if (> (count provenance) 1)
                " to the following strings."
                " to the following string.")]]]]
          [list-heuristic-previewer @elem-choice @sep-choice @encloser-choice heuristic]
          [label
           :class "heading-label"
           :label
           (if (> (count provenance) 1)
             "It looks like these strings should be parsed as sequences with the following form:"
             "It looks like this string should be parsed as a sequence with the following form:")]
          [v-box
           :gap "3px"
           :children
           [[h-box
             :class "inset"
             :align :baseline
             :children
             [[label :label "Each element is an instance of"]
              [gap :size "5px"]
              [sym-choice-selector elem @elem-choice #(reset! elem-choice %)]
              [gap :size "2px"]
              [:span
               (if (> (count elem) 1)
                 "(please select one)."
                 ".")]]]
            (when (seq sep)
              [h-box
               :class "inset"
               :align :baseline
               :children
               [[label :label "The separator between elements is an instance of"]
                [gap :size "5px"]
                [sym-choice-selector sep @sep-choice #(reset! sep-choice %)]
                [gap :size "2px"]
                [:span
                 (if (> (count sep) 1)
                   "(please select one)."
                   ".")]]])
            (when (seq encloser)
              [h-box
               :class "inset"
               :align :baseline
               :children
               [[label :label "The entire list is surrounded by"]
                [gap :size "5px"]
                [encloser-choice-selector encloser @encloser-choice #(reset! encloser-choice %)]
                [gap :size "2px"]
                [:span
                 (if (> (count encloser) 1)
                   "(please select one)."
                   ".")]]])]]
          [gap :size "5px"]
          [h-box
           :gap "5px"
           :children [[button
                       :label "Accept these choices"
                       :attr {:on-mouse-down (handler-fn (.preventDefault event))}
                       :on-click #(dispatch [:solver/accept-heuristic
                                             (assoc heuristic
                                                    :params
                                                    (merge {:elem @elem-choice}
                                                           (when (some? @sep-choice)
                                                             {:sep @sep-choice})
                                                           (when (some? @encloser-choice)
                                                             {:encloser @encloser-choice})))])]
                      [button
                       :label
                       (if (> (count provenance) 1)
                         "No, these are not lists"
                         "No, this is not a list")
                       :attr {:on-mouse-down (handler-fn (.preventDefault event))}
                       :on-click #(dispatch [:solver/reject-heuristic heuristic])]]]
          #_[:pre (pprint-str heuristic)]]]))))

;;------------------------------------------------------------------------------
;; Expression Heuristics
;;------------------------------------------------------------------------------

(defn- expression-previewer [lhs-nt classification path segments]
  (letfn [(->sym-view [c p s]
            (if (= :elem c)
              [sym-view lhs-nt s]
              [sym-view (first (filter parser/terminal? p)) s]))]
    [h-box
     :class "inset"
     :children (vec (map ->sym-view classification path segments))]))

(defn- expression-heuristic-previewer [heuristic]
  [v-box
   :gap "3px"
   :children (into []
                   (map (partial vector
                                 expression-previewer
                                 (get-in heuristic [:params :lhs-nt])
                                 (get-in heuristic [:params :classification])
                                 (:path heuristic)))
                   (:path-strings heuristic))])

(defn- assoc-choice-view [choice on-click sym]
  (let [css {:border-color "black"
             :border-style "solid"}
        chosen-css {:font-weight "bold"
                    :border-width "2px"
                    :margin "0px"}
        unchosen-css {:border-width "1px"
                      :margin "1px"
                      :opacity "0.5"}]
    [h-box
     :gap "1px"
     :children [[:span.sym {:style (merge css (if (= choice :left) chosen-css unchosen-css))
                            :on-click (handler-fn (on-click sym :left))} "left"]
                [:span.sym {:style (merge css (if (= choice :right) chosen-css unchosen-css))
                            :on-click (handler-fn (on-click sym :right))} "right"]]]))

(defn- operator-associativity-view [choices on-click heuristic]
  (let [{:keys [op]} (:params heuristic)]
    [h-box
     :style {:padding-left "20px"}
     :gap "1px"
     :children
     [[v-box
       :gap "2px"
       :children (into []
                       (map (partial vector static-sym-view))
                       op)]
      [v-box
       :gap "2px"
       :children (into []
                       (map (fn [sym]
                              (vector assoc-choice-view
                                      (get @choices sym)
                                      on-click
                                      sym)))
                       op)]]]))

(defn precedence-sym-view [sym]
  (let [decoration-map (subscribe [:affinity-decoration-map])
        decorations (subscribe [:decorations])]
    (fn [sym]
      (let [css (-> sym
                    (label->decoration @decoration-map @decorations)
                    (decoration->html-css))]
        [:span.sym {:style css
                    :draggable true
                    :on-drag-start
                    (handler-fn
                      (doto event
                        (.stopPropagation event) ;; prevent parent precedence-group-view from handling drag-start
                        (set-drag-data ::precedence
                                       {:syms [sym]})))}
         (name (parser/->nonterminal sym))]))))

(defn- precedence-group-view [choices on-drop precedence-level]
  (let [hover (reagent/atom nil)]
    (fn [choices on-drop precedence-level]
      (let [my-syms (into []
                          (for [[sym precedence] @choices
                                :when (= precedence-level precedence)]
                            sym))]
        [h-box
         :style (merge {:padding "5px"
                        :border "1px solid #bbb"
                        :margin "1px"
                        :background-color "#efefef"}
                       (when @hover
                         {:border "2px solid black"
                          :margin "0px"}))
         :attr {:draggable true
                :on-drag-start
                (handler-fn
                  (doto event
                    (set-drag-data ::precedence
                                   {:syms my-syms})))
                :on-drag-over
                (handler-fn
                  (.preventDefault event)
                  (.stopPropagation event)
                  (reset! hover true)
                  (set-drop-effect event :copy))
                :on-drop
                (handler-fn
                  (reset! hover nil)
                  (let [{:keys [syms]} (get-drag-data event ::precedence)]
                    (on-drop syms (inc (* 2 precedence-level)))))
                :on-drag-enter
                (handler-fn
                  (reset! hover true))
                :on-drag-leave
                (handler-fn
                  (reset! hover nil))}
         :children (into []
                         (map (partial vector precedence-sym-view))
                         (sort my-syms))]))))

(defn- operator-precedence-view [choices on-drop heuristic]
  (let [{:keys [op]} (:params heuristic)]
    [box/draggy-v-box
     {:style {:padding-left "20px"}
      :gap "3px"
      :gap-color "black"
      :valid-drag-type-fn (fn [drag-types i]
                            (contains? drag-types ::precedence))
      :on-gap-drop (fn [data idx]
                     (when-let [syms (get-in data [::precedence :syms])]
                       (on-drop syms (* 2 idx))))
      :children (into []
                      (map (partial vector precedence-group-view choices on-drop))
                      (sort (distinct (vals @choices))))}]))

(defn- reindex-precedences [m]
  (let [gs (group-by second m)
        result (reduce
                 (fn [[m n] [_ xs]]
                   [(into m (map #(vector (first %) n)) xs)
                    (inc n)])
                 [{} 0]
                 (sort-by first gs))]
    (first result)))

(defn- insert-precedence [choices syms idx]
  (if (seq syms)
    (-> {}
        (into (map #(vector % idx)) syms)
        (into (map (fn [[k v]]
                     [k (inc (* 2 v))]))
              (apply dissoc choices syms))
        (reindex-precedences))
    choices))

(defn- expression-heuristic-view [heuristic]
  (let [associativity-choices (reagent/atom (get-in heuristic [:params :associativity]) )
        precedence-choices (reagent/atom (-> (get-in heuristic [:params :precedence])
                                             (reindex-precedences)))]
    (fn [{:keys [provenance] :as heuristic}]
      (let [{:keys [lhs-nt op paren]} (:params heuristic)]
        [v-box
         :gap "5px"
         :children
         [[h-box
           :gap "3px"
           :align :baseline
           :children
           [[label
             :class "heading-label"
             :label
             [:span
              "You applied label "
              [static-sym-view lhs-nt]
              (if (> (count provenance) 1)
                " to the following strings."
                " to the following string.")]]]]
          [expression-heuristic-previewer heuristic]
          [label
           :class "heading-label"
           :label
           (if (> (count provenance) 1)
             "It looks like these strings should be parsed as algebraic expressions with the following form:"
             "It looks like this string should be parsed as an algebraic expression with the following form:")]
          [v-box
           :gap "10px"
           :children
           [[h-box
             :class "inset"
             :gap "3px"
             :align :baseline
             :children
             [[verbose-static-sym-list op]
              [label :label (if (> (count op) 1)
                              "are binary operators."
                              "is a binary operator.")]]]
            [v-box
             :class "inset"
             :gap "2px"
             :align :start
             :children
             [[label :label
               (if (> (count @associativity-choices) 1)
                 "The operators have the following associativities (please select for each operator):"
                 "The operator has the following associativity (please select):")]
              [operator-associativity-view
               associativity-choices
               (fn [sym choice]
                 (console/debug :on-click {:sym sym :choice choice})
                 (swap! associativity-choices assoc sym choice))
               heuristic]]]
            (when (> (count @precedence-choices) 1)
              [v-box
               :class "inset"
               :align :start
               :children
               [[label :label "The operators have the following precedences:"]
                [h-box
                 :gap "20px"
                 :min-height "90px"
                 :align :center
                 :children
                 [[operator-precedence-view
                   precedence-choices
                   (fn [syms idx]
                     (console/debug :on-drop {:syms syms :idx idx})
                     (swap! precedence-choices insert-precedence syms idx))
                   heuristic]
                  [:ol.precedence-help
                   [:li "Drag operators up and down to assign them precedences."]
                   [:li "Operators have higher precedence than those positioned below them."]
                   [:li "Place operators in the same box to give them equal precedence."]
                   [:li "You can also drag boxes to move all contained operators."]
                   [:li "View "
                    [:span
                     {:on-click (handler-fn (dispatch [:exec-command-from-menu :show-operator-reference]))
                      :style {:font-style "italic"
                              :text-decoration "underline"
                              :cursor "pointer"}}
                     "Help \u2192 Operator Reference"]
                    " for a standard order of operations (from the C language)."]]]]]])]]
          [gap :size "5px"]
          [h-box
           :gap "5px"
           :children [[button
                       :label "Accept these choices"
                       :attr {:on-mouse-down (handler-fn (.preventDefault event))}
                       :on-click #(dispatch [:solver/accept-heuristic
                                             (update heuristic
                                                     :params
                                                     merge
                                                     {:associativity @associativity-choices
                                                      :precedence @precedence-choices})])]
                      [button
                       :label
                       (if (> (count provenance) 1)
                         "No, these are not algebraic expressions"
                         "No, this is not an algebraic expression")
                       :attr {:on-mouse-down (handler-fn (.preventDefault event))}
                       :on-click #(dispatch [:solver/reject-heuristic heuristic])]]]
          #_[:pre (pprint-str @associativity-choices)]
          #_[:pre (pprint-str @precedence-choices)]
          #_[:pre (pprint-str heuristic)]]]))))

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------

(defn one-heuristic-view [heuristic]
  (with-meta
    (case (:type heuristic)
      (:delimited-list :undelimited-list :enclosed-delimited-list :enclosed-undelimited-list)
      [list-heuristic-view heuristic]
      :expression
      [expression-heuristic-view heuristic]
      ;; default
      [unknown-heuristic heuristic])
    ;; XXX: a hack to force remount so that selection atoms are re-initialized
    {:key (str (gensym))}))

(defn heuristics-view []
  (let [state (subscribe [:solver/ux-state])]
    [scroller
     :class "heuristics-view"
     :size "auto"
     :child (if-let [heuristic (first (:heuristics @state))]
              [one-heuristic-view heuristic]
              [debug-view])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; solutions-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn lhs-sym-view [sym]
  (let [decoration-map (subscribe [:affinity-decoration-map])
        decorations (subscribe [:decorations])]
    (fn [sym]
      (let [css (-> sym
                    (label->decoration @decoration-map @decorations)
                    (decoration->html-css))]
        [:span.lhs-sym {:style (dissoc css :border-width)}
         (name (parser/->nonterminal sym))]))))

(defn rhs-view [syms]
  [h-box
   :class "rhs-view"
   :gap "5px"
   :children (into []
                   (map #(vector static-sym-view % (name %)))
                   syms)])

(defn production-view [production]
  (let [decoration-map (subscribe [:affinity-decoration-map])
        decorations (subscribe [:decorations])]
    (fn [[lhs rhs :as production]]
      (let [css (-> lhs
                    (label->decoration @decoration-map @decorations)
                    (decoration->html-css))]
        [h-box
         :class "production-view"
         :style (select-keys css [:border-color])
         :children [[lhs-sym-view lhs]
                    [rhs-view rhs]]]))))

;;------------------------------------------------------------------------------
;; accepted-heuristics-view
;;------------------------------------------------------------------------------

(defn expression-heuristic-summary-view [heuristic]
  (let [decoration-map (subscribe [:affinity-decoration-map])
        decorations (subscribe [:decorations])]
    (fn [{:keys [provenance] :as heuristic}]
      (let [{:keys [lhs-nt op]} (:params heuristic)
            css (-> lhs-nt
                    (label->decoration @decoration-map @decorations)
                    (decoration->html-css))]
        [h-box
         :align :baseline
         :gap "4px"
         :children
         (-> []
             (into [[static-sym-view lhs-nt]
                    [label :label
                     (if (> (count op) 1)
                       "expressions have binary operators"
                       "expressions have binary operator")]
                    [verbose-static-sym-list (sort op)]])
             (conj [:span "."]))]))))

(defn list-heuristic-summary-view [heuristic]
  (let [decoration-map (subscribe [:affinity-decoration-map])
        decorations (subscribe [:decorations])]
    (fn [{:keys [provenance] :as heuristic}]
      (let [{:keys [elem sep encloser]} (:params heuristic)
            lhs (first (inference/extract-provenance-syms provenance))
            css (-> lhs
                    (label->decoration @decoration-map @decorations)
                    (decoration->html-css))]
        [h-box
         :align :baseline
         :gap "4px"
         :children
         (-> []
             (into [[static-sym-view lhs]
                    [label :label "is a sequence of"]
                    [static-sym-view elem]])
             (into (when (some? sep)
                     [[label :label "separated by"]
                      [static-sym-view sep]]))
             (into (when (some? encloser)
                     [[label :label "and enclosed by"]
                      [static-sym-view (first encloser)]
                      [static-sym-view (second encloser)]]))
             (conj [:span "."]))]))))

(defn heuristic-summary-view [heuristic]
  (case (:type heuristic)
    :expression [expression-heuristic-summary-view heuristic]
    ; default
    [list-heuristic-summary-view heuristic]))

(defn heuristic-productions-view [heuristic]
  (let [productions (->> heuristic
                         (heuristic/heuristic->productions)
                         #_(map heuristic/->permanent-production))]
    [v-box
     :class "inset-more"
     :align :start
     :gap "3px"
     :children
     (into []
           (map (partial vector production-view))
           productions)]))

(defn heuristic-filters-view [heuristic]
  (if (= (:type heuristic) :expression)
    (let [{:keys [precedence associativity]} (:params heuristic)
          ordered-priority-groups (->> precedence
                                       (group-by second)
                                       (sort-by first)
                                       (vals)
                                       (map (partial map first)))
          left-assoc (sort (map first (filter #(= (second %) :left) associativity)))
          right-assoc (sort (map first (filter #(= (second %) :right) associativity)))]
      [v-box
       :gap "3px"
       :class "inset-more"
       :children
       [(when (> (count ordered-priority-groups) 1)
          [h-box
           :align :baseline
           :gap "3px"
           :children (into [[label :label "Precedence hierarchy:"]]
                           (comp
                             (map (fn [syms]
                                    [:div {:style {:padding "2px" :border "1px solid #bbb"}}
                                     [concise-static-sym-list syms]]))
                             (interpose [:span ">"]))
                           ordered-priority-groups)])
        (when (seq left-assoc)
          [h-box
           :align :baseline
           :gap "3px"
           :children [[verbose-static-sym-list left-assoc]
                      [label :label
                       (if (> (count left-assoc) 1)
                         "are left associative."
                         "is left associative.")]]])
        (when (seq right-assoc)
          [h-box
           :align :baseline
           :gap "3px"
           :children [[verbose-static-sym-list right-assoc]
                      [label :label
                       (if (> (count right-assoc) 1)
                         "are right associative."
                         "is right associative.")]]])]])
    [:span {:style {:display "none"}}]))

(defn one-accepted-heuristic-view [heuristic]
  (let [details? (reagent/atom false)]
    (fn [heuristic]
      [v-box
       :gap "3px"
       :align :start
       :children [[h-box
                   :class "inset"
                   :gap "3px"
                   :align :baseline
                   :children
                   [[heuristic-summary-view heuristic]
                    [hyperlink
                     :label (if @details?
                              "hide productions"
                              "show productions")
                     :on-click #(swap! details? not)]]]
                  (when @details?
                    [heuristic-productions-view heuristic])
                  (when @details?
                    [heuristic-filters-view heuristic])]])))

(defn accepted-heuristics-view []
  (let [state (subscribe [:solver/ux-state])
        accepted-heuristics (reaction (:accepted-heuristics @state))]
    (fn []
      [v-box
       :class "accepted-heuristics-view"
       :gap "3px"
       :children (into [[label
                         :class "heading-label"
                         :label "Based on your responses, Parsimony has determined the following:"]]
                       (map (partial vector one-accepted-heuristic-view))
                       @accepted-heuristics)])))

;;------------------------------------------------------------------------------
;; production-candidates-view
;;------------------------------------------------------------------------------

(defn- lhs-selector [sym on-click]
  (let [decoration-map (subscribe [:affinity-decoration-map])
        decorations (subscribe [:decorations])]
    (fn [sym on-click]
      (let [css (-> sym
                    (label->decoration @decoration-map @decorations)
                    (decoration->html-css))]
        [:span.lhs-sym {:style (dissoc css :border-width)
                        :on-click (handler-fn
                                    (on-click))}
         (name (parser/->nonterminal sym))]))))

(defn- rhs-node-selector [syms choice on-click]
  [v-box
   :gap "2px"
   :children (into []
                   (map #(vector choice-view % choice on-click))
                   (sort syms))])

(defn- rhs-selector [path choices on-click]
  [h-box
   :class "rhs-view"
   :gap "5px"
   :children (into []
                   (map #(vector rhs-node-selector %1 %2
                                 (fn [choice] (on-click %3 choice)))
                        path choices (range)))])

(defn- candidate-selector [chosen-candidate lhs-nt solution-idx candidate-idx candidate]
  (let [decoration-map (subscribe [:affinity-decoration-map])
        decorations (subscribe [:decorations])]
    (fn [chosen-candidate lhs-nt solution-idx candidate-idx {:keys [path chosen-syms]}]
      (let [css (-> lhs-nt
                    (label->decoration @decoration-map @decorations)
                    (decoration->html-css))
            chosen? (= chosen-candidate candidate-idx)]
        [h-box
         :class "production-view"
         :style (merge (select-keys css [:border-color])
                       {:position "relative"}
                       (when-not chosen?
                         {:opacity "0.7"}))
         :children [[lhs-selector lhs-nt #(dispatch [:solver/toggle-candidate solution-idx candidate-idx])]
                    [rhs-selector path chosen-syms
                     (fn [choice-idx choice]
                       (when chosen?
                         (dispatch [:solver/set-rhs-choice solution-idx candidate-idx choice-idx choice])))]
                    (when-not chosen?
                      [:div
                       {:style {:position "absolute"
                                :left "0"
                                :right "0"
                                :top "0"
                                :bottom "0"
                                :opacity "0.6"
                                :cursor "pointer"
                                :background "url(\"data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' version='1.1' preserveAspectRatio='none' viewBox='0 0 10 10'> <path d='M10 0 L0 10' fill='none' stroke='black' stroke-width='0.5' /><path d='M0 0 L10 10' fill='none' stroke='black' stroke-width='0.5' /></svg>\")"
                                :background-repeat "no-repeat"
                                :background-position "center center"
                                :background-size "100% 100%, auto"}
                        :on-click (handler-fn
                                    (dispatch [:solver/toggle-candidate solution-idx candidate-idx]))}])]]))))

(defn one-solution-view [solution-idx {:keys [chosen-candidate provenance] :as solution}]
  (let [lhs-nt (first (inference/extract-provenance-syms provenance))]
    [h-box
     :align :center
     :children
     [[label
       :style {:font-weight "bold"
               :font-size "140%"
               :color "#bbb"
               :padding-left "5px"
               :min-width "30px"}
       :label (str (inc solution-idx))]
      [v-box
       :class "inset"
       :gap "3px"
       :align :start
       :children (into [#_[:pre (pprint-str solution)]]
                       (map-indexed (partial vector candidate-selector chosen-candidate lhs-nt solution-idx))
                       (:candidates solution))]]]))

(defn- help-view []
  [v-box
   :class "help"
   :children [[label
               :class "inset"
               :label
               [:ol {:style {:list-style "decimal"
                             :margin "0px 0px 0px 15px"}}
                [:li "Click on a right-hand side symbol to select it. The
                      sequence of highlighted symbols, read from left to
                      right, corresponds to the production currently
                      being previewed."]
                [:li "Disable a candidate by clicking on its left-hand
                      side nonterminal.  The candidate will then be
                      grayed out and covered by a X. Click again to
                      reenable the candidate."]
                [:li "You may enable at most one candidate from each
                      numbered section."]
                [:li "When satisfied with the results, click 'Accept
                      Solution' to commit your selections to your
                      grammar."]]]]])

(defn production-candidates-view []
  (let [state (subscribe [:solver/ux-state])
        accepted-heuristics (reaction (:accepted-heuristics @state))
        solutions (reaction (:solutions @state))
        show-help (reagent/atom false)
        hover-help (reagent/atom false)]
    (fn []
      (if (seq @solutions)
        (let [heading-label
              [label
               :class "heading-label"
               :label
               (if (seq @accepted-heuristics)
                 "However, not all of your labels are satisfied by the above inferences.
                  To handle the remaining labels, Parsimony has found the following
                  candidate productions."
                 "Parsimony has inferred the candidate productions below.")]
              help-button
              [h-box
               :class (str "help-toggle-button " (when @hover-help "hoverme"))
               :align :center
               :gap "5px"
               :attr {:on-click (handler-fn
                                  (swap! show-help not))
                      :on-mouse-over (handler-fn
                                       (reset! hover-help true))
                      :on-mouse-out (handler-fn
                                      (reset! hover-help false))}
               :children [[icon {:md-icon-name "zmdi-help"}]
                          (if @show-help
                            [label :label "Click to hide help"]
                            [label :label "Click to show help"])]]]
          [v-box
           :class "production-candidates-view"
           :gap "3px"
           :children
           (-> [[h-box :gap "5px" :children [heading-label help-button]]
                (when @show-help [help-view])
                [line]]
               (into (comp (map-indexed (partial vector one-solution-view))
                           (interpose [line :size "1px"]))
                     @solutions)
               (conj [line]))])
        [:span {:style {:display "none"}}]))))

;;------------------------------------------------------------------------------
;; error-info-view
;;------------------------------------------------------------------------------

(defmulti error-info-view
  (fn [{:keys [causes]}]
    (first causes)))

(defmethod error-info-view :default
  [error-info]
  [:pre (pprint-str error-info)])

(defmethod error-info-view :undefined-symbol
  [{:keys [symbols]}]
  [h-box
   :gap "3px"
   :align :baseline
   :children
   [[label :label "because no productions for"]
    [verbose-static-sym-list symbols]
    [label :label "are defined."]]])

(defmethod error-info-view :production-cycle
  [{:keys [productions]}]
  [v-box
   :gap "10px"
   :children
   [[label :label "because these productions form a cycle:"]
    [v-box
     :class "inset"
     :align :start
     :gap "5px"
     :children (into []
                     (map (partial vector production-view))
                     productions)]]])

;;------------------------------------------------------------------------------
;; preview-pane
;;------------------------------------------------------------------------------

(defn forest-view [args]
  (let [decoration-map (subscribe [:affinity-decoration-map])
        parse-dashboard (subscribe [:parse-dashboard])
        disabled-overlays (subscribe [:solver/disabled-overlays])
        peek (subscribe [:solver/peek])]
    (fn [[[sample-id ti-label] {:keys [string tokens forest disambiguated-forest]}]]
      (let [positive-labels (into #{}
                                  (map (partial parse-dashboard/sample-label->token-indexed-label tokens))
                                  (parse-dashboard/positive-labels-for @parse-dashboard sample-id))
            negative-labels (into #{}
                                  (map (partial parse-dashboard/sample-label->token-indexed-label tokens))
                                  (parse-dashboard/negative-labels-for @parse-dashboard sample-id))
            root-labels (set (parser/root-nodes forest))]
        [parse-forest/parse-forest-view
         {:forest forest
          :string string
          :tokens tokens
          :decoration-map @decoration-map
          :disabled-overlays @disabled-overlays
          :peek @peek
          :positive-labels positive-labels
          :negative-labels negative-labels
          :locked-labels root-labels
          :emphasis-subforest disambiguated-forest
          :on-click-positive #(dispatch [:solver/add-sample sample-id tokens % false])
          :on-click-negative #(dispatch [:solver/add-sample sample-id tokens % true])
          :on-click-remove #(dispatch [:solver/remove-sample sample-id tokens %])}]))))

(defn no-parse-msg
  "Component shown when the current solution does not produce a parse forest for the given group"
  [{[nt i l] :ti-label :keys [tokens string]}]
  (if-let [[char-from char-to] (lexer/token-range->char-range tokens i l)]
    [alert-box
     :alert-type :danger
     :heading [:span
               [icon {:md-icon-name "zmdi-alert-triangle"}]
               " Does not parse: expected " [static-sym-view nt]]
     :body [:pre (subs string char-from char-to)]]
    [:span]))

(defn one-preview [[[_ ti-label] {:keys [tokens string forest]} :as args]]
  (if (seq forest)
    [forest-view args]
    [no-parse-msg {:tokens tokens
                   :string string
                   :ti-label ti-label}]))

(defn preview-pane []
  (let [state (subscribe [:solver/ux-state])]
    (fn []
      (if-not (some? (:error-info @state))
        [v-box
         :class "preview-pane"
         :size "auto"
         :gap "20px"
         :children
         (into []
               (comp (map one-preview)
                     (interpose [line :size "3px"]))
               (:preview @state))]
        [alert-box
         :style {:flex "auto"
                 :margin "0px"
                 :border-radius "0px"}
         :alert-type :danger
         :heading [:span
                   [icon {:md-icon-name "zmdi-close-circle"}]
                   " The solution you have selected is invalid"]
         :body [error-info-view (:error-info @state)]]))))

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------

(defn solutions-view []
  (let [state (subscribe [:solver/ux-state])]
    (fn []
      (if (or (seq (:accepted-heuristics @state))
              (seq (:solutions @state)))
        [v-box
         :class "solutions-view"
         :size "auto"
         :children
         [[v-split
           :margin "0px"
           :panel-1
           [scroller
            :style {:border "1px solid #bbb"}
            :child
            [v-box
             :size "auto"
             :gap "5px"
             :children
             [(when (seq (:accepted-heuristics @state))
                [accepted-heuristics-view])
              (when (seq (:solutions @state))
                [production-candidates-view])
              [button
               :style {:margin-left "5px"}
               :disabled? (some? (:error-info @state))
               :label "Accept Solution"
               :tooltip "Commit this solution to your parser definition"
               :tooltip-position :right-center
               :attr {:on-mouse-down (handler-fn (.preventDefault event))}
               :on-click #(dispatch [:exec-command-from-ribbon :solver-accept-solution])]]]]
           :panel-2
           [scroller
            :style {:border "1px solid #bbb"}
            :child [preview-pane]]]]]
        ;; this should be unreachable
        [nothing-to-solve-view]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; solver-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn solver-view []
  (let [state (subscribe [:solver/ux-state])
        mode (reaction (:mode @state))]
    (fn []
      [v-box
       :size "auto"
       :class "solver-view"
       :children [#_[ribbon {:model [(ribbon/command-item :run-solver {:image-url "url(assets/persimmon.svg)"})
                                     (ribbon/command-item :solver-accept-solution {:md-icon-name "zmdi-play"})]}]
                  (case @mode
                    :idle [idle-view]
                    :view-heuristics [heuristics-view]
                    :view-solutions [solutions-view]
                    :nothing-to-solve [nothing-to-solve-view]
                    [debug-view])]])))
