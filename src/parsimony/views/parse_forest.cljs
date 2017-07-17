(ns parsimony.views.parse-forest
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [dommy.core :refer-macros [sel1]]
            [parsimony.dag :as dag]
            [parsimony.models.colors :as colors :refer [decoration->html-css decoration->svg-css]]
            [parsimony.lexer :as lexer]
            [parsimony.parser :as parser :refer [leaf-nodes all-nodes all-edges dummy-node? add-dummy-nodes]]
            [parsimony.util :refer [pprint-str dom-computed-style dom-visible?]]
            [parsimony.views.common :refer [icon]]
            [parsimony.views.neo-overlay :as neo-overlay]
            [reagent.core :as reagent]
            [reagent.ratom :as ratom :refer-macros [reaction]]
            [re-com.core :refer [h-box v-box label md-icon-button popover-tooltip radio-button slider input-text] :refer-macros [handler-fn]]
            [re-com.validate :refer-macros [validate-args-macro]]
            [re-frame.core :refer [dispatch subscribe]]
            [parsimony.console :as console]))

(def ^:private SHOWALL-DEPTH-LIMIT 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn remove-token-nodes
  "Remove nodes that represent token productions"
  [forest]
  (letfn [(aux [rhs]
            (into []
                  (remove #(parser/terminal? (first %)))
                  rhs))]
    (into #{}
          (map (fn [[lhs rhs :as rule]]
                 [lhs (aux rhs)]
                 #_(if (dummy-node? lhs)
                   rule ;; don't remove anything from dummy nodes since we want full context
                   [lhs (aux rhs)])))
          forest)))

(defn forest->graph [forest tokens]
  (add-dummy-nodes forest))

(defn- spanned-string
  [string tokens i l]
  (let [tokens (vec tokens)
        start (:start (get tokens i))
        end (:end (get tokens (dec (+ i l))))]
    (if (and start end)
      (subs string start end)
      (do (console/warn :spanned-string :bad-indices
                        {:start start :end end})
          ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ->label-str
  "Normalize a label keyword to an overlay-tag string"
  [label]
  (name (parser/->nonterminal label)))

(defn label->decoration-index [label decoration-map]
  (get-in decoration-map [:parse (->label-str label)] ;; first, try the nonterminals
          ;; otherwise, this must be a terminal
          (get-in decoration-map [:tokens (->label-str label)])))

(defn label->decoration [label decoration-map decorations]
  (if-let [decoration-index (label->decoration-index label decoration-map)]
    (colors/lookup-decoration decorations decoration-index)
    colors/missing-decoration))

(defn- rhs->overlays [[_ i-offset :as node] rhs tokens decoration-map]
  (let [char-offset (:start (get (vec tokens) i-offset))]
    (apply merge-with
           (fn [x y]
             (merge x y {:char-ranges (vec (concat (:char-ranges x)
                                                   (:char-ranges y)))}))
           (map (fn [[label i l]]
                  (let [label-str (->label-str label)
                        [char-start char-end] (lexer/token-range->char-range tokens i l)]
                    (when (and char-offset char-start char-end)
                      {label-str {:char-ranges [[(- char-start char-offset)
                                                 (- char-end char-offset) nil]]
                                  :active-infos nil
                                  :decoration-index (label->decoration-index label decoration-map)
                                  :decoration-mod (when (parser/terminal? label)
                                                    colors/no-mod)
                                  :type (if (parser/terminal? label) :tokens :parse)
                                  :tag label-str}})))
                rhs))))

(defn- node-text-view [{:keys [string] :as args}]
  (let [char-width (reagent/atom nil)
        line-height (reagent/atom nil)
        update-proportions
        (fn [c]
          ;; XXX: It's inefficient to recompute line-height and char-width for every node instead of once for entire tree,
          ;; but this doesn't seem to be a problem so far. Revisit this if tree-drawing performance is a problem.
          (let [pre-dom (sel1 (reagent/dom-node c) ".string")
                bbox (.getBoundingClientRect pre-dom)
                width (.-width bbox)
                longest-line-length (->> (str/split-lines string)
                                         (sort-by count)
                                         (last)
                                         (count))
                line-height-str (dom-computed-style pre-dom "line-height")]
            (reset! line-height (-> line-height-str
                                    (subs 0 (- (count line-height-str) 2))
                                    (js/parseFloat)))
            (reset! char-width (/ width longest-line-length))))]
    (reagent/create-class
     {:component-did-mount update-proportions
      :component-did-update update-proportions
      :reagent-render
      (fn [{:keys [overlays disabled-overlays peek string decorations] :as args}]
        [h-box
         :class "node-text-view"
         :style {:position "relative"}
         :children [[:pre {:class "string"} string]
                    [:div.overlays
                     {:style {:left 0 :top 0}}
                     (doall
                      (for [[tag overlay] overlays]
                        ^{:key tag}
                        [neo-overlay/static-overlay-view
                         {:overlay overlay
                          :string string
                          :char-width @char-width
                          :line-height @line-height
                          :decorations decorations
                          :disabled-overlays disabled-overlays
                          :peek peek}]))]]])})))

(defn- ambig-rhs? [rhs]
  (seq (filter #(= 4 %) (map count rhs))))

(defn- node-label-buttons [args]
  (let [positive-hovered (reagent/atom nil)
        negative-hovered (reagent/atom nil)]
    (fn [{:keys [node-type positive-labels negative-labels locked-labels hover on-click-positive on-click-negative on-click-remove]
          [label i l :as node] :node
          :as args}]
      (let [flair (cond
                    (contains? (set positive-labels) node)
                    :positive
                    (contains? (set negative-labels) node)
                    :negative
                    ;; default
                    :else nil)
            hovered (= @hover node)
            locked (contains? locked-labels node)
            positive-button
            (when (and (#{:normal :ambiguous} node-type)
                       (some? on-click-positive))
              [icon
               {:md-icon-name "zmdi-plus"
                :class (str "node-label-button positive-label-button "
                            (cond
                              @positive-hovered "hoverme "
                              (and (some? flair) (= flair :positive)) "flairme "
                              :else "idleme "))
                :tooltip (cond
                           locked nil
                           (not= flair :positive) "Create a positive label from this node"
                           (and (= flair :positive)
                                (some? on-click-remove)) "Remove existing label"
                           :else nil)
                :tooltip-position :right-center
                :on-click (when-not locked
                            (fn []
                              (if (= flair :positive)
                                (on-click-remove node)
                                (when (some? on-click-positive)
                                  (on-click-positive node)))))
                :on-mouse-over #(reset! positive-hovered true)
                :on-mouse-out #(reset! positive-hovered nil)}])
            negative-button
            (when (and (#{:normal :ambiguous} node-type)
                       (some? on-click-negative))
              [icon
               {:md-icon-name "zmdi-minus"
                :class (str "node-label-button negative-label-button "
                            (cond
                              @negative-hovered "hoverme "
                              (and (some? flair) (= flair :negative)) "flairme "
                              :else "idleme "))
                :tooltip (cond
                           locked nil
                           (not= flair :negative) "Create a negative label from this node"
                           (and (= flair :negative)
                                (some? on-click-remove)) "Remove existing label"
                           :else nil)
                :tooltip-position :right-center
                :on-click (when-not locked
                            (fn []
                              (if (= flair :negative)
                                (on-click-remove node)
                                (when (some? on-click-negative)
                                  (on-click-negative node)))))
                :on-mouse-over #(reset! negative-hovered true)
                :on-mouse-out #(reset! negative-hovered nil)}])]
        (if (or positive-button
                negative-button)
          [h-box
           :class (str "node-label-buttons "
                       (cond
                         locked "lockme "
                         hovered "hoverme "
                         (some? flair) "flairme "
                         :else "idleme "))
           :gap "5px"
           :children [positive-button
                      negative-button]]
          [:span {:style {:display "none"}}])))))

(defn- examine-button [{:keys [node hover on-click-examine]}]
  [:div.examine-button
   {:style {:display
            (if (= @hover node) "inline" "none")
            :position "absolute"
            :right "-10px"
            :top "-4px"}}
   [icon {:md-icon-name "zmdi-eye"
          :on-click #(on-click-examine node)
          :tooltip "Restrict view to this subtree only"}]])

(defn- node-label-view [{:keys [node-type positive-labels negative-labels locked-labels hover decoration on-click-positive on-click-negative on-click-remove]
                         [label _ _ :as node] :node
                         :as args}]
  (let [css (decoration->html-css decoration)]
    [v-box
     :class "node-label-view"
     :style (assoc (select-keys css [:background])
                   :border-right (str "1px solid " (:border-color css)))
     :children [[:pre {:class "overlay-tag"}
                 (name (parser/->nonterminal label))]
                [node-label-buttons {:node node
                                     :node-type node-type
                                     :positive-labels positive-labels
                                     :negative-labels negative-labels
                                     :locked-labels locked-labels
                                     :hover hover
                                     :on-click-positive on-click-positive
                                     :on-click-negative on-click-negative
                                     :on-click-remove on-click-remove}]]]))

(defn node-box [args]
  (let [decorations (subscribe [:decorations])]
    (fn [{:keys [string tokens decoration-map disabled-overlays peek positive-labels negative-labels locked-labels placement active rhs hover on-click-positive on-click-negative on-click-remove on-click-examine]
          [label i l :as node] :node :as args}]
      (let [string (spanned-string string tokens i l)
            decoration (label->decoration label decoration-map @decorations)
            overlays (rhs->overlays node rhs tokens decoration-map)
            ambiguous (ambig-rhs? rhs)
            dummy (dummy-node? node)]
        [h-box
         :class (str "node "
                     (if ambiguous "ambiguous " "")
                     (if (and (some? active)
                              (not (contains? active node)))
                       "inactive"
                       ""))
         :style (select-keys (decoration->html-css decoration) [:border-color])
         :align :stretch
         :gap "2px"
         :attr {:on-mouse-enter (handler-fn
                                  (reset! hover node))
                :on-mouse-leave (handler-fn
                                  (reset! hover nil))}
         :children [[node-label-view {:node node
                                      :node-type (cond ambiguous :ambiguous
                                                       dummy :dummy
                                                       :else :normal)
                                      :positive-labels positive-labels
                                      :negative-labels negative-labels
                                      :locked-labels locked-labels
                                      :hover hover
                                      :decoration decoration
                                      :on-click-positive on-click-positive
                                      :on-click-negative on-click-negative
                                      :on-click-remove on-click-remove}]
                    [node-text-view {:overlays overlays
                                     :disabled-overlays disabled-overlays
                                     :peek peek
                                     ;; XXX: the placement prop forces the node-text-view to update whenever placement
                                     ;; changes, even though node-text-view doesn't use the placement directly
                                     :placement placement
                                     :string string
                                     :decorations @decorations}]
                    (when on-click-examine
                      [examine-button {:node node
                                       :hover hover
                                       :on-click-examine on-click-examine}])]]))))

(defn node-placement-box [{:keys [node placement] :as args}]
  (let [left-placement (- (:x placement)
                          (/ (:width placement) 2))
        top-placement (- (:y placement)
                         (/ (:height placement) 2))
        hidden? (or (= 0 left-placement) ;; 0 value means this node's position hasn't been calculated yet, so hide it
                    (= 0 top-placement))]
    [v-box
     :class "node-placement-box"
     :style {:position "absolute"
             :visibility (if hidden? "hidden" "visible")
             :left left-placement
             :top top-placement}
     :children [[node-box args]]]))

(def node-view-args-desc
  [{:name :string :required true}
   {:name :tokens :required true}
   {:name :node-dimensions :required true}
   {:name :decoration-map :required true}
   {:name :disabled-overlays :required true}
   {:name :peek :required true}
   {:name :positive-labels :required false}
   {:name :negative-labels :required false}
   {:name :locked-labels :required false}
   {:name :active :required true}
   {:name :placement :required true}
   {:name :node :required true}
   {:name :rhs :required true}
   {:name :hover :required true :type "atom" :description "The node currently hovered over"}
   {:name :on-click-positive :required false :type "node -> nil" :validate-fn fn?}
   {:name :on-click-negative :required false :type "node -> nil" :validate-fn fn?}
   {:name :on-click-remove :required false :type "node -> nil" :validate-fn fn?}
   {:name :on-click-examine :required false :type "node -> nil" :validate-fn fn?}])

(defn node-view [{:keys [node-dimensions node] :as args}]
  {:pre [(validate-args-macro node-view-args-desc args "node-view")]}
  (let [refresh-interval-id (atom nil) ;; holds the WindowTimers intervalID
        similar? (fn [new old]
                   ;; Dimensions are the 'same' as long as they're within a pixel difference
                   (and (<= -1 (- (:width new) (:width old)) 1)
                        (<= -1 (- (:height new) (:height old)) 1)))
        update-node-dimensions
        (fn [c]
          (let [d (reagent/dom-node c)
                bbox (.getBoundingClientRect d)
                ;; new-dimensions {:width (.-offsetWidth d)
                ;;                 :height (.-offsetHeight d)}
                new-dimensions {:width (.-width bbox)
                                :height (.-height bbox)}]
            (when (not (similar? new-dimensions (get @node-dimensions node)))
              (swap! node-dimensions assoc node new-dimensions)
              #_(console/debug :update-node-dimensions node new-dimensions))))]
    (reagent/create-class
     {:component-will-unmount
      (fn [c]
        (swap! node-dimensions dissoc node)
        (when-let [interval-id @refresh-interval-id]
          (.clearInterval js/window interval-id)))
      :component-did-mount
      (fn [c]
        (update-node-dimensions c)
        (let [dom-node (reagent/dom-node c)]
          (reset! refresh-interval-id (.setInterval js/window (fn []
                                                                (when (dom-visible? dom-node)
                                                                  (update-node-dimensions c)))
                                                    500))))
      :reagent-render
      (fn [{:keys [string tokens node decoration-map disabled-overlays peek positive-labels negative-labels locked-labels active placement rhs hover on-click-positive on-click-negative on-click-remove on-click-examine]}]
        [node-placement-box {:string string
                             :tokens tokens
                             :node node
                             :rhs rhs
                             :hover hover
                             :decoration-map decoration-map
                             :disabled-overlays disabled-overlays
                             :peek peek
                             :positive-labels positive-labels
                             :negative-labels negative-labels
                             :locked-labels locked-labels
                             :placement placement
                             :active active
                             :on-click-positive on-click-positive
                             :on-click-negative on-click-negative
                             :on-click-remove on-click-remove
                             :on-click-examine on-click-examine}])})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; edge-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- point->str [{:keys [x y]}]
  (str x " " y))

(defn- points->bezier-str [points]
  (let [[a & more] (into [] (map point->str) points)]
    (str "M" a " Q " (str/join " " more))))

(defn edge-view [args]
  (let [decorations (subscribe [:decorations])]
    (fn [{:keys [edge active decoration-map placement]}]
      #_(console/debug :edge-view-placement (pprint-str placement))
      (let [[_ [dest-label]] edge
            decoration (label->decoration dest-label decoration-map @decorations)]
        [:path (merge
                 (select-keys (decoration->svg-css decoration) [:stroke :stroke-opacity])
                 {:class (str "edge "
                              (if (and (some? active)
                                       (not (contains? active edge)))
                                "inactive"
                                ""))
                  :d (points->bezier-str (:points placement))})]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; forest-pane
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn new-graphlib []
  (let [Graph (.-Graph (.-graphlib js/dagre))]
    (doto (Graph.)
      (.setGraph #js {})
      (.setDefaultEdgeLabel (fn [] #js {})))))

(defn- gen-constraint-graph
  "Given a forest, constructs the node ordering constraint graph for consumption by Dagre"
  [forest]
  #_(console/debug :gen-constraint-graph (pprint-str forest))
  (let [transitive
        (fn transitive [xs]
          (let [xs (vec xs)
                len (count xs)]
            (for [i (range 0 len)
                  j (range (min len (inc i)) len)]
              [(nth xs i) (nth xs j)])))
        rule-constraints
        (for [[_ rhs] forest
              constraint (transitive rhs)]
          constraint)
        token-constraints (->> (leaf-nodes forest)
                               (sort-by second)
                               (transitive))
        cg (new-graphlib)]
    (doseq [[from to] rule-constraints]
      #_(console/debug :add-rule-constraint from to)
      (.setEdge cg (str from) (str to)))
    (doseq [[from to] token-constraints]
      #_(console/debug :add-token-constraint from to)
      (.setEdge cg (str from) (str to)))
    cg))

(defn- calc-layout [node-dimensions forest]
  (if (or (not (set/subset? (all-nodes forest) (set (keys node-dimensions)))) ;; not all node-dimensions have been submitted yet
          (contains? (into #{}
                           (mapcat vals)
                           (vals node-dimensions)) ;; at least 1 node still has 0 dimension (due to being hidden)
                     0))
    nil
    (let [g (new-graphlib)]
      (doseq [n (all-nodes forest)]
        #_(console/debug n)
        (.setNode g (str n) (clj->js (get node-dimensions n))))
      (doseq [[from to] (all-edges forest)]
        (.setEdge g (str from) (str to)))
      (let [cg (gen-constraint-graph forest)
            layout-options #js {:nodesep 20
                                :ranksep 20
                                :marginx 25
                                :marginy 25
                                :constraintGraph cg}]
        #_(console/debug :layout-options layout-options)
        (.setGraph g layout-options)
        (.layout js/dagre g))

      {:nodes (into {}
                    (map (fn [n]
                           [n (assoc (js->clj (.node g (str n)) :keywordize-keys true)
                                     :node n)]))
                    (all-nodes forest))
       :edges (into {}
                    (map (fn [[from to :as e]]
                           [e (assoc (js->clj (.edge g #js {:v (str from) :w (str to)}) :keywordize-keys true)
                                     :edge e)]))
                    (all-edges forest))})))

(defn- calc-max-extent [{:keys [nodes] :as layout}]
  (reduce
    (fn [[curr-x-max curr-y-max] {:keys [width height x y]}]
      (let [x-max (+ x (/ width 2))
            y-max (+ y (/ height 2))]
        [(max curr-x-max x-max)
         (max curr-y-max y-max)]))
    [0 0]
    (vals nodes)))

(defn- parents-map
  "Given a forest, return map from parent node to sets of child nodes"
  [forest]
  (let [f (fn [acc [lhs rhs]]
            (update acc lhs (fnil into #{}) rhs))]
    (reduce f {} forest)))

(defn- children-map
  "Given a forest, return a map from child node to sets of parent nodes"
  [forest]
  (let [f (fn [acc [lhs rhs]]
            (reduce
              (fn [acc r]
                (update acc r (fnil conj #{}) lhs))
              acc
              rhs))]
    (reduce f {} forest)))

(defn- descendants-map
  [pmap cmap node]
  (letfn [(attach-parent [{:keys [nodes edges] :as m}]
            (let [parent-nodes (get cmap node)]
              {:nodes (into nodes parent-nodes)
               :edges (into edges
                            (map #(vector % node))
                            parent-nodes)}))]
    (when (some? node)
      (loop [frontier (list node) seen-nodes #{} seen-edges #{}]
        (if-let [n (first frontier)]
          (let [cs (get pmap n)]
            (recur (into (next frontier)
                         (remove seen-nodes)
                         cs)
                   (conj seen-nodes n)
                   (into seen-edges
                         (map #(vector n %))
                         cs)))
          (attach-parent
            {:nodes seen-nodes
             :edges seen-edges}))))))

(def forest-pane-args-desc
  ;; all of the below arguments are pure, not atoms
  [{:name :forest :required true}
   {:name :string :required true}
   {:name :tokens :required true}
   {:name :decoration-map :required true}
   {:name :disabled-overlays :required true}
   {:name :peek :required true}
   {:name :positive-labels :required false}
   {:name :negative-labels :required false}
   {:name :locked-labels :required false}
   {:name :emphasis-subforest :required false}
   {:name :on-click-positive :required false :type "node -> nil" :validate-fn fn?}
   {:name :on-click-negative :required false :type "node -> nil" :validate-fn fn?}
   {:name :on-click-remove :required false :type "node -> nil" :validate-fn fn?}
   {:name :on-click-examine :required false :type "node -> nil" :validate-fn fn?}])

(defn- add-corresponding-dummy-nodes
  "Inject corresponding dummy nodes from forest-with-dummies into
   subforest-without-dummies.  This function ensures dummy node numbers are
   preserved."
  [forest-with-dummies subforest-without-dummies]
  (let [g (group-by first forest-with-dummies)
        f (fn [[lhs rhs :as x]]
            (if (contains? forest-with-dummies x)
              [x]
              (when-let [xs (get g lhs)]
                (let [rs (into #{} (mapcat second) xs)
                      es (into []
                               (comp (mapcat (partial get g))
                                     (filter (fn [[_ rhs']]
                                               (= rhs rhs'))))
                               rs)
                      ls (into #{} (map first) es)
                      res (into es
                                (map #(vector lhs [%]))
                                ls)]
                  #_(console/debug (pprint-str {:rs rs :es es :ls ls :res res}))
                  res))))]
    (into #{} (mapcat f) subforest-without-dummies)))

(defn- emphasis-map [forest-without-tokens emphasis-subforest]
  (when (seq emphasis-subforest)
    (let [subforest-with-dummies (->> emphasis-subforest
                                      (remove-token-nodes)
                                      (add-corresponding-dummy-nodes forest-without-tokens))]
      {:nodes (parser/all-nodes subforest-with-dummies)
       :edges (parser/all-edges subforest-with-dummies)})))

(defn- compute-active
  "Return map with keys :nodes :edges whose values are sets of nodes (resp.
   edges) that are currently actively visible."
  [forest-without-tokens emphasis-subforest hovered-node]
  (let [cmap (children-map forest-without-tokens)
        pmap (parents-map forest-without-tokens)
        dmap (descendants-map pmap cmap @hovered-node)
        emap (emphasis-map forest-without-tokens emphasis-subforest)]
    (cond
      (not (seq emap)) dmap
      (contains? (:nodes emap) @hovered-node)
      (-> emap
          (update :nodes set/intersection (:nodes dmap))
          (update :edges set/intersection (:edges dmap)))
      ;; NOTE: uncomment below to allow non-emphasized nodes to become active via hover
      ;; (some? @hovered-node) dmap
      :else emap)))

(defn forest-pane [args]
  {:pre [(validate-args-macro forest-pane-args-desc args "forest-pane")]}
  (let [node-dimensions (reagent/atom {})
        hovered-node (reagent/atom nil)]
    (fn [{:keys [forest string tokens decoration-map disabled-overlays peek positive-labels negative-labels locked-labels emphasis-subforest on-click-positive on-click-negative on-click-remove on-click-examine]}]
      (let [forest-with-dummies (add-dummy-nodes forest)
            forest-without-tokens (remove-token-nodes forest-with-dummies)
            layout (calc-layout @node-dimensions forest-without-tokens)
            max-extent (calc-max-extent layout)
            width (str (first max-extent) "px")
            height (str (second max-extent) "px")
            active (compute-active forest-without-tokens emphasis-subforest hovered-node)
            active-nodes (:nodes active)
            active-edges (:edges active)]
        (-> [:div {:class "forest-pane"
                   :style {:position "relative"
                           :width width
                           :height height}}
             ;; edges
             (into [:svg {:class "edge-svg"
                          :style {:width "1px" ;; make it tiny so it does not overlap/interfere with the nodes
                                  :height "1px"
                                  :overflow "visible"
                                  :position "absolute"}}]
                   (map (fn [[e placement]]
                          [edge-view {:edge e
                                      :active active-edges
                                      :decoration-map decoration-map
                                      :placement placement}]))
                   (:edges layout))
             ;; nodes
             (let [rule-map (into {} forest-with-dummies)]
               ;; we generate the rule-map from forest-with-dummies instead of forest-without-tokens since we want tokens shown in overlays
               #_(console/debug :rule-map (pprint-str rule-map))
               (into [:div {:class "node-pane"
                            :style {:position "absolute"}}]
                     (map (fn [n]
                            ^{:key (str n)}
                            [node-view {:string string
                                        :tokens tokens
                                        :node-dimensions node-dimensions
                                        :decoration-map decoration-map
                                        :disabled-overlays disabled-overlays
                                        :peek peek
                                        :positive-labels positive-labels
                                        :negative-labels negative-labels
                                        :locked-labels locked-labels
                                        :active active-nodes
                                        :placement (get-in layout [:nodes n])
                                        :node n
                                        :rhs (get rule-map n)
                                        :hover hovered-node
                                        :on-click-positive on-click-positive
                                        :on-click-negative on-click-negative
                                        :on-click-remove on-click-remove
                                        :on-click-examine (when on-click-examine
                                                            (fn [node]
                                                              (reset! hovered-node nil)
                                                              (on-click-examine node)))}]))
                     (all-nodes forest-without-tokens)))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; option-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- update-depth [curr-depth new-depth max-depth]
  (cond
    (> new-depth max-depth) max-depth
    (< new-depth 1) 1
    :else new-depth))

(defn- option-view [option node-depth max-depth num-nodes num-ambiguous]
  (let [depth-str (str (min max-depth @node-depth))
        buttons [[:show-all [radio-button
                             :label (str "Show all " num-nodes " nodes "
                                         (when (> num-nodes 70)
                                           "(might take a while to draw)"))
                             :model @option
                             :value :show-all
                             :on-change #(reset! option :show-all)]]
                 [:top-down [radio-button
                             :label "Crop ambiguities top down"
                             :model @option
                             :value :top-down
                             :on-change #(reset! option :top-down)]]
                 [:bottom-up [radio-button
                              :label "Crop ambiguities bottom up"
                              :model @option
                              :value :bottom-up
                              :on-change #(reset! option :bottom-up)]]
                 [:show-depth
                  [h-box
                   :align :center
                   :gap "10px"
                   :children
                   [[radio-button
                     :label "Hide nodes with depth greater than"
                     :model @option
                     :value :show-depth
                     :on-change #(reset! option :show-depth)]
                    [input-text
                     :model depth-str
                     :on-change (fn [n]
                                  (reset! option :show-depth)
                                  (swap! node-depth update-depth (js/parseInt n) max-depth))
                     :width "45px"
                     :height "30px"]
                    [slider
                     :model depth-str
                     :on-change (fn [n]
                                  (reset! option :show-depth)
                                  (swap! node-depth update-depth n max-depth))
                     :min 1
                     :max max-depth
                     :width "100px"]]]]]
        enabled (-> #{}
                    (into
                      (when (> num-ambiguous 0)
                        [:show-all :top-down :bottom-up :show-depth]))
                    (into
                      (when (> max-depth SHOWALL-DEPTH-LIMIT)
                        [:show-all :show-depth])))]
    #_(console/debug ::option-view {:enabled enabled :node-depth @node-depth})
    (if (seq enabled)
      [h-box
       :align :center
       :class "option-view"
       :gap "20px"
       :children
       (->> (filter #(contains? enabled (first %)) buttons)
            (map second)
            (vec))]
      [:span {:style {:display "none"}}])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse-forest-view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- subforest [forest root]
  (-> forest
      (parser/forest->dag)
      (dag/successor-subgraph [root])
      (parser/dag->forest)))

(defn- crop-top-down [forest ambig-nodes]
  (let [g (parser/forest->dag forest)
        roots (dag/roots g)
        ambig-nodes (set/difference ambig-nodes roots)
        ambig-successors (into #{}
                               (comp (mapcat (partial dag/successors g))
                                     (mapcat (partial dag/successors g)))
                               ambig-nodes)]
    (-> g
        (dag/successor-subgraph roots ambig-successors)
        (parser/dag->forest))))

(defn- crop-bottom-up [forest ambig-nodes]
  (let [g (parser/forest->dag forest)]
    (-> g
        (dag/predecessor-subgraph (dag/leaves g) ambig-nodes)
        (parser/dag->forest))))

(defn- crop-depth
  [forest depth]
  (let [g (parser/forest->dag forest)
        g' (dag/crop-to-depth g depth)
        result (parser/dag->forest g')]
    #_(console/debug ::crop-depth {:forest forest
                                   :g g
                                   :g' g'
                                   :depth depth
                                   :depth-map (dag/node-depth-map g (first (dag/roots g)))
                                   :result result})
    result))

(defn- max-depth
  [forest]
  (-> forest
      (parser/forest->dag)
      (dag/max-depth)))

(defn- elide-line [prefix-chars suffix-chars line]
  (let [n (count line)]
    (if (> n (+ prefix-chars suffix-chars 5))
      (str (subs line 0 prefix-chars)
           " ... "
           (subs line (- n suffix-chars) n))
      line)))

(defn- elide-lines [prefix-lines suffix-lines prefix-chars suffix-chars lines]
  (let [lines (if (> (count lines) (+ prefix-lines suffix-lines 1))
                (let [prefix (take prefix-lines lines)
                      suffix (take-last suffix-lines lines)]
                  (into (conj (vec prefix) "...")
                        suffix))
                lines)]
  (into []
        (map (partial elide-line prefix-chars suffix-chars))
        lines)))

(defn- elide-str [prefix-lines suffix-lines prefix-chars suffix-chars s]
  (->> (str/split-lines s)
       (elide-lines prefix-lines suffix-lines prefix-chars suffix-chars)
       (str/join "\n")))

(defn- crumb [string tokens decoration node on-click]
  (let [hover (reagent/atom nil)]
    (fn [string tokens decoration [label i l :as node] on-click]
      (let [css (decoration->html-css decoration)]
        (if-let [[char-from char-to] (lexer/token-range->char-range tokens i l)]
          [popover-tooltip
           :label "Click to return to this view"
           :position :below-right
           :showing? hover
           :anchor
           [h-box
            :class "crumb"
            :style (select-keys css [:border-color])
            :attr {:on-mouse-enter (handler-fn
                                     (reset! hover true))
                   :on-mouse-leave (handler-fn
                                     (reset! hover nil))
                   :on-click (handler-fn
                               (on-click node))}
            :gap "2px"
            :children [[:span {:class "crumb-label"
                               :style (merge (select-keys css [:background])
                                             {:border-right (str "1px solid " (:border-color css))})}
                        [:span {:class "overlay-tag"} (name label)]]
                       [:span {:class "crumb-text"}
                        [:pre {:class "string"}
                         (elide-str 1 1 20 20 (subs string char-from char-to))]]]]]
          [:span {:style {:display "none"}}])))))

(defn- forest-crumbs [string tokens decoration-map forest-stack on-crumb-click]
  (let [decorations (subscribe [:decorations])
        hover (reagent/atom nil)]
    (fn [string tokens decoration-map forest-stack on-crumb-click]
      [h-box
       :class (str "forest-crumbs " (when @hover "hoverme"))
       :attr {:on-mouse-enter (handler-fn
                                (reset! hover true))
              :on-mouse-leave (handler-fn
                                (reset! hover nil))}
       :gap "5px"
       :align :center
       :children
       (into []
             (comp
               (map (fn [[label i l :as node]]
                      (let [decoration (label->decoration label decoration-map @decorations)]
                        [crumb string tokens decoration node on-crumb-click])))
               (interpose [icon {:md-icon-name "zmdi-caret-right crumb-separator"}]))
             (:roots forest-stack))])))

(defn- forest-stack-view [forest-stack args]
  (let [option (reagent/atom :show-depth)
        node-depth (reagent/atom SHOWALL-DEPTH-LIMIT)
        local-state (reagent/atom nil)
        mount-fn
        (fn mount-fn [forest-stack]
          (if-let [forest (:forest forest-stack)]
            (let [forest' (subforest forest (peek (:roots forest-stack)))
                  ambig-nodes (parser/ambiguous-nodes forest')
                  max-depth (max-depth forest')]
              (reset! local-state {:forest forest'
                                   :ambig-nodes ambig-nodes
                                   :num-nodes (count (parser/all-nodes forest'))
                                   :max-depth max-depth}))
            (reset! local-state nil)))]
    (reagent/create-class
      {:component-will-mount
       (fn [c]
         (let [forest-stack (reagent/props c)]
           #_(console/debug ::forest-stack-view--will-mount {:forest-stack forest-stack})
           (mount-fn forest-stack)))
       :component-will-receive-props
       (fn [_ [_ forest-stack args]]
         #_(console/debug ::forest-stack-view--will-receive-props {:forest-stack forest-stack})
         (mount-fn forest-stack))
       :reagent-render
       (fn [forest-stack {:keys [string tokens decoration-map on-crumb-click] :as args}]
         (if-let [forest (:forest @local-state)]
           (let [{:keys [ambig-nodes num-nodes max-depth]} @local-state
                 cropped-forest
                 (case @option
                   :top-down (crop-top-down forest ambig-nodes)
                   :bottom-up (crop-bottom-up forest ambig-nodes)
                   :show-depth (crop-depth forest (min max-depth @node-depth))
                   :show-all forest)]
             #_(console/debug ::render {:local-state @local-state :cropped-forest cropped-forest})
             [v-box
              :class "forest-stack-view"
              :size "auto"
              :gap "5px"
              :children [[option-view option node-depth max-depth num-nodes (count ambig-nodes)]
                         [forest-crumbs string tokens decoration-map forest-stack on-crumb-click]
                         [forest-pane
                          (-> args
                              (assoc :forest cropped-forest)
                              (dissoc :on-crumb-click))]]])
           [:span {:style {:display "none"}}]))})))

(defn parse-forest-view [args]
  (let [forest-stack (reagent/atom {:forest nil :roots []})]
    (reagent/create-class
      {:component-will-mount
       (fn [c]
         (let [{:keys [forest]} (reagent/props c)]
           #_(console/debug :parse-forest-view :component-will-mount (pprint-str forest))
           (reset! forest-stack {:forest forest
                                 :roots [(first (parser/root-nodes forest))]})))
       :component-will-receive-props
       (fn [_ [_ {:keys [forest] :as args}]]
         (when (not= forest (:forest @forest-stack))
           #_(console/debug :parse-forest-view :component-will-receive-props (pprint-str forest))
           (reset! forest-stack {:forest forest
                                 :roots [(first (parser/root-nodes forest))]})))
       :reagent-render
       (fn [args]
         [h-box
          :class "parse-forest-view"
          ;; :size "auto"
          :children
          [[forest-stack-view
            @forest-stack
            (assoc args
                   :on-click-examine
                   (fn [node]
                     (when-not (contains? (set (:roots @forest-stack)) node)
                       (swap! forest-stack update :roots conj node)))
                   :on-crumb-click
                   (fn [node]
                     (swap! forest-stack update :roots
                            (fn [roots]
                              (as-> roots roots
                                (into [] (take-while (partial not= node)) roots)
                                (conj roots node))))))]]])})))
