(ns parsimony.models.solver
  (:require [clojure.set :as set]
            [parsimony.disambiguation :as disambiguation]
            [parsimony.inference :as inference]
            [parsimony.lexer :as lexer]
            [parsimony.models.colors :as colors]
            [parsimony.models.parse-dashboard :as parse-dashboard]
            [parsimony.asm.parser :as asm.parser]
            [parsimony.parser :as parser]
            [parsimony.refactor.parser :as refactor.parser]
            [parsimony.solver-impl :as solver-impl]
            [parsimony.solver-ux :as solver-ux]
            [re-frame.core :refer [dispatch]]
            [schema.core :as s :include-macros true]
            [parsimony.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def schema
  {:impl solver-impl/schema
   :ux solver-ux/schema})

(def default-model {:impl solver-impl/default-model
                    :ux solver-ux/default-model})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn reset [_]
  default-model)

(defn set-mode [state mode]
  (update state :ux solver-ux/set-mode mode))

(defn set-impl
  [state impl-state]
  (assoc state :impl impl-state))

(defn get-impl
  [state]
  (:impl state))

(defn- xform-candidate [{:keys [path] :as candidate}]
  {:path path
   :chosen-syms
   (into []
         (map first)
         path)})

(defn- xform-solution [{:keys [provenance candidates] :as solution}]
  {:chosen-candidate 0
   :provenance provenance
   :candidates (into []
                     (map xform-candidate)
                     candidates)})

(defn populate-ux-state [{:keys [impl ux] :as state}]
  (case (:mode ux)
    :view-heuristics (assoc-in state [:ux :heuristics] (:heuristics impl))
    :view-solutions (-> state
                        (assoc-in [:ux :accepted-heuristics] (:accepted-heuristics impl))
                        (assoc-in [:ux :solutions]
                                  (into []
                                        (map xform-solution)
                                        (:solutions impl))))
    ;; default
    state))

(defn accept-heuristic [state selections]
  (update state :impl solver-impl/accept-heuristic selections))

(defn reject-heuristic [state heuristic]
  (update state :impl solver-impl/reject-heuristic heuristic))

(defn solution-exists? [state]
  (solver-ux/solution-exists? (:ux state)))

(defn try-solution-parser
  "Attempt to compile solution parser. Throws ex-info if parser fails to compile."
  [state]
  (let [{:keys [parser]} (solver-impl/recompile-parser (:impl state) false)
        productions (solver-ux/candidate-productions (:ux state))]
    (-> parser
        (refactor.parser/add-productions productions)
        (parser/compile-parser {:skip-checks false}))))

(defn set-rhs-choice [state solution-idx candidate-idx choice-idx choice]
  (update state :ux solver-ux/set-rhs-choice solution-idx candidate-idx choice-idx choice))

(defn toggle-candidate [state solution-idx candidate-idx]
  (update state :ux solver-ux/toggle-candidate solution-idx candidate-idx))

;;------------------------------------------------------------------------------
;; gen-previews
;;------------------------------------------------------------------------------

(defn- gen-sample-previews [state parser sample-id]
  (let [{:keys [string tokens]} (get-in state [:impl :sample-cache sample-id])
        {:keys [cyk codec]} (inference/run-parser-constrained parser tokens nil)]
    (letfn [(gen-forest [[nt i l :as ti-label]]
              (let [forest (-> (asm.parser/reconstruct codec
                                                       cyk
                                                       (into [] (map :label) tokens)
                                                       parser
                                                       nt i l)
                               (parser/disambiguate parser))]
                {:string string
                 :tokens tokens
                 :forest forest}))]
      (let [result (reduce
                     (fn [state ti-label]
                       (assoc-in state
                                 [:ux :preview [sample-id ti-label]]
                                 (gen-forest ti-label)))
                     state
                     (->> (get-in state [:impl :working-set])
                          (filter #(= (first %) sample-id))
                          (map second)))]
        (asm.parser/cpp-free cyk)
        result))))

(defn gen-previews [state]
  (try
    (let [parser (try-solution-parser state)
          state
          (reduce
            (fn [state sample-id]
              (gen-sample-previews state parser sample-id))
            state
            (keys (get-in state [:impl :sample-cache])))]
      (console/debug ::gen-previews :success)
      (assoc-in state [:ux :error-info] nil))
    (catch js/Error e
      (let [exception-data (ex-data e)]
        (console/error ::gen-previews :compile-failure
                       {:error exception-data})
        (-> state
            (assoc-in [:ux :preview] nil)
            (assoc-in [:ux :error-info] exception-data))))))

;;------------------------------------------------------------------------------
;; overlay-state
;;------------------------------------------------------------------------------

(defn register-overlay
  [db overlay-type overlay-tag decoration-mod]
  (solver-ux/register-overlay db overlay-type overlay-tag decoration-mod))

(defn populate-overlay-state
  [db]
  (letfn [(populate-overlay-type [db type syms]
            (reduce
              (fn [db sym]
                (register-overlay db
                                  type
                                  (name (parser/->nonterminal sym))
                                  (when (= :tokens type)
                                    colors/no-mod)))
              db
              syms))]
    (let [state (:solver db)
          {:keys [lexer orig-parser]} (:impl state)
          lexer-syms (-> lexer
                         (lexer/all-syms)
                         (set)
                         (disj :ws :comment)) ;; ignore whitespace and comments
          parser-syms (-> orig-parser
                          (get-in [:orig :productions])
                          (parser/all-syms)
                          (set)
                          (disj :&)) ;; ignore placeholder
          new-productions (solver-ux/solution-productions (:ux state))
          new-syms (set (parser/all-syms new-productions))]
      (console/debug ::populate-overlay-state {:lexer-syms lexer-syms
                                               :parser-syms parser-syms
                                               :new-syms new-syms})
      (-> db
          (populate-overlay-type :tokens lexer-syms)
          (populate-overlay-type :parse (remove parser/terminal? (into parser-syms new-syms)))))))

(defn disable-overlay [state overlay-type overlay-tag]
  (update state :ux solver-ux/disable-overlay overlay-type overlay-tag))

(defn disable-overlays-by-type [state overlay-type]
  (update state :ux solver-ux/disable-overlays-by-type overlay-type))

(defn enable-overlays-by-type [state overlay-type]
  (update state :ux solver-ux/enable-overlays-by-type overlay-type))

(defn enable-overlay [state overlay-type overlay-tag]
  (update state :ux solver-ux/enable-overlay overlay-type overlay-tag))

(defn toggle-overlay [state overlay-type overlay-tag]
  (update state :ux solver-ux/toggle-overlay overlay-type overlay-tag))

(defn peek-overlay [state overlay-type overlay-tag]
  (update state :ux solver-ux/peek-overlay overlay-type overlay-tag))

(defn unpeek-overlay [state overlay-type overlay-tag]
  (update state :ux solver-ux/unpeek-overlay overlay-type overlay-tag))

;;------------------------------------------------------------------------------
;; disambiguation
;;------------------------------------------------------------------------------

(defn disambiguation-candidate [state]
  (when-let [candidate-id (get-in state [:ux :disambiguations :chosen-candidate])]
    (get-in state [:ux :disambiguations :candidates candidate-id :candidate])))

(defn diagnose-root [state parser parse-dashboard [sample-id ti-label :as root]]
  (let [sample (parse-dashboard/get-sample parse-dashboard sample-id)
        {:keys [forest tokens string]} (get-in state [:ux :preview root])]
    (when (seq forest)
      (console/debug ::diagnose-root {:root root
                                      :forest forest})
      (disambiguation/diagnose-forest forest tokens string parser sample))))

(defn synthesize-disambiguations [state parse-dashboard]
  (try
    (let [parser (try-solution-parser state)
          provenances (keys (get-in state [:ux :preview]))
          groups (inference/compute-provenance-groups provenances)
          roots (for [g groups]
                  (->> g
                       (sort-by (fn [[_ [_ _ l]]] l))
                       (vec)
                       (peek)))
          candidates
          (->> roots
               (map (partial diagnose-root state parser parse-dashboard))
               (keep identity)
               (disambiguation/compose-diagnoses parser)
               (map-indexed #(vector (inc %1) {:candidate-id (inc %1)
                                               :candidate %2}))
               (into {0 {:candidate-id 0
                         :candidate nil}}))]
      (console/debug ::synthesize-disambiguations {:parser parser
                                                   :provenances provenances
                                                   :groups groups
                                                   :roots roots
                                                   :candidates candidates})
      (assoc-in state
                [:ux :disambiguations]
                {:chosen-candidate 0
                 :candidates candidates}))
    (catch js/Error e
      (console/error ::synthesize-disambiguations :compile-failure
                     {:error (ex-data e)})
      state)))

(defn gen-disambiguation-previews [state candidate-id]
  (console/debug ::gen-disambiguation-previews {:candidate-id candidate-id})
  (let [candidate (get-in state [:ux :disambiguations :candidates candidate-id :candidate])
        parser (-> (try-solution-parser state)
                   (disambiguation/apply-candidate candidate)
                   (parser/compile-parser))]
    (reduce
      (fn [state [k {:keys [forest]}]]
        (assoc-in state [:ux :preview k :disambiguated-forest]
                  (parser/disambiguate forest parser)))
      state
      (get-in state [:ux :preview]))))

(defn select-disambiguation [state candidate-id]
  (console/debug ::select-disambiguation {:candidate-id candidate-id})
  (assoc-in state [:ux :disambiguations :chosen-candidate] candidate-id))

(defn clear-disambiguations [state]
  (assoc-in state [:ux :disambiguations] nil))

