(ns parsimony.solver-ux
  (:require [clojure.set :as set]
            [parsimony.console :as console]
            [parsimony.heuristic :as heuristic]
            [parsimony.inference :as inference]
            [parsimony.models.lexer :refer [token-schema]]
            [parsimony.models.overlay-state :as overlay-state]
            [parsimony.parser :as parser]
            [parsimony.solver-impl :refer [heuristic-schema]]
            [parsimony.util :refer [assoc-if-exists]]
            [parsimony.console :as console]
            [schema.core :as s :include-macros true]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def provenance-schema s/Any)

(def idle-schema {:mode :idle})

(def nothing-to-solve-schema {:mode :nothing-to-solve})

(def view-heuristics-schema
  {:mode :view-heuristics
   :heuristics [heuristic-schema]})

(def view-solutions-schema
  {:mode :view-solutions
   :accepted-heuristics [heuristic-schema]
   :solutions [{:chosen-candidate (s/maybe s/Num)
                :provenance provenance-schema
                :candidates [{:path s/Any
                              :chosen-syms [s/Keyword]}]}]
   :error-info s/Any
   :preview (s/maybe
              {[(s/one s/Num "sample-id") (s/one s/Any "ti-label")]
               {:string s/Str
                :tokens [token-schema]
                :forest s/Any
                (s/optional-key :disambiguated-forest) s/Any}})
   :disambiguations (s/maybe
                      {:chosen-candidate s/Num
                       :candidates {s/Num
                                    {:candidate-id s/Num
                                     :candidate s/Any}}})
   :overlay-state overlay-state/schema})

(def schema
  s/Any)

(def default-model {:mode :idle})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn set-mode [state mode]
  (case mode
    :idle {:mode :idle}
    :nothing-to-solve {:mode :nothing-to-solve}
    :view-heuristics {:mode :view-heuristics
                      :heuristics []}
    :view-solutions {:mode :view-solutions
                     :accepted-heuristics []
                     :solutions []
                     :error-info nil
                     :preview {}
                     :disambiguations nil
                     :overlay-state overlay-state/default-value}))

;;------------------------------------------------------------------------------
;; view-solutions
;;------------------------------------------------------------------------------

(defn set-rhs-choice [state solution-idx candidate-idx choice-idx choice]
  (assoc-if-exists state
                   [:solutions solution-idx :candidates candidate-idx :chosen-syms choice-idx]
                   choice))

(defn toggle-candidate [state solution-idx candidate-idx]
  (let [{:keys [chosen-candidate]} (get-in state [:solutions solution-idx])
        new-v (if (not= chosen-candidate candidate-idx)
                candidate-idx
                nil)]
    #_(console/debug ::toggle-candidate {:solution-idx solution-idx
                                         :candidate-idx candidate-idx
                                         :chosen-candidate chosen-candidate
                                         :new-v new-v})
    (assoc-in state [:solutions solution-idx :chosen-candidate] new-v)))

(defn- heuristic-productions [state]
  (into []
        (comp (mapcat heuristic/heuristic->productions)
              #_(map heuristic/->permanent-production)
              (distinct))
        (:accepted-heuristics state)))

(defn candidate-productions [state]
  (into []
        (for [{:keys [chosen-candidate provenance candidates] :as solution}
              (:solutions state)
              :when (some? chosen-candidate)
              :let [candidate (get candidates chosen-candidate)
                    lhs (first (inference/extract-provenance-syms provenance))]]
          [lhs (vec (:chosen-syms candidate))])))

(defn solution-productions [state]
  (vec (distinct (into (heuristic-productions state)
                       (candidate-productions state)))))

(defn solution-exists? [state]
  (and (= (:mode state) :view-solutions)
       (seq (solution-productions state))))

;;------------------------------------------------------------------------------
;; overlay-state
;;------------------------------------------------------------------------------

(defn register-overlay
  [db overlay-type overlay-tag decoration-mod]
  (let [[db overlay-state]
        (overlay-state/inject-normal-overlays
          db
          (or (get-in db [:solver :ux :overlay-state])
              overlay-state/default-value)
          overlay-type
          [[overlay-tag []]]
          decoration-mod)]
    (assoc-in db [:solver :ux :overlay-state] overlay-state)))

(defn disable-overlay [state overlay-type overlay-tag]
  (update state :overlay-state overlay-state/disable-overlay overlay-type overlay-tag))

(defn disable-overlays-by-type [state overlay-type]
  (update state :overlay-state overlay-state/disable-overlays-by-type overlay-type))

(defn enable-overlay [state overlay-type overlay-tag]
  (update state :overlay-state overlay-state/enable-overlay overlay-type overlay-tag))

(defn enable-overlays-by-type [state overlay-type]
  (update state :overlay-state overlay-state/enable-overlays-by-type overlay-type))

(defn toggle-overlay [state overlay-type overlay-tag]
  (update state :overlay-state overlay-state/toggle-overlay overlay-type overlay-tag))

(defn peek-overlay [state overlay-type overlay-tag]
  (update state :overlay-state overlay-state/peek-overlay overlay-type overlay-tag))

(defn unpeek-overlay [state overlay-type overlay-tag]
  (update state :overlay-state overlay-state/unpeek-overlay overlay-type overlay-tag))
