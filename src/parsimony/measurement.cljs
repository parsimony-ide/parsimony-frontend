(ns parsimony.measurement
  (:require [re-frame.core :refer [->interceptor]]
            [parsimony.comm :as comm]
            [parsimony.config :as config]
            [parsimony.lexer :as lexer]
            [parsimony.models.editor :as editor]
            [parsimony.models.focus-ring :as focus-ring]
            [parsimony.models.live-parse-view :as live-parse-view]
            [parsimony.models.token-dashboard :as token-dashboard]
            [parsimony.models.parse-dashboard :as parse-dashboard]
            [parsimony.models.solver :as solver]
            [parsimony.query :as q]
            [parsimony.solver-ux :as solver-ux]
            [parsimony.views.workspace :as workspace]
            [parsimony.worker :as worker]
            [parsimony.workers.parser :as workers.parser]
            [taoensso.sente :as sente :refer [cb-success?]]
            [parsimony.console :as console]))

(defonce ^:private EVENT-COUNTER (atom 0))

(def
  ^{:doc "Events in this set are marked with :event/hidden? attribute"
    :private true}
  HIDDEN-EVENT-KWS
  #{:editor-init-codemirror
    :editor-history-add
    :editor-cursor
    :editor-blur
    :editor-focus

    :contextbar-clear-all-emphasis

    :remove-all-overlays
    :toggle-overlay ;; redundant with :enable-overlay and :disable-overlay
    :unpeek-overlay ;; redundant with :peek overlay

    :workers-step-all
    :disj-running-worker
    :reset-running-workers
    :-workers-step-all
    :async-worker-complete-success
    :async-worker-complete-failure
    :garbage-collect-workers

    :set-token-horizon
    :set-token-package-cache
    :set-token-example-strings

    :parse-dashboard-unpeek-all-labels

    :solver/unpeek-overlay

    :comm/set-open-flag
    :comm/register-in-flight
    :comm/complete-in-flight
    :comm/show-details})

(declare xform-event-params)

(def ^:private nop (constantly nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Measurement Channels
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def channels
  [#_(sente/make-channel-socket! "/chsk" {:type :auto :host config/measurement-host-1})
   #_(sente/make-channel-socket! "/chsk" {:type :auto :host config/measurement-host-2})])

(defn- request-1
  [{:keys [send-fn] :as channel} request-id data succ-cb fail-cb]
  (send-fn [request-id data]
           30000
           (fn [edn-reply]
             (if (sente/cb-success? edn-reply)
               (succ-cb edn-reply)
               (fail-cb edn-reply)))))

(defn- request
  [request-id data succ-cb fail-cb]
  (doseq [c channels :when c]
    (request-1 c request-id data succ-cb fail-cb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Middleware/Interceptor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -log-event [db event-kw params]
  (try
    (let [timestamp (.now js/Date)
          event-data
          (merge
            {:event/number (swap! EVENT-COUNTER inc)
             :event/timestamp timestamp
             :event/kw event-kw
             :event/url js/window.location.href
             :param/raw-params (pr-str params)}
            (when (contains? HIDDEN-EVENT-KWS event-kw)
              {:event/hidden? true})
            (xform-event-params db event-kw params))]
      (request :measurement/log-event event-data nop nop))
    (catch js/Error e
      (console/warn ::-log-event {:exception e}))))

(def log-event-interceptor
  (->interceptor
    :id :log-event
    :before (fn [context]
              (let [{:keys [event db]} (:coeffects context)
                    [event-kw & params] event]
                (-log-event db event-kw params))
              context)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- get-project-description [db project-id]
  (if-let [description (get-in db [:projects project-id :description])]
    description
    ""))

(defn- get-source-path [db source-id]
  (if-let [source-path (:source-path (get-in db [:sources source-id]))]
    source-path
    ""))

(defn- get-editor-source-path [db editor-id]
  (let [source-id (first (q/editors->sources db [editor-id]))]
    (if-let [source-path (get-in db [:sources source-id :source-path])]
      source-path
      "")))

(defn- get-card-key [db workspace-id pile-id idx]
  (let [workspace (get-in db [:workspaces workspace-id])]
    (if-let [card (workspace/get-card workspace pile-id idx)]
      (workspace/card-key card)
      "")))

(defn- get-card-source-path [db workspace-id pile-id idx]
  (if-let [editor-id (first (q/card->editors db workspace-id pile-id idx))]
    (get-editor-source-path db editor-id)
    ""))

(defn- get-char-range [db editor-id i l]
  (let [token-editor-id (first (q/token-editors db))
        cfg-editor-id (first (q/cfg-editors db))
        worker (get-in db [:workers (workers.parser/->cache-key token-editor-id
                                                                cfg-editor-id
                                                                editor-id)])
        tokens (:target-tokens worker)]
    (lexer/token-range->char-range tokens i l)))

(defn- get-editor-substring [db editor-id char-from char-to]
  (subs (str (editor/-string db editor-id)) char-from char-to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xform-event-params
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti xform-event-params (fn [db event-kw params]
                      event-kw))

(defmethod xform-event-params :default [db event-kw params]
  nil)

;;------------------------------------------------------------------------------
;; Sources
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; File Picker
;;------------------------------------------------------------------------------

(defmethod xform-event-params :file-picker-click [db event-kw [source-id]]
  {:param/source-id source-id
   :param/source-path (get-source-path db source-id)})

;;------------------------------------------------------------------------------
;; Project
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Project Picker
;;------------------------------------------------------------------------------

(defmethod xform-event-params :open-project [db event-kw [project-id]]
  {:param/project-id project-id
   :param/project-description (get-project-description db project-id)})

;;------------------------------------------------------------------------------
;; Workspace
;;------------------------------------------------------------------------------

(defmethod xform-event-params :card-click [db event-kw [workspace-id pile-id idx]]
  (let [source-path (get-card-source-path db workspace-id pile-id idx)]
    (merge
      {:param/card-key (get-card-key db workspace-id pile-id idx)}
      (when (seq source-path)
        {:param/source-path source-path}))))

(defmethod xform-event-params :card-close [db event-kw [workspace-id pile-id idx]]
  (let [source-path (get-card-source-path db workspace-id pile-id idx)]
    (merge
      {:param/card-key (get-card-key db workspace-id pile-id idx)}
      (when (seq source-path)
        {:param/source-path source-path}))))

(defmethod xform-event-params :card-discard [db event-kw [workspace-id pile-id idx]]
  (let [source-path (get-card-source-path db workspace-id pile-id idx)]
    (merge
      {:param/card-key (get-card-key db workspace-id pile-id idx)}
      (when (seq source-path)
        {:param/source-path source-path}))))

(defmethod xform-event-params :card-move [db event-kw [workspace-id pile-id idx card-info]]
  (let [source-path (get-card-source-path db workspace-id (:pile-id card-info) (:idx card-info))]
    (merge
      {:param/card-key (workspace/card-key (:card card-info))
       :param/pile-id pile-id
       :param/idx idx}
      (when (seq source-path)
        {:param/source-path source-path}))))

(defmethod xform-event-params :tabber-fp-drop [db event-kw [workspace-id pile-id idx source-id]]
  {:param/source-id source-id
   :param/source-path (get-source-path db source-id)
   :param/pile-id pile-id
   :param/idx idx})

(defmethod xform-event-params :activate-live-parse-view [db event-kw [overlay]]
  (let [{:keys [overlay-type overlay-tag string]} overlay]
    {:param/overlay-type overlay-type
     :param/overlay-tag overlay-tag
     :param/string string}))

(defmethod xform-event-params :source-hyperlink-click [db event-kw [source-id]]
  {:param/source-id source-id
   :param/source-path (get-source-path db source-id)})

(defmethod xform-event-params :source-hyperlink-click-and-lex [db event-kw [source-id]]
  {:param/source-id source-id
   :param/source-path (get-source-path db source-id)})

;;------------------------------------------------------------------------------
;; Modal
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Editors
;;------------------------------------------------------------------------------

(defmethod xform-event-params :internal-editor-change [db event-kw [editor-id string]]
  {:param/editor-id editor-id
   :param/source-path (get-editor-source-path db editor-id)
   :param/string string})

(defmethod xform-event-params :external-editor-change [db event-kw [editor-id string]]
  {:param/editor-id editor-id
   :param/source-path (get-editor-source-path db editor-id)
   :param/string string})

(defmethod xform-event-params :editor-undo [db event-kw [editor-id]]
  {:param/editor-id editor-id
   :param/source-path (get-editor-source-path db editor-id)})

(defmethod xform-event-params :editor-redo [db event-kw [editor-id]]
  {:param/editor-id editor-id
   :param/source-path (get-editor-source-path db editor-id)})

(defmethod xform-event-params :editor-save [db event-kw [editor-id]]
  {:param/editor-id editor-id
   :param/source-path (get-editor-source-path db editor-id)})

;;------------------------------------------------------------------------------
;; Sample Editor Contextbar
;;------------------------------------------------------------------------------

(defmethod xform-event-params :contextbar-emphasize-overlay
  [db event-kw [editor-id overlay-type overlay-tag char-from char-to]]
  {:param/editor-id editor-id
   :param/source-path (get-editor-source-path db editor-id)
   :param/overlay-type overlay-type
   :param/overlay-tag overlay-tag
   :param/char-from char-from
   :param/char-to char-to})

;;------------------------------------------------------------------------------
;; Overlays
;;------------------------------------------------------------------------------

(defmethod xform-event-params :peek-overlay [db event-kw [editor-id overlay-type overlay-tag]]
  {:param/editor-id editor-id
   :param/overlay-type overlay-type
   :param/overlay-tag overlay-tag})

(defmethod xform-event-params :disable-overlay [db event-kw [editor-id overlay-type overlay-tag]]
  {:param/editor-id editor-id
   :param/overlay-type overlay-type
   :param/overlay-tag overlay-tag})

(defmethod xform-event-params :enable-overlay [db event-kw [editor-id overlay-type overlay-tag]]
  {:param/editor-id editor-id
   :param/overlay-type overlay-type
   :param/overlay-tag overlay-tag})

(defmethod xform-event-params :disable-overlays-by-type [db event-kw [editor-id overlay-type]]
  {:param/editor-id editor-id
   :param/overlay-type overlay-type})

(defmethod xform-event-params :enable-overlays-by-type [db event-kw [editor-id overlay-type]]
  {:param/editor-id editor-id
   :param/overlay-type overlay-type})

;;------------------------------------------------------------------------------
;; Workers
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Token Dashboard
;;------------------------------------------------------------------------------

(defmethod xform-event-params :add-token-sample [db event-kw [string]]
  {:param/string string})

(defmethod xform-event-params :delete-token-sample [db event-kw [sample-id]]
  (let [{:keys [string]} (token-dashboard/get-sample (:token-dashboard db) sample-id)]
    {:param/sample-id sample-id
     :param/string string}))

(defmethod xform-event-params :delete-token-category [db event-kw [element-id]]
  {:param/sample-id element-id})

(defmethod xform-event-params :rename-token-category [db event-kw [element-id new-name]]
  {:param/sample-id element-id
   :param/string new-name})

(defmethod xform-event-params :token-dashboard/accept [db event-kw [path]]
  {:param/string (pr-str path)})

;;------------------------------------------------------------------------------
;; Parse Dashboard
;;------------------------------------------------------------------------------

(defmethod xform-event-params :parse-dashboard/add-sample
  [db event-kw [nt-str editor-id char-from char-to negative?]]
  {:param/nt-str nt-str
   :param/editor-id editor-id
   :param/source-path (get-editor-source-path db editor-id)
   :param/char-from char-from
   :param/char-to char-to
   :param/string (get-editor-substring db editor-id char-from char-to)})

(defmethod xform-event-params :parse-dashboard-delete-sample
  [db event-kw [sample-id]]
  (let [source-path (:source-path (parse-dashboard/get-sample (:parse-dashboard db) sample-id))]
    (merge
      {:param/sample-id sample-id}
      (when (seq source-path)
        {:param/source-path source-path}))))

(defmethod xform-event-params :parse-dashboard-peek-label
  [db event-kw [sample-id label-id]]
  (let [{:keys [string source-path]} (parse-dashboard/get-sample (:parse-dashboard db) sample-id)
        {:keys [nt char-from char-to] :as label} (parse-dashboard/get-label (:parse-dashboard db) sample-id label-id)]
    (merge
      {:param/sample-id sample-id
       :param/label-id label-id}
      (when (seq source-path)
        {:param/source-path source-path})
      (when (seq string)
        {:param/string (subs string char-from char-to)})
      (when label
        {:param/nt-str (name nt)
         :param/char-from char-from
         :param/char-to char-to}))))

(defmethod xform-event-params :parse-dashboard-delete-label
  [db event-kw [sample-id label-id]]
  (let [{:keys [string source-path]} (parse-dashboard/get-sample (:parse-dashboard db) sample-id)
        {:keys [nt char-from char-to] :as label} (parse-dashboard/get-label (:parse-dashboard db) sample-id label-id)]
    (merge
      {:param/sample-id sample-id
       :param/label-id label-id}
      (when (seq source-path)
        {:param/source-path source-path})
      (when (seq string)
        {:param/string (subs string char-from char-to)})
      (when label
        {:param/nt-str (name nt)
         :param/char-from char-from
         :param/char-to char-to}))))

;;------------------------------------------------------------------------------
;; Live Parse View
;;------------------------------------------------------------------------------

(defmethod xform-event-params :live-parse-view/add-sample
  [db event-kw [[nt-kw i l] negative?]]
  (let [editor-id (focus-ring/last-focused-sample-editor db)
        [char-from char-to :as char-range] (get-char-range db editor-id i l)]
    (merge
      {:param/nt-str (name nt-kw)
       :param/i i
       :param/l l
       :param/negative? (boolean negative?)}
      (when editor-id
        {:param/editor-id editor-id
         :param/source-path (get-editor-source-path db editor-id)})
      (when char-range
        {:param/char-from char-from
         :param/char-to char-to
         :param/string (get-editor-substring db editor-id char-from char-to)}))))

(defmethod xform-event-params :live-parse-view/remove-sample
  [db event-kw [sample-id tokens [nt-kw i l]]]
  (let [editor-id (focus-ring/last-focused-sample-editor db)
        [char-from char-to :as char-range] (lexer/token-range->char-range tokens i l)]
    (merge
      {:param/sample-id sample-id
       :param/nt-str (name nt-kw)
       :param/i i
       :param/l l}
      (when editor-id
        {:param/editor-id editor-id
         :param/source-path (get-editor-source-path db editor-id)})
      (when char-range
        {:param/char-from char-from
         :param/char-to char-to}))))

(defmethod xform-event-params :live-parse-view/accept-disambiguation
  [db event-kw [candidate-id]]
  {:disambiguation/candidate
   (pr-str (live-parse-view/disambiguation-candidate (:live-parse-view db) candidate-id))})

;;------------------------------------------------------------------------------
;; Solver
;;------------------------------------------------------------------------------

(defmethod xform-event-params :solver/set-rhs-choice
  [db event-kw [solution-idx candidate-idx choice-idx choice]]
  {:param/solution-idx solution-idx
   :param/candidate-idx candidate-idx
   :param/choice-idx choice-idx
   :param/choice choice})

(defmethod xform-event-params :solver/toggle-candidate
  [db event-kw [solution-idx candidate-idx]]
  {:param/solution-idx solution-idx
   :param/candidate-idx candidate-idx})

(defmethod xform-event-params :solver/peek-overlay
  [db event-kw [overlay-type overlay-tag]]
  {:param/overlay-type overlay-type
   :param/overlay-tag overlay-tag})

(defmethod xform-event-params :solver/toggle-overlay
  [db event-kw [overlay-type overlay-tag]]
  {:param/overlay-type overlay-type
   :param/overlay-tag overlay-tag})

(defmethod xform-event-params :solver/disable-overlays-by-type
  [db event-kw [overlay-type]]
  {:param/overlay-type overlay-type})

(defmethod xform-event-params :solver/enable-overlays-by-type
  [db event-kw [overlay-type]]
  {:param/overlay-type overlay-type})

(defmethod xform-event-params :solver/accept-heuristic
  [db event-kw [heuristic]]
  {:heuristic/type (:type heuristic)
   :heuristic/params (pr-str (:params heuristic))})

(defmethod xform-event-params :solver/reject-heuristic
  [db event-kw [heuristic]]
  {:heuristic/type (:type heuristic)
   :heuristic/params (pr-str (:params heuristic))})

;;------------------------------------------------------------------------------
;; Comm
;;------------------------------------------------------------------------------

(defmethod xform-event-params :comm/register-in-flight
  [db event-kw [registry-id request-id payload]]
  {:comm/registry-id registry-id
   :comm/request-id request-id
   :comm/payload (pr-str payload)})

(defmethod xform-event-params :comm/complete-in-flight
  [db event-kw [registry-id request-id edn-reply status-kw]]
  (merge
    {:comm/registry-id registry-id
     :comm/request-id request-id}
    (when-not (= request-id :algo/all-packages)
      {:comm/reply (pr-str edn-reply)})))

;;------------------------------------------------------------------------------
;; Log
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Commands
;;------------------------------------------------------------------------------

(declare xform-command-args)

(defmethod xform-event-params :exec-command-from-keyboard [db _ [command-kw args]]
  (merge {:command/kw command-kw}
         (xform-command-args db command-kw args)))

(defmethod xform-event-params :exec-command-from-menu [db _ [command-kw args]]
  (merge {:command/kw command-kw}
         (xform-command-args db command-kw args)))

(defmethod xform-event-params :exec-command-from-ribbon [db _ [command-kw args]]
  (merge {:command/kw command-kw}
         (xform-command-args db command-kw args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xform-command-args
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti xform-command-args (fn [db command-kw args]
                               command-kw))

(defmethod xform-command-args :default [db command-kw args]
  nil)

(defmethod xform-command-args :lex-file [db command-kw editor-id]
  {:param/editor-id editor-id
   :param/source-path (get-editor-source-path db editor-id)})

(defmethod xform-command-args :parse-file [db command-kw editor-id]
  {:param/editor-id editor-id
   :param/source-path (get-editor-source-path db editor-id)})

(defmethod xform-command-args :batch-parse-file [db command-kw editor-id]
  {:param/editor-id editor-id
   :param/source-path (get-editor-source-path db editor-id)})

(defmethod xform-command-args :solver-accept-solution [{:keys [solver] :as db} command-kw _]
  {:disambiguation/candidate (pr-str (solver/disambiguation-candidate solver))
   :solver/candidate-productions (pr-str (solver-ux/candidate-productions (:ux solver)))
   :solver/accepted-heuristics (pr-str (get-in solver [:ux :accepted-heuristics]))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; log-worker-status
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti xform-worker-status
  (fn [db worker]
    (:algo (worker/cache-key worker))))

(defn log-worker-status [db worker]
  (try
    (let [timestamp (.now js/Date)
          error (get-in worker [:result :error])]
      (request :measurement/log-event
               (merge
                 {:event/number (swap! EVENT-COUNTER inc)
                  :event/timestamp timestamp
                  :event/kw :worker-status
                  :event/url js/window.location.href
                  :worker/algo (:algo (worker/cache-key worker))
                  :worker/status (worker/status worker)
                  :worker/result (pr-str (:result worker))}
                 (when-let [cause (first (:causes error))]
                   {:worker/cause cause})
                 (xform-worker-status db worker))
               nop
               nop))
    (catch js/Error e
      (console/warn ::log-worker-status {:exception e}))))

(defmethod xform-worker-status :default
  [db worker]
  nil)

;;------------------------------------------------------------------------------
;; compile-lexer
;;------------------------------------------------------------------------------

(defmethod xform-worker-status :compile-lexer
  [db worker]
  (let [editor-id (:token-editor-id (worker/cache-key worker))]
    {:param/editor-id editor-id
     :param/source-path (get-editor-source-path db editor-id)
     :param/string (str (:token-source-string worker))}))

;;------------------------------------------------------------------------------
;; lexer
;;------------------------------------------------------------------------------

(defmethod xform-worker-status :lexer
  [db worker]
  (let [editor-id (:target-editor-id (worker/cache-key worker))]
    {:param/editor-id editor-id
     :param/source-path (get-editor-source-path db editor-id)
     :param/string (str (:target-source-string worker))}))

;;------------------------------------------------------------------------------
;; compile-parser
;;------------------------------------------------------------------------------

(defmethod xform-worker-status :compile-parser
  [db worker]
  (let [editor-id (:cfg-editor-id (worker/cache-key worker))]
    {:param/editor-id editor-id
     :param/source-path (get-editor-source-path db editor-id)
     :param/string (str (:cfg-source-string worker))}))

;;------------------------------------------------------------------------------
;; parser
;;------------------------------------------------------------------------------

(defmethod xform-worker-status :parser
  [db worker]
  (let [editor-id (:target-editor-id (worker/cache-key worker))]
    {:param/editor-id editor-id
     :param/source-path (get-editor-source-path db editor-id)}))

;;------------------------------------------------------------------------------
;; batch-lexer
;;------------------------------------------------------------------------------

(defmethod xform-worker-status :batch-lexer
  [db worker]
  (let [editor-id (:target-editor-id (worker/cache-key worker))]
    {:param/editor-id editor-id
     :param/source-path (get-editor-source-path db editor-id)
     :param/string (str (:target-source-string worker))}))

;;------------------------------------------------------------------------------
;; batch-parser
;;------------------------------------------------------------------------------

(defmethod xform-worker-status :batch-parser
  [db worker]
  (let [editor-id (:target-editor-id (worker/cache-key worker))]
    {:param/editor-id editor-id
     :param/source-path (get-editor-source-path db editor-id)}))

;;------------------------------------------------------------------------------
;; test-answer
;;------------------------------------------------------------------------------

(defmethod xform-worker-status :test-answer
  [db worker]
  (let [editor-id (:target-editor-id (worker/cache-key worker))]
    {:param/editor-id editor-id
     :param/source-path (get-editor-source-path db editor-id)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Timed Function Call
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ->timed-function
  ([kw f]
   (->timed-function kw f nil))
  ([kw f output-fn]
   (fn [& args]
     (let [timestamp (.now js/Date)
           t (system-time)
           result (apply f args)
           duration (- (system-time) t)
           event-data
           (merge
             {:event/number (swap! EVENT-COUNTER inc)
              :event/timestamp timestamp
              :event/kw kw
              :event/url js/window.location.href
              :event/duration (long duration)}
             (when (fn? output-fn)
               (try
                 (doall {:param/raw-params (pr-str (apply output-fn args))})
                 (catch js/Error e
                   nil))))]
       #_(console/warn ::timed-function event-data)
       (request :measurement/log-event
                event-data
                nop
                nop)
       result))))

