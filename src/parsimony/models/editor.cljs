(ns parsimony.models.editor
  (:require [medley.core :refer [dissoc-in]]
            [parsimony.com.neo-codemirror :as codemirror]
            [parsimony.models.overlay :as overlay]
            [parsimony.models.overlay-state :as overlay-state]
            [parsimony.models.colors :as colors]
            [parsimony.util :refer [overlay-seq]]
            [schema.core :as s :include-macros true]
            [parsimony.console :as console]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def cursor-schema {:line s/Num
                    :ch s/Num
                    :index s/Num})

(def cursor-info-schema {:selection? s/Bool
                         :anchor cursor-schema
                         :head cursor-schema})

(def edit-schema {:string s/Str
                  :cursor-info cursor-info-schema})

(def schema {:id s/Num
             :buffer-id s/Num
             :cursor-info (s/maybe cursor-info-schema)
             :read-only s/Bool
             :auto-parse s/Bool
             :history {:done [edit-schema]
                       :undone [edit-schema]}
             :overlay-state overlay-state/schema
             :codemirror s/Any ;; the underlying codemirror instance
             :tag s/Str})

(def default-value
  {:cursor-info nil
   :read-only false
   :auto-parse false
   :history {:done []
             :undone []}
   :overlay-state overlay-state/default-value
   :codemirror nil})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn overlays-at-index
  "Return sequence of maps of form {:overlay-type :overlay-tag :decoration-index :decoration-mod :char-range}, one map for each overlay that
   intersects with the given index"
  [db editor-id index]
  (when-let [overlay-state (get-in db [:editors editor-id :overlay-state])]
    (overlay-state/overlays-at-index db overlay-state index)))

(defn overlays-at-cursor
  [db editor-id]
  (let [editor (get (:editors db) editor-id)
        {:keys [head]} (:cursor-info editor)]
    (when head
      (vec (overlays-at-index db editor-id (:index head))))))

(defn -string
  "Get the underlying string associated with the given editor.  Returns the buffer string, NOT the source string"
  [db editor-id]
  (let [buffer-id (get-in db [:editors editor-id :buffer-id])
        ;;source-id (get-in db [:buffers buffer-id :source-id])
        ]
    #_(get-in db [:sources source-id :string])
    (get-in db [:buffers buffer-id :string])))

(defn inject-normal-overlays
  ([db editor-id overlay-type overlays]
   (inject-normal-overlays db editor-id overlay-type overlays nil))
  ([db editor-id overlay-type overlays decoration-mod]
   (let [{:keys [overlay-state] :as editor} (get-in db [:editors editor-id])
         [db overlay-state] (overlay-state/inject-normal-overlays db overlay-state overlay-type overlays decoration-mod)]
     (assoc-in db [:editors editor-id :overlay-state] overlay-state))))

(defn disable-overlay [db editor-id overlay-type overlay-tag]
  (update-in db [:editors editor-id :overlay-state]
             overlay-state/disable-overlay overlay-type overlay-tag))

(defn disable-overlays-by-type [db editor-id overlay-type]
  (update-in db [:editors editor-id :overlay-state]
             overlay-state/disable-overlays-by-type overlay-type))

(defn enable-overlay [db editor-id overlay-type overlay-tag]
  (update-in db [:editors editor-id :overlay-state]
             overlay-state/enable-overlay overlay-type overlay-tag))

(defn enable-overlays-by-type [db editor-id overlay-type]
  (update-in db [:editors editor-id :overlay-state]
             overlay-state/enable-overlays-by-type overlay-type))

(defn exclusively-enable-overlays-by-type [db editor-id overlay-type]
  (update-in db [:editors editor-id :overlay-state]
             overlay-state/exclusively-enable-overlays-by-type overlay-type))

(defn peek-overlay [db editor-id overlay-type overlay-tag]
  (update-in db [:editors editor-id :overlay-state]
             overlay-state/peek-overlay overlay-type overlay-tag))

(defn unpeek-overlay [db editor-id overlay-type overlay-tag]
  (update-in db [:editors editor-id :overlay-state]
             overlay-state/unpeek-overlay overlay-type overlay-tag))

(defn remove-overlay [db editor-id overlay-type overlay-tag]
  (update-in db [:editors editor-id :overlay-state]
             overlay-state/remove-overlay overlay-type overlay-tag))

(defn remove-overlays [db editor-id overlay-type]
  (update-in db [:editors editor-id :overlay-state]
             overlay-state/remove-overlays-by-type overlay-type))

(defn inject-error-overlays [db editor-id overlays]
  (let [{:keys [overlay-state] :as editor} (get-in db [:editors editor-id])
        [db overlay-state] (overlay-state/inject-error-overlays db overlay-state overlays)]
    (assoc-in db [:editors editor-id :overlay-state] overlay-state)))

(defn- ->char-range
  "Helper function for creating overlay-compatible char-ranges"
  ([index]
   (->char-range index index nil))
  ([from to]
   (->char-range from to nil))
  ([from to info]
   [(max 0 from) (max 0 to) info]))

(defn polygon-index
  "Return the index of the polygon with the given char-from and char-to, if any"
  [db editor-id overlay-type overlay-tag char-from char-to]
  (when-let [overlay (overlay-state/get-overlay (get-in db [:editors editor-id :overlay-state]) overlay-type overlay-tag)]
    (overlay/polygon-index overlay char-from char-to)))

(defn apply-emphasis [db editor-id overlay-type overlay-tag key index mod]
  (update-in db [:editors editor-id :overlay-state]
             overlay-state/apply-emphasis overlay-type overlay-tag key index mod))

(defn apply-all-emphasis [db editor-id key mod]
  (if (get-in db [:editors editor-id])
    (update-in db [:editors editor-id :overlay-state]
               overlay-state/apply-all-emphasis key mod)
    db))

(defn clear-all-emphasis [db editor-id key]
  (if (get-in db [:editors editor-id])
    (update-in db [:editors editor-id :overlay-state]
               overlay-state/clear-all-emphasis key)
    db))

(defn set-read-only [db editor-id]
  (if (get-in db [:editors editor-id])
    (assoc-in db [:editors editor-id :read-only] true)
    db))

(defn clear-selection [db editor-id]
  (when-let [{cm :codemirror} (get-in db [:editors editor-id])]
    (let [cursor-info (codemirror/get-cursor-info cm)]
      (when (:selection? cursor-info)
        (codemirror/set-cursor cm (assoc cursor-info :anchor (:head cursor-info))))))
  db)

(defn toggle-auto-parse [db editor-id]
  (if (get-in db [:editors editor-id])
    (update-in db [:editors editor-id :auto-parse] not)
    db))

(defn editors-with-auto-parse
  "Return seq of ids for editors whose :auto-parse flag is true"
  [db]
  (seq (for [[editor-id {:keys [buffer-id auto-parse] :as editor}] (:editors db)
             :let [{:keys [source-id] :as buffer} (get-in db [:buffers buffer-id])]
             :let [{:keys [source-type]} (get-in db [:sources source-id])]
             :when (some? source-id)
             :when (and auto-parse
                        (not (#{:token :grammar} source-type)))]
         editor-id)))

(defn backing-buffer
  [db editor-id]
  (let [{:keys [buffer-id]} (get-in db [:editors editor-id])]
    (get-in db [:buffers buffer-id])))

(defn backing-source
  [db editor-id]
  (let [{:keys [source-id]} (backing-buffer db editor-id)]
    (get-in db [:sources source-id])))

(defn backed-editor?
  [db editor-id]
  (some? (backing-source db editor-id)))

(defn token-editor?
  [db editor-id]
  (= :token (:source-type (backing-source db editor-id))))

(defn grammar-editor?
  [db editor-id]
  (= :grammar (:source-type (backing-source db editor-id))))

(defn sample-editor?
  [db editor-id]
  (when-let [source (backing-source db editor-id)]
    (not (#{:token :grammar} (:source-type source)))))

(defn unsaved-editor-ids
  [db]
  (seq (for [[editor-id _] (:editors db)
             :let [buffer (backing-buffer db editor-id)
                   source (backing-source db editor-id)]
             :when (and (some? buffer)
                        (some? source)
                        (not= (:string buffer) (:string source)))]
         editor-id)))

;; -----------------------------------------------------------------------------
;; History
;; -----------------------------------------------------------------------------

(defn history-add [editor edit]
  (letfn [(limit-conj [n {:keys [done] :as history} edit]
            {:done (if (< (count done) n)
                     (conj done edit)
                     (conj (into [] (drop 1) done)
                           edit))
             :undone []})]
    (update editor :history (partial limit-conj 100) edit)))

(defn- resolve-edit-in-progress
  "If an edit is in progress, then add that edit to the history and return the
   modified editor.  Otherwise return editor unmodified"
  [{{:keys [done]} :history
    cm :codemirror
    :as editor}]
  (if-let [edit (peek done)]
    (let [cm-value (codemirror/get-value cm)]
      (if (not= cm-value (:string edit))
        (history-add editor {:string cm-value
                             :cursor-info (codemirror/get-cursor-info cm)})
        editor))
    editor))

(defn undo [editor]
  (let [{{:keys [done undone]} :history
         cm :codemirror
         :as editor}
        (resolve-edit-in-progress editor)]
    (if-let [edit (peek done)]
      (let [new-history {:done (pop done) :undone (conj undone edit)}]
        (when-let [{:keys [string cursor-info]} (peek (:done new-history))]
          (codemirror/set-value-silently cm string)
          (codemirror/set-cursor cm cursor-info)
          (codemirror/propagate! cm false))
        (assoc editor :history new-history))
      (do (console/debug :no-undos-left)
          editor))))

(defn redo [editor]
  (let [{{:keys [done undone]} :history
         cm :codemirror
         :as editor}
        (resolve-edit-in-progress editor)]
  (if-let [{:keys [string cursor-info] :as edit} (peek undone)]
    (let [new-history {:done (conj done edit) :undone (pop undone)}]
      (codemirror/set-value-silently cm string)
      (codemirror/set-cursor cm cursor-info)
      (codemirror/propagate! cm false)
      (assoc editor :history new-history))
    (do (console/debug :no-redos-left)
        editor))))
