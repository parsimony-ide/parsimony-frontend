(ns parsimony.commands
  "Registry for user-accessible commands"
  (:require [clojure.set :as set]
            [keybind.core :as key]
            [parsimony.config :as config]
            [parsimony.models.editor :as editor]
            [parsimony.models.parse-dashboard :as parse-dashboard]
            [parsimony.models.solver :as solver]
            [parsimony.models.source :as source]
            [re-frame.core :refer [dispatch]]
            [schema.core :as s :include-macros true]))

(def last-command-schema
  {:count s/Num
   :kw (s/maybe s/Keyword)
   :origin (s/maybe (s/enum :menu :keyboard :ribbon))})

(def last-command-default-value
  {:count 0
   :kw nil
   :origin nil})

(def ^:private reverse-key-lookup
  (set/map-invert key/KEYS))

(defn parse-keybind-str [s]
  (letfn [(add-key [{:keys [code] :as segment}]
            (assoc segment :key (get reverse-key-lookup code)))]
    (into []
      (map add-key)
      (key/parse s))))

(defn- command
  ([kw description]
   (command kw description nil nil))
  ([kw description enable-fn]
   (command kw description enable-fn nil))
  ([kw description enable-fn keybind]
   [kw (merge {:name kw :description description}
              (when (some? enable-fn)
                {:enable-fn enable-fn})
              (when (some? keybind)
                {:keybind keybind}))]))

(def ^:private definitions
  (list
    (command :save-project
             "Save Project"
             (fn [db _]
               (some? (:current-project db)))
             "shift-cmd-alt-s")
    (command :save-as-new-project
             "Save as New Project"
             (fn [db _]
               (some? (:current-project db))))
    (command :close-current-project
             "Close Current Project"
             (fn [db _]
               (some? (:current-project db))))
    (command :export-project
             "Export Project"
             (fn [db _]
               (some? (:current-project db))))
    (command :edit-preferences
             "Edit Preferences"
             nil
             "cmd-alt-,")
    (command :save-file
             "Save File"
             (fn [db editor-id]
               (and (some? editor-id)
                    (editor/backed-editor? db editor-id))))
    (command :save-current-file
             "Save Current File"
             (fn [db _]
               (when-let [editor-id (:current-editor db)]
                 (editor/backed-editor? db editor-id)))
             "cmd-alt-s")
    (command :format-file
             "Format File"
             (fn [db editor-id]
               (and (empty? (:running-workers db))
                    (some? editor-id)
                    (= :grammar (:source-type (editor/backing-source db editor-id))))))
    (command :format-current-file
             "Format Current File"
             (fn [db _]
               (when (empty? (:running-workers db))
                 (when-let [editor-id (:current-editor db)]
                   (= :grammar (:source-type (editor/backing-source db editor-id))))))
             "cmd-alt-o")
    (command :compile-lexer
             "Compile Lexer"
             (fn [db _]
               (and (empty? (:running-workers db))
                    (seq (source/token-sources db))))
             "cmd-alt-8")
    (command :compile-parser
             "Compile Parser"
             (fn [db _]
               (and (empty? (:running-workers db))
                    (seq (source/grammar-sources db))))
             "cmd-alt-9")
    (command :lex-file
             "Lex File"
             (fn [db editor-id]
               (and (empty? (:running-workers db))
                    (some? editor-id)
                    (editor/backed-editor? db editor-id)
                    (not (editor/grammar-editor? db editor-id))
                    (not (editor/token-editor? db editor-id)))))
    (command :lex-current-file
             "Lex Current File"
             (fn [db _]
               (when-let [editor-id (:current-editor db)]
                 (and (empty? (:running-workers db))
                      (editor/backed-editor? db editor-id)
                      (not (editor/grammar-editor? db editor-id))
                      (not (editor/token-editor? db editor-id)))))
             "shift-cmd-alt-8")
    (command :batch-lex-file
             "Batch Lex File"
             (fn [db editor-id]
               (and (empty? (:running-workers db))
                    (some? editor-id)
                    (editor/backed-editor? db editor-id)
                    (not (editor/grammar-editor? db editor-id))
                    (not (editor/token-editor? db editor-id)))))
    (command :batch-lex-current-file
             "Batch Lex Current File"
             (fn [db _]
               (when (empty? (:running-workers db))
                 (when-let [editor-id (:current-editor db)]
                   (and (editor/backed-editor? db editor-id)
                        (not (editor/grammar-editor? db editor-id))
                        (not (editor/token-editor? db editor-id)))))))
    (command :parse-file
             "Parse File"
             (fn [db editor-id]
               (and (empty? (:running-workers db))
                    (some? editor-id)
                    (editor/backed-editor? db editor-id)
                    (not (editor/grammar-editor? db editor-id))
                    (not (editor/token-editor? db editor-id)))))
    (command :parse-current-file
             "Parse Current File"
             (fn [db _]
               (when (empty? (:running-workers db))
                 (when-let [editor-id (:current-editor db)]
                   (and (editor/backed-editor? db editor-id)
                        (not (editor/grammar-editor? db editor-id))
                        (not (editor/token-editor? db editor-id))))))
             "shift-cmd-alt-9")
    (command :batch-parse-file
             "Batch Parse File"
             (fn [db editor-id]
               (and (empty? (:running-workers db))
                    (some? editor-id)
                    (editor/backed-editor? db editor-id)
                    (not (editor/grammar-editor? db editor-id))
                    (not (editor/token-editor? db editor-id)))))
    (command :batch-parse-current-file
             "Batch Parse Current File"
             (fn [db _]
               (when (empty? (:running-workers db))
                 (when-let [editor-id (:current-editor db)]
                   (and (editor/backed-editor? db editor-id)
                        (not (editor/grammar-editor? db editor-id))
                        (not (editor/token-editor? db editor-id)))))))
    (command :run-solver "Run Solver"
             (fn [db _]
               (and (empty? (:running-workers db))
                    (seq (parse-dashboard/all-samples (:parse-dashboard db))))))
    (command :solver-accept-solution "Accept Solution"
             (fn [db _]
               (solver/solution-exists? (:solver db))))
    (command :test-answer "Test Answer"
             (fn [db editor-id]
               (and (empty? (:running-workers db))
                    (some? editor-id)
                    (editor/backed-editor? db editor-id)
                    (not (editor/grammar-editor? db editor-id))
                    (not (editor/token-editor? db editor-id)))))
    (command :about "About")
    (command :show-parsing-lesson "Parsing Primer")
    (command :show-lexer-reference "Lexer Reference")
    (command :show-parser-reference "Parser Reference")
    (command :show-operator-reference "Operator Reference")
    (command :show-important-tips "Important Tips")
    (command :install-example-projects "Install Example Projects")
    (command :show-admin-panel "Show Admin Panel")))

(def registry
  (into (sorted-map)
        definitions))

(defn get-command [kw]
  (get registry kw))

(defn enabled? [db kw arg]
  (let [{:keys [enable-fn]} (get-command kw)]
    (or (not (some? enable-fn))
        (enable-fn db arg))))

(defn install-keybindings! []
  (key/bind! "escape" ::escape #(dispatch [:clear-modal]))
  (doseq [[kw {:keys [keybind]}] registry
          :when (some? keybind)]
    (key/bind! keybind kw #(dispatch [:exec-command-from-keyboard kw nil]))))

(def ^:private hidden-command-kws
  (case config/BUILD
    "production"
    #{:batch-lex-file
      :batch-lex-current-file
      :batch-parse-file
      :batch-parse-current-file}

    ;; default
    #{}))

(defn hidden?
  "Return true iff the given command should not be user-visible"
  [command-kw]
  (contains? hidden-command-kws command-kw))
