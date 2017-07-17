(ns parsimony.commands.handlers
  (:require [parsimony.commands :as commands]
            [parsimony.handlers.dna
             :refer [hf-return hf-pure <-hf middleware]
             :refer-macros [hf->]
             :as dna]
            [parsimony.handlers.xy :as xy]
            [parsimony.views.admin :as admin]
            [parsimony.views.important-tips :as important-tips]
            [parsimony.views.lexer-reference :as lexer-reference]
            [parsimony.views.parser-reference :as parser-reference]
            [parsimony.views.operator-reference :as operator-reference]
            [parsimony.views.modal :as modal]
            [parsimony.views.parsing-lesson :as parsing-lesson]
            [re-frame.core :refer [reg-event-db]]
            [parsimony.console :as console]))

(defmulti exec-command
  (fn [db command-kw args]
    (console/debug ::exec-command {:command-kw command-kw})
    command-kw))

(defmethod exec-command :default [db command-kw _]
  (console/error ::exec-command :failure
                 {:reason "No implementation for command-kw"
                  :command-kw command-kw})
  (hf-return db nil))

(defmethod exec-command :about [db _ _]
  (hf-return (assoc db :modal (modal/about-modal)) nil))

(defmethod exec-command :show-parsing-lesson [db _ _]
  (hf-return (assoc db :modal (parsing-lesson/parsing-lesson-modal)) nil))

(defmethod exec-command :show-lexer-reference [db _ _]
  (hf-return (assoc db :modal (lexer-reference/lexer-reference-modal)) nil))

(defmethod exec-command :show-parser-reference [db _ _]
  (hf-return (assoc db :modal (parser-reference/parser-reference-modal)) nil))

(defmethod exec-command :show-operator-reference [db _ _]
  (hf-return (assoc db :modal (operator-reference/operator-reference-modal)) nil))

(defmethod exec-command :show-important-tips [db _ _]
  (hf-return (assoc db :modal (important-tips/important-tips-modal)) nil))

(defmethod exec-command :install-example-projects [db _ _]
  (xy/install-example-projects db))

(defmethod exec-command :show-admin-panel [db _ _]
  (hf-return (assoc db :modal (admin/admin-modal)) nil))

(defmethod exec-command :save-project [db _ _]
  (xy/save-and-persist-current-project! db))

(defmethod exec-command :save-as-new-project [db _ _]
  (xy/save-and-persist-current-project-as-new! db))

(defmethod exec-command :close-current-project [db _ _]
  (xy/close-current-project-with-confirmation db))

(defmethod exec-command :save-file [db _ editor-id]
  (xy/save-and-persist-editor! db editor-id))

(defmethod exec-command :save-current-file [db _ _]
  (xy/save-and-persist-editor! db (:current-editor db)))

(defmethod exec-command :format-file [db _ editor-id]
  (xy/reformat-cfg db editor-id))

(defmethod exec-command :format-current-file [db _ _]
  (xy/reformat-cfg db (:current-editor db)))

(defmethod exec-command :compile-lexer [db _ _]
  (xy/compile-lexer db))

(defmethod exec-command :compile-parser [db _ _]
  (xy/run-auto-parse db))

(defmethod exec-command :lex-file [db _ editor-id]
  (xy/run-lexer db editor-id))

(defmethod exec-command :lex-current-file [db _ _]
  (xy/run-lexer db (:current-editor db)))

(defmethod exec-command :batch-lex-file [db _ editor-id]
  (xy/run-batch-lexer db editor-id))

(defmethod exec-command :batch-lex-current-file [db _ _]
  (xy/run-batch-lexer db (:current-editor db)))

(defmethod exec-command :parse-file [db _ editor-id]
  (xy/run-parser db editor-id))

(defmethod exec-command :parse-current-file [db _ _]
  (xy/run-parser db (:current-editor db)))

(defmethod exec-command :batch-parse-file [db _ editor-id]
  (xy/run-batch-parser db editor-id))

(defmethod exec-command :batch-parse-current-file [db _ _]
  (xy/run-batch-parser db (:current-editor db)))

(defmethod exec-command :run-solver [db _ _]
  (xy/run-solver db))

(defmethod exec-command :test-answer [db _ editor-id]
  (xy/test-answer db editor-id))

(defmethod exec-command :solver-accept-solution [db _ _]
  (xy/solver-accept-solution db))

(defn set-last-command [db command-kw origin]
  (hf-return (-> db
                 (update-in [:last-command :count] inc)
                 (assoc-in  [:last-command :kw] command-kw)
                 (assoc-in  [:last-command :origin] origin))
             command-kw))

(defn- -exec-command-handler [origin db [command-kw args]]
  (if (commands/enabled? db command-kw args)
    (hf-> db
          (set-last-command command-kw origin)
          (exec-command args)
          (<-hf))
    (do (console/debug ::-exec-command-handler :skip-disabled {:command-kw command-kw})
        db)))

(reg-event-db
  :exec-command-from-keyboard
  middleware
  (partial -exec-command-handler :keyboard))

(reg-event-db
  :exec-command-from-menu
  middleware
  (partial -exec-command-handler :menu))

(reg-event-db
  :exec-command-from-ribbon
  middleware
  (partial -exec-command-handler :ribbon))
