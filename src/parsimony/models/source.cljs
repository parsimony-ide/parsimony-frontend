(ns parsimony.models.source
    (:require [schema.core :as s :include-macros true]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def source-types (s/enum :file :scratch :token :grammar))

(def schema {:id s/Num
             :string s/Str
             :source-type source-types
             :source-path (s/maybe s/Str)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defaults
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def default-value
  {:string ""
   :source-type :scratch
   :source-path nil})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn source-path
  "If the given source's :source-type is not :scratch, then return the
   corresponding :source-path.  If the :source-type is :scratch, then it has
   nil :source-path, so return placeholder string instead"
  [{:keys [id source-type source-path] :as source}]
  (if (= :scratch source-type)
    (str "<scratch>" id)
    source-path))

(defn sources-with-type [db source-type]
  (seq (for [[_ source] (:sources db)
             :when (= source-type (:source-type source))]
         source)))

(defn sources-with-path [db source-path]
  (seq (for [[_ source] (:sources db)
             :when (= source-path (:source-path source))]
         source)))

(defn token-sources [db]
  (sources-with-type db :token))

(defn grammar-sources [db]
  (sources-with-type db :grammar))
