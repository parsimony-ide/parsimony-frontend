(ns parsimony.query
  (:require [cljs.core.logic :as m :refer [membero] :refer-macros [fresh == run* all]]
            [cljs.core.logic.pldb :as pldb :include-macros]
            [parsimony.views.cards :as cards]
            [parsimony.views.file-picker :as file-picker]
            [parsimony.views.workspace :as workspace]
            [parsimony.console :as console]))

;; Rationale:
;;
;; Instead of performing explicit queries about buffers, editors, sources,
;; etc., we create a database of their relationships encoded as constraints via
;; core.logic.  We then can then use core.logic queries to cross reference
;; things.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fact Database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pldb/db-rel ebo ^:index eid ^:index bid) ;; (editor-id, buffer-id)
(pldb/db-rel bso ^:index bid ^:index sid) ;; (buffer-id, source-id)
(pldb/db-rel source-typeo ^:index sid ^:index stype) ;; (source-id, source-type)
(pldb/db-rel wactiveo ^:index wid ^:index pid ^:index aid) ;; (workspace-id, pile-id, active-id)
(pldb/db-rel weo ^:index wid ^:index pid ^:index cid ^:index eid) ;; (workspace-id, pile-id, card-idx, editor-id)

;; really, this is redundant, but need it to get around the lack of disequality constraint
;; in cljs version of core.logic
(pldb/db-rel wco ^:index wid ^:index pid ^:index cid) ;; (workspace-id, pile-id, card-idx)

(pldb/db-rel fpo ^:index fid ^:index sid) ;; (element-id, source-id)

(defn editor-facts [editor]
  (list
   [ebo (:id editor) (:buffer-id editor)]))

(defn buffer-facts [buffer]
  (list
   [bso (:id buffer) (:source-id buffer)]))

(defn source-facts [source]
  (list
   [source-typeo (:id source) (:source-type source)]))

(defn file-picker-facts [file-picker-state]
  (for [elem file-picker-state
        :when (file-picker/source-link-element? elem)]
    [fpo (:id elem) (get-in elem [:data :source-id])]))

(defn workspace-facts [[workspace-id workspace]]
  (let [wco-facts
        (apply concat
               (for [[pid pile] (:cards workspace)
                     [cid card] (map-indexed vector (:available pile))]
                 (if (cards/editor-link? card)
                   (list
                    [weo workspace-id pid cid (cards/backing-editor-id card)]
                    [wco workspace-id pid cid])
                   (list
                    [wco workspace-id pid cid]))))
        wactiveo-facts
        (for [[pid pile] (:cards workspace)]
          [wactiveo workspace-id pid (:active pile)])]

    (concat wco-facts
            wactiveo-facts nil)))

(defn gen-pldb [db]
  (let [all-facts
        (concat
         (mapcat editor-facts (vals (:editors db)))
         (mapcat buffer-facts (vals (:buffers db)))
         (mapcat source-facts (vals (:sources db)))
         (file-picker-facts (:file-picker-state db))
         (mapcat workspace-facts (:workspaces db)))]
    (apply pldb/db
           all-facts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Derived Relations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn eso [eid sid]
  (fresh [bid]
    (ebo eid bid)
    (bso bid sid)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predefined Queries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def last-app-db (atom nil))
(def fact-db (atom nil))

(defn refresh-fact-db! [db]
  ;; XXX: identity check is really conservative. In reality, we only want to change the fact-db state when relevant portions
  ;; of the db have changed (i.e., :editors, :buffers, :sources)
  (when-not (identical? @last-app-db db)
    (do
      #_(console/debug "fact-db refresh")
      (reset! last-app-db db)
      (reset! fact-db (gen-pldb db)))))

(defn editors->sources [db eids]
  (refresh-fact-db! db)
  (pldb/with-db @fact-db
    (run* [sid]
      (fresh [eid]
        (membero eid eids)
        (eso eid sid)))))

(defn editors->buffers [db eids]
  (refresh-fact-db! db)
  (pldb/with-db @fact-db
    (run* [bid]
      (fresh [eid]
        (membero eid eids)
        (ebo eid bid)))))

(defn sources->buffers [db sids]
  (refresh-fact-db! db)
  (pldb/with-db @fact-db
    (run* [bid]
      (fresh [sid]
        (membero sid sids)
        (bso bid sid)))))

(defn sources->editors [db sids]
  (refresh-fact-db! db)
  (pldb/with-db @fact-db
    (run* [eid]
      (fresh [sid]
        (membero sid sids)
        (eso eid sid)))))

(defn sources->cards [db wid sids]
  (refresh-fact-db! db)
  (pldb/with-db @fact-db
    (let [massage (fn [[pid cid]]
                    {:pile-id pid
                     :card-idx cid})]
      (map massage
           (run* [pid cid]
             (fresh [eid sid]
               (membero sid sids)
               (weo wid pid cid eid)
               (eso eid sid)
               ))))))

(defn sources->fp-elements [db sids]
  (refresh-fact-db! db)
  (pldb/with-db @fact-db
    (run* [fid]
      (fresh [sid]
        (membero sid sids)
        (fpo fid sid)))))

(defn fp-elements->sources [db fids]
  (refresh-fact-db! db)
  (pldb/with-db @fact-db
    (run* [sid]
      (fresh [fid]
        (membero fid fids)
        (fpo fid sid)))))

(defn card->buffers [db wid pid cid]
  (refresh-fact-db! db)
  (pldb/with-db @fact-db
    (run* [bid]
      (fresh [eid]
        (weo wid pid cid eid)
        (ebo eid bid)))))

(defn card->editors [db wid pid cid]
  (refresh-fact-db! db)
  (pldb/with-db @fact-db
    (run* [eid]
      (weo wid pid cid eid))))

(defn active-editors [db wid]
  (refresh-fact-db! db)
  (let [massage (fn [[pid cid _ eid]]
                  {:editor-id eid
                   :pile-id pid
                   :card-idx cid})]
    (map massage
         (pldb/with-db @fact-db
           (run* [pid cid aid eid]
             (weo wid pid cid eid)
             (m/== aid cid)
             (wactiveo wid pid aid))))))

(defn token-sources
  "Return all sources with type :token"
  [db]
  (refresh-fact-db! db)
  (pldb/with-db @fact-db
    (run* [sid]
      (source-typeo sid :token))))

(defn token-editors
  "Return all editors whose underlying source is of type :token"
  [db]
  (refresh-fact-db! db)
  (pldb/with-db @fact-db
    (run* [eid]
      (fresh [sid bid]
        (eso eid sid)
        (bso bid sid)
        (source-typeo sid :token)))))

(defn cfg-sources
  "Return all sources with type :grammar"
  [db]
  (refresh-fact-db! db)
  (pldb/with-db @fact-db
    (run* [sid]
      (source-typeo sid :grammar))))

(defn cfg-editors
  "Return all editors whose underlying source is of type :grammar"
  [db]
  (refresh-fact-db! db)
  (pldb/with-db @fact-db
    (run* [eid]
      (fresh [sid bid]
        (eso eid sid)
        (bso bid sid)
        (source-typeo sid :grammar)))))
