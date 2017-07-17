(ns parsimony.example-projects
  (:require [parsimony.example-projects.demo :as demo]
            [parsimony.storage :as storage]
            [parsimony.transit :refer [transit->cljs cljs->transit]]))

(def projects
  (into []
        (map transit->cljs)
        [demo/demo-partial-string
         demo/demo-string]))

(defn install!
  []
  (doseq [project projects]
    (let [id (random-uuid)]
      (storage/persist! id (assoc project :id id)))))

;; (defn emit-current-project
;;   []
;;   (let [db @re-frame.db/app-db
;;         id (:current-project db)
;;         project (get-in db [:projects id])]
;;     (cljs->transit (assoc project :id nil))))
