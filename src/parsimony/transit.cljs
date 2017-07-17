(ns parsimony.transit
  (:require [cognitect.transit :as t]
            [parsimony.models.project-picker :as project-picker]
            [parsimony.models.parse-dashboard :as parse-dashboard]
            [parsimony.models.token-dashboard :as token-dashboard]
            [parsimony.views.file-picker :as file-picker]))

(defn reify-write-handler [tag-str]
  (reify Object
    (tag [this v] tag-str)
    (rep [this v] (into {} v))))

(def uuid-write-handler
  (reify Object
    (tag [this v] "uuid")
    (rep [this v] (str v))))

(defn uuid-read-handler [s]
  (UUID. s nil))

(def write-handlers
  {cljs.core/UUID uuid-write-handler
   file-picker/SourceLink (reify-write-handler "file-picker/SourceLink")
   file-picker/Heading (reify-write-handler "file-picker/Heading")
   project-picker/Project (reify-write-handler "project-picker/Project")
   project-picker/Heading (reify-write-handler "project-picker/Heading")
   token-dashboard/Sample (reify-write-handler "token-dashboard/Sample")
   token-dashboard/Heading (reify-write-handler "token-dashboard/Heading")
   parse-dashboard/Sample (reify-write-handler "parse-dashboard/Sample")
   parse-dashboard/Heading (reify-write-handler "parse-dashboard/Heading")})

(def read-handlers
  {"uuid" uuid-read-handler
   "file-picker/SourceLink" file-picker/map->SourceLink
   "file-picker/Heading" file-picker/map->Heading
   "project-picker/Project" project-picker/map->Project
   "project-picker/Heading" project-picker/map->Heading
   "token-dashboard/Sample" token-dashboard/map->Sample
   "token-dashboard/Heading" token-dashboard/map->Heading
   "parse-dashboard/Sample" parse-dashboard/map->Sample
   "parse-dashboard/Heading" parse-dashboard/map->Heading})

(def w (t/writer :json-verbose {:handlers write-handlers}))
(def r (t/reader :json {:handlers read-handlers}))

(defn cljs->transit [x]
  (t/write w x))

(defn transit->cljs [s]
  (t/read r s))

