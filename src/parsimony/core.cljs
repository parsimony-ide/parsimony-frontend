(ns parsimony.core
  (:require [alanlcode.dagre]
            [cljsjs.codemirror]
            [cljsjs.moment]
            [parsimony.asm-impl-js]
            [cljs.pprint]
            [cljs.reader]
            #_[codemirror.show-invisibles]
            [devtools.core :as devtools]
            [parsimony.comm :as comm]
            [parsimony.commands :as commands]
            [parsimony.config :as config]
            [parsimony.db]
            [parsimony.debug]
            [parsimony.handlers]
            [parsimony.storage]
            [parsimony.subs]
            [reagent.core :as reagent]
            [re-com.util :refer [get-element-by-id]]
            [re-frame.core :refer [dispatch-sync]]))

(defn start-app [top-component]
  (enable-console-print!)
  (when (= "dev" config/BUILD)
    (devtools/enable-feature! :sanity-hints :dirac)
    (devtools/install!))
  (dispatch-sync [:initialize-db])
  (dispatch-sync [:initialize-app])
  (comm/start-router!)
  (commands/install-keybindings!)
  (reagent/render top-component (get-element-by-id "app")))
