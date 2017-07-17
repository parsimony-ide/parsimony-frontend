(ns parsimony.app
  (:require [parsimony.core]
            [parsimony.user-agent :as user-agent]
            [parsimony.views.app]
            [reagent.core :as reagent]))

;; We need to var-quote the main component due to a limitation in force-update-all. From the documentation for
;; reagent.core/force-update-all:
;;
;; Note that force-update-all may not update root components. This happens if a component 'foo' is mounted with
;; `(render [foo])` (since functions are passed by value, and not by reference, in ClojureScript). To get around this
;; you'll have to introduce a layer of indirection, for example by using `(render [#'foo])` instead."
;;
;; See this GH thread for additional discussion: https://github.com/reagent-project/reagent/issues/94
(defonce mounted
  (if (user-agent/is-supported?)
    (parsimony.core/start-app [#'parsimony.views.app/main])
    (js/alert "Your browser is not currently supported. Please use Google Chrome Version 45 or higher.")))

;; for interactive development, force update on all components after figwheel reload
(reagent/force-update-all)
