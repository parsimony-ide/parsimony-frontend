(defproject parsimony-frontend "0.1.0"
  :description "Parsimony frontend"
  :license {:name "BSD 3-clause"
            :url "https://opensource.org/licenses/BSD-3-Clause"}
  :dependencies [[alandipert/storage-atom "2.0.1"]
                 [alanlcode/dagre "0.7.5-fork-0"]
                 [binaryage/devtools "0.5.2"]
                 [cljsjs/codemirror "5.7.0-3"]
                 [cljsjs/moment "2.10.6-4"]
                 [clojurewerkz/balagan "1.0.5" :exclusions [org.clojure/clojure]]
                 [com.cognitect/transit-cljs "0.8.239"]
                 [com.lucasbradstreet/instaparse-cljs "1.4.1.0"]
                 [com.taoensso/sente "1.8.1"]
                 [com.rpl/specter "0.9.0"]
                 [garden "1.3.1"]
                 [keybind "2.0.0"]
                 [medley "0.7.0"]
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.229"]
                 [org.clojure/core.async "0.2.374"]
                 [org.clojure/core.logic "0.8.10"]
                 [prismatic/dommy "1.1.0"]
                 [prismatic/schema "1.0.3"]
                 [re-frame "0.8.0"]
                 [re-com "0.9.0"]
                 #_[day8/re-frame-tracer "0.1.0-SNAPSHOT" :exclusions [org.clojars.stumitchell/clairvoyant]]
                 #_[org.clojars.stumitchell/clairvoyant "0.1.0-SNAPSHOT"]
                 [reagent "0.6.0"]
                 [shodan "0.4.2"]]
  :source-paths ["src" "target/classes"]
  :clean-targets ^{:protect false} [:target-path "resources/public/js/compiled" "resources/public/css/compiled"]
  :target-path "target/%s/"
  :cljsbuild {:builds [{:id "production"
                        :source-paths ["src"]
                        :compiler {:optimizations :simple
                                   :main "parsimony.app"
                                   :asset-path "js/compiled/production/out"
                                   :output-to "resources/public/js/compiled/production.js"
                                   :output-dir "resources/public/js/compiled/production/out"
                                   :foreign-libs [{:file "resources/public/js/compiled/asm_impl.js"
                                                   :provides ["parsimony.asm-impl-js"]}]}}
                       {:id "dev"
                        :source-paths ["src" "test"]
                        :figwheel true
                        :compiler {:optimizations :none
                                   :main "parsimony.app"
                                   :asset-path "js/compiled/dev/out"
                                   :output-to "resources/public/js/compiled/dev.js"
                                   :output-dir "resources/public/js/compiled/dev/out"
                                   :foreign-libs [{:file "resources/public/js/compiled/asm_impl.js"
                                                   :provides ["parsimony.asm-impl-js"]}]
                                   :source-map-timestamp true}}
                       {:id "test"
                        :source-paths ["src" "test"]
                        :compiler {:main "parsimony.test-runner"
                                   :output-to "resources/public/js/compiled/test.js"
                                   :output-dir "resources/public/js/compiled/test/out"
                                   :foreign-libs [{:file "resources/public/js/compiled/asm_impl.js"
                                                   :provides ["parsimony.asm-impl-js"]}]}}]}
  :profiles {:dev {:plugins [[lein-figwheel "0.5.8"]
                             [lein-cljsbuild "1.1.4"]
                             [lein-garden "0.3.0"]
                             [lein-doo "0.1.7"]
                             [lein-npm "0.6.2"]]
                   :dependencies [[com.cemerick/piggieback "0.2.1"]
                                  [org.clojure/tools.nrepl "0.2.10"]
                                  [figwheel-sidecar "0.5.8"]
                                  [doo "0.1.7"]]
                   :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}}
  :figwheel {:css-dirs ["resources/public/css/compiled"]
             :nrepl-port 7888
             :server-port 3452}
  :garden {:builds [{:id "screen"
                     :source-paths ["src"]
                     :stylesheet parsimony.styles/screen
                     :compiler {:output-to "resources/public/css/compiled/screen.css"
                                :pretty-print? true}}
                    {:id "reset"
                     :source-paths ["src"]
                     :stylesheet parsimony.styles/reset
                     :compiler {:output-to "resources/public/css/compiled/reset.css"
                                :pretty-print? true}}]}
  :npm {:dependencies [[karma "1.3.0"]
                       [karma-cljs-test "0.1.0"]
                       [karma-chrome-launcher "2.0.0"]]}
  :doo {:build "test"
        :paths {:karma "./node_modules/karma/bin/karma"}})

(defn- add-version-info-to-build
  [build project]
  (if-let [build-index
           (ffirst (filter #(= build (:id (second %)))
                           (map-indexed vector (get-in project [:cljsbuild :builds]))))]
    (update-in project
               [:cljsbuild :builds build-index]
               (fn [build]
                 (-> build
                     (assoc-in [:compiler :closure-defines 'parsimony.config/VERSION]
                               (:version project))
                     (assoc-in [:compiler :closure-defines 'parsimony.config/BUILD]
                               (:id build)))))
    project))

(def project 
  (->> project
       (add-version-info-to-build "dev")
       (add-version-info-to-build "production")))
