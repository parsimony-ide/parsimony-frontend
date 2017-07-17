(ns parsimony.transit-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [parsimony.transit :refer [cljs->transit transit->cljs]]
            [parsimony.models.project-picker :as project-picker]
            [parsimony.models.parse-dashboard :as parse-dashboard]
            [parsimony.models.token-dashboard :as token-dashboard]
            [parsimony.views.file-picker :as file-picker]))

(defn round-trip [x]
  (-> x
      (cljs->transit)
      (transit->cljs)))

(defn check-round-trip [x]
  (= x (round-trip x)))

(deftest round-trip-1
  (is (check-round-trip file-picker/default-model))
  (is (check-round-trip (file-picker/source-link 1)))

  (is (check-round-trip project-picker/default-model))
  (is (check-round-trip (project-picker/project 1)))

  (is (check-round-trip token-dashboard/default-model))
  (is (check-round-trip (token-dashboard/sample 1 "test")))

  (is (check-round-trip parse-dashboard/default-model))
  (is (check-round-trip (parse-dashboard/sample 1 2 nil "foo" [{:nt :Foo
                                                                :char-from 0
                                                                :char-to 3
                                                                :type :positive
                                                                :label-id 0}]))))

