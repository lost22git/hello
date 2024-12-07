(ns demo.core-test
  (:use clojure.test)
  (:require [demo.core :as s]))

(deftest test-add
  (testing "add 1 2 => 3"
    (->> (s/add 1 2)
         (= 3)
         is)))
