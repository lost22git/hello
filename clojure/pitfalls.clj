#!/usr/bin/env -S clj -M

;; 
(->> (let [filter :abc
           search "foo"]
       (filter (partial = search) ["foo" "bar" "foo"]))
     (= ["foo" "bar" "foo"])
     assert)

(->> (let [filter :abc
           search "foo"]
       (clojure.core/filter (partial = search) ["foo" "bar" "foo"]))
     (= ["foo" "foo"])
     assert)
