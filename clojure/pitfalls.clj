#!/usr/bin/env -S clj -M

(comment
  (->> (let [filter :abc
             search "foo"]
         (filter (partial = search) ["foo" "bar" "foo"]))
       (= ["foo" "bar" "foo"])
       assert)

  (->> (let [filter :abc
             search "foo"]
         (clojure.core/filter (partial = search) ["foo" "bar" "foo"]))
       (= ["foo" "foo"])
       assert))

(comment
  (let [c nil]
    (= '(2 1) (conj c 1 2)))

  (let [c []]
    (= [1 2] (conj c 1 2)))

  (let [c nil]
    (= [1 2] (conj (or c []) 1 2))))
