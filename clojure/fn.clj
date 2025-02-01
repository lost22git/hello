#!/usr/bin/env -S clj -M

;; fn
(defn add
  [a b]
  (+ a b))

;; param destructing
(defn add
  [{:keys [a b]}]
  (+ a b))

;; fnil: default param value
(def add-default (fnil add {:a 0 :b 0}))
(->
 (add-default nil)
 (= 0)
 assert)

;; default param value
(defn add
  ([] (add {}))
  ([{:keys [a b] :or {a 0 b 0}}]
   (+ a b)))

(->
 (add {:a 1 :b 10})
 (= 11)
 assert)

(->
 (add)
 (= 0)
 assert)

(->
 (add {:a 1})
 (= 1)
 assert)

(->
 (add {:b 1})
 (= 1)
 assert)

;; varargs & named param & default param value
(defn add
  [& {:keys [a b] :or {a 0 b 0}}]
  (+ a b))

(->
 (add)
 (= 0)
 assert)

(->
 (add :a 1)
 (= 1)
 assert)

(->
 (add :b 1)
 (= 1)
 assert)

(->
 (add :a 1 :b 1)
 (= 2)
 assert)

(->
 (add {:a 1 :b 1})
 (= 2)
 assert)

;; apply
(->
 (apply add [:a 1 :b 1])
 (= 2)
 assert)

;; partial
;; => (defn [& args] (apply add :a 10 args))
(def add-10 (partial add :a 10))
(->
 (add-10 :b 1)
 (= 11)
 assert)

;; annoymouse fn
;; => (defn [& args] (apply add args))
(def add-anno #(apply add %&))
(->
 (add-anno)
 (= 0)
 assert)

(->
 (add-anno :b 1)
 (= 1)
 assert)

(->
 (add-anno :a 1 :b 1)
 (= 2)
 assert)

;; => (fn [x a b] (add :a a :b b))
(def add-unnamed #(add :a %2 :b %3))

(->
 (add-unnamed "whatever be ingored" 1 2)
 (= 3)
 assert)

;; complement
(->
 (filter even? [1 2 3 4 5])
 (= [2 4])
 assert)

(->
 (filter (complement even?) [1 2 3 4 5])
 (= [1 3 5])
 assert)

;; constantly
(assert (= 1 ((constantly 1) 1)))
(assert (= 1 ((constantly 1) 1 2)))
(assert (= 1 ((constantly 1) 1 2 "sdsf")))

;; comp: compose
(assert (= "HELLO"
           ((comp clojure.string/upper-case clojure.string/trim) " hello ")))
