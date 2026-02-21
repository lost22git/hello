#!/usr/bin/env -S clj -M

;; let binding destructuring 
(let [[a b] [1 2]]
  (assert (= (+ a b) 3)))

(let [{:keys [first last]} {:first "foo" :last "bar"}]
  (assert (= first "foo"))
  (assert (= last "bar")))

;; fn param destructuring
(defn add
  [{:keys [x y]}]
  (+ x y))
(assert (= (add {:x 1 :y 2 :z 0}) 3))
(defrecord Point [x y z])
(assert (= (add (->Point 1 2 0)) 3))

;; destructuring nil
(let [{:keys [x y]} nil]
  (assert (nil? x))
  (assert (nil? y)))

(let [{:keys [x y]} {:z 10}]
  (assert (nil? x))
  (assert (nil? y)))
