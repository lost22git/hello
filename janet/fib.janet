#!/usr/bin/env janet

(defn fib
  [n]
  (case n
    0 1
    1 1
    (+ (fib (- n 1))
       (fib (- n 2)))))

(comment
  (assert (= 144
             (fib 11))))


(defn- visit [n a b]
  (case n
    0 a
    (visit (dec n) (+ a b) a)))
(defn fib-tail-recur
  [n]

  (visit n 1 0))

(comment
  (assert (= 1
             (fib-tail-recur 0)))
  (assert (= 1
             (fib-tail-recur 1)))
  (assert (= 144
             (fib-tail-recur 11)))

  (fib-tail-recur 1111))


(defn fib-loop
  [n]
  (var data [1 1])
  (for _ 1 n
    (let [[a b] data]
      (set data [(+ a b) a])))
  (get data 0))

(comment
  (assert (= 1 (fib-loop 0)))
  (assert (= 1 (fib-loop 1)))
  (assert (= 144 (fib-loop 11)))

  (fib-loop 1111))
