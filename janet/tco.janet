#!/usr/bin/env janet

# Check Tail Call Optimization support

(defn fib-recur
  "recur variant"
  [n]
  (case n
    0 1
    1 1
    (+ (fib-recur (- n 1))
       (fib-recur (- n 2)))))

(comment
  (assert (= 144
             (fib-recur 11))))


# this would check if TCO
(defn- visit [n a b]
  (case n
    0 a
    (visit (dec n) (+ a b) a)))
(defn fib-tail-recur
  "tail recur variant"
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


(defn fib-iterate
  "iterate variant"
  [n]
  (var data [1 1])
  (for _ 1 n
    (let [[a b] data]
      (set data [(+ a b) a])))
  (get data 0))

(comment
  (assert (= 1 (fib-iterate 0)))
  (assert (= 1 (fib-iterate 1)))
  (assert (= 144 (fib-iterate 11)))

  (fib-iterate 1111))
