#!/usr/bin/env -S clj -M

;;;;;;;;;;;;;;;
;  transient  ;
;;;;;;;;;;;;;;;

; slow
(time
 (loop [current 1000000 state []]
   (if (zero? current)
     state
     (recur (dec current) (conj state current)))))

; fast
(time (loop [current 1000000 state (transient [])]
        (if (zero? current)
          (persistent! state)
          (recur (dec current) (conj! state current)))))
;;;;;;;;;;
;  poly  ;
;;;;;;;;;;
; poly-pef: https://insideclojure.org/2015/04/27/poly-perf/
; multi method
(defmulti fib identity)
(defmethod fib 0 [_] 0)
(defmethod fib 1 [_] 1)
(defmethod fib :default [n] (+ (fib (- n 1))
                               (fib (- n 2))))

(assert (= 55 (fib 10)))
(time  (fib 20))

(defn fib-cond [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib-cond (- n 1)) (fib-cond (- n 2)))))

(assert (= 55 (fib-cond 10)))
(time (fib-cond 20))

