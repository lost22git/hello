#!/usr/bin/env -S racket -r

;; call/cc
;; modern: let/cc
(define double #f)
(* 2
   (call/cc (λ (k)
              (set! double k)
              1)))
(* 2
   (let/cc k
     (set! double k)
     1))

(double 10)
(double (double 10)) ;; NOTE: composable? no!
(+ 10 (double 10)) ;; NOE: composable? no!
(define a (double 10)) ;; NOE: composable? no!

;; call-with-composable-continuation
;; modern: reset / shift
(require racket/control)
(define fib #f)
(define (make-fib)
  (reset (let loop ([n (shift k (set! fib k) 0)]
                    [a 1]
                    [b 0])

           (if (zero? n)
               a
               (loop (- n 1) (+ a b) a)))))

(make-fib)
(displayln (fib 11)) ;; composable? yes!
(displayln (fib (fib 11))) ;; composable? yes!
(define a (fib 11)) ;; composable? yes!
(println a)
