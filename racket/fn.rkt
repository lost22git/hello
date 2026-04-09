#!/usr/bin/env -S racket -r

(require srfi/1) ;; for iota

;; curry vs curryr
(= 10/3 ((curry / 10) 3))
(= 3/10 ((curryr / 10) 3))

;; compose
(define mapcat (compose append* map))
(mapcat iota (iota 4))

(require racket/function)

;; constantly (const)
;; const const*
(= 1 ((const 1) 2 3))
(let-values ([(a b) ((const* 1 2) "foo" 11)])
  (and (= a 1) (= b 2)))

;; identity
(= 1 (identity 1))

;; complement (negate)
((negate odd?) 2)

;; define a function with named arguments
(define (greet name #:times [times 1])
  (when (> times 0)
    (printf "HELLO, ~A.~%" name)
    (greet name #:times (- times 1))))
(greet "Racket")
(greet "Racket" #:times 2)

;; defina a case lambda (not support named arguments)
(define greet-case-lambda
  (case-λ
   [(name) (greet-case-lambda name 1)]
   [(name times)
    (when (> times 0)
      (printf "HELLO, ~A.(case-lambda)~%" name)
      (greet-case-lambda name (- times 1)))]))

(greet-case-lambda "Racket")
(greet-case-lambda "Racket" 2)
