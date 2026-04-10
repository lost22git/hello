#!/usr/bin/env -S chicken-csi -s

;; define a function with named arguments
(define (greet name #!key (times 1))
  (when (> times 0)
    (begin
      (print "Hello, " name ".")
      (greet name #:times (- times 1)))))

(greet "chicken")
(greet "chicken" #:times 3)

;; define a case-lambda with named arguments
(define greet-case-lambda
  (case-lambda
    [(name) (greet-case-lambda name #:times 1)]
    [(name #!key times)
     (when (> times 0)
       (begin
         (print "Hello, " name ". (case-lambda)")
         (greet-case-lambda name
                            #:times
                            (- times 1))))]))

(greet-case-lambda "chicken")
(greet-case-lambda "chicken" #:times 3)

;; compose
(import srfi-1)
(define mapcat (compose concatenate map))
(equal? '(0 0 1 0 1 2) (mapcat iota (iota 4)))

;; constantly
(constantly 1)

;; identity
(identity 1)

;; complement
(complement odd?)

;; flip
(= 2 ((flip /) 1 2))

;; partial
;; cut vs cute
;; cute is eager version of cut
(let* ([a 1]
       [cut-add (cut + <> a)]
       [cute-add (cute + <> a)])
  (set! a 2)
  (assert (= 4 (cut-add 2)))
  (assert (= 3 (cute-add 2))))
