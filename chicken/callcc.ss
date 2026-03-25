#!/usr/bin/env -S chicken-csi -s

(define retry #f)

(define fact
  (lambda (x)
    (if (= x 0)
        (call/cc (lambda (k)
                   (set! retry k)
                   1))
        (* x (fact (- x 1))))))

(display (fact 4))
(newline)
(display (retry 1)) ; 1 * 1 * 2 * 3 * 4
(newline)
(display (retry 2)) ; 2 * 1 * 2 * 3 * 4
(newline)
(display (retry 10)) ; 10 * 1 * 2 * 3 * 4

;; source of procedure `values` in `scheme` module
(define (values . vals)
  (call-with-current-continuation
   (lambda (k) (apply k vals))))
