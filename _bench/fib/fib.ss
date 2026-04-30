#!/usr/bin/env -S chez --script

(define (fib n)
  (if (< n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

(let ([n 40]) (display (fib n)))
