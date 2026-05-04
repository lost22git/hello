#lang racket/base

(require racket/fixnum)

(define (fib n)
  (if (fx< n 2)
      1
      (fx+ (fib (fx- n 1)) (fib (fx- n 2)))))

(let ([n 40]) (displayln (fib n)))
