#!/usr/bin/env -S racket -r

(define (fib n)
  (cond
    [(= n 0) 1]
    [(= n 1) 1]
    [else (+ (fib (- n 1)) (fib (- n 2)))]))

(define (fib-tail-recur n)
  (define (visit i a b)
    (if (= i 0)
        a
        (visit (- i 1) (+ a b) a)))
  (visit n 1 0))

(define (fib-loop n)
  (let ([data '(1 1)])
    (for ([_ (in-range 1 n)])
      (set! data
            (list (apply + data) (car data))))
    (car data)))

(printf "fib-tail-recur ~A = ~A\n"
        0
        (fib-tail-recur 0))
(printf "fib-tail-recur ~A = ~A\n"
        1
        (fib-tail-recur 1))
(printf "fib-tail-recur ~A = ~A\n"
        11
        (fib-tail-recur 11))
(printf "fib-tail-recur ~A = ~A\n"
        111
        (fib-tail-recur 111))

(printf "fib-loop ~A = ~A\n" 0 (fib-loop 0))
(printf "fib-loop ~A = ~A\n" 1 (fib-loop 1))
(printf "fib-loop ~A = ~A\n" 11 (fib-loop 11))
(printf "fib-loop ~A = ~A\n" 111 (fib-loop 111))

(displayln "fib-tail-recur bench:")
(time (fib-tail-recur 111111))
(displayln "fib-loop bench:")
(time (fib-loop 111111))
