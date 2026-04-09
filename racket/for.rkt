#!/usr/bin/env -S racket -r

;; for
(for ([i (in-range 10)]
      #:break (>= i 5)
      #:when (odd? i)
      #:do [(define j (* 2 i))])
  (println (list i j)))

;; for/list
(for/list ([i (in-range 10)]
           #:break (>= i 5)
           #:when (odd? i)
           #:do [(define j (* 2 i))])
  (list i j))

;; for/sum
(for/sum ([i (in-range 10)] #:break (>= i 5)
                            #:when (odd? i)
                            #:do [(define j
                                    (* 2 i))])
         j)

;; for/fold
(for/fold ([sum 0]
           #:result sum)
          ([i (in-range 10)]
           #:break (>= i 5)
           #:when (odd? i)
           #:do [(define j (* 2 i))])
  (values (+ sum j)))

;; in-producer
;; create a custom sequence
(define (fib-gen n)
  (define ab (cons 1 0))
  (define c n)
  (λ ()
    (if (zero? c)
        'stop
        (match-let ([(cons a b) ab])
          (set! ab (cons (+ a b) a))
          (set! c (- c 1))
          a))))
(for ([i (in-producer (fib-gen 11) 'stop)])
  (println i))
