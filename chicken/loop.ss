#!/usr/bin/env -S chicken-csi -s

(import srfi-1)

;; let loop
(let loop ([a (iota 10)])
  (if (null? a)
      '()
      (begin
        (print (car a))
        (loop (cdr a)))))

;; do
(do ((a (iota 10) (cdr a)))
    ((null? a) '())
    (print (car a)))

;; for-each
(for-each print (iota 10))
