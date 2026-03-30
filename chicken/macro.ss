#!/usr/bin/env -S chicken-csi -s

; define-syntax
; chicken scheme does not support `syntax-case`

(define-syntax dotimes
  (syntax-rules ()
    [(_ times body ...)
     (do ((t times (- t 1)))
         ((zero? t) '())
         body ...)]))

(dotimes 3
         (display "Hello, ")
         (display "chicken.")
         (display "(dotimes)\n"))
