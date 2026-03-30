#!/usr/bin/env -S chicken-csi -s

; ChickenScheme thread model:
; multiple lightweight threads run on a single platform thread.

(import srfi-1
        srfi-18)

(define-syntax spawn
  (syntax-rules ()
    [(_ body ...)
     (let ([t (make-thread (lambda ()
                             body ...))])
       (thread-start! t)
       t)]))

(define a 0)
(define (work i)
  (sleep 1)
  ;; we don't need lock since single thread
  (set! a (+ a 1))
  (print "["
         (thread-name (current-thread))
         "] "
         i
         " ..."))

(for-each thread-join!
          (map (lambda (i) (spawn (work i)))
               (iota 10)))

(print "a => " a)
