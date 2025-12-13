#!/usr/bin/env -S racket -r

(require racket/async-channel)

(struct actor
        (mailbox exitsignal [state #:mutable])
  #:transparent)

(define (make-actor init-state)
  (actor (make-async-channel)
         (make-async-channel)
         init-state))

(define (actor-monitor a)
  (async-channel-get (actor-exitsignal a)))

(define (actor-send a msg)
  (async-channel-put (actor-mailbox a) msg))

(define (actor-stop a)
  (actor-send a #f))

(define (actor-start a f)
  (thread
   (λ ()
     (let loop ()
       (let ([msg (async-channel-get
                   (actor-mailbox a))]
             [state (actor-state a)])
         (if msg
             (match (with-handlers
                        ([exn:fail?
                          (λ (exn)
                            (list 'err exn))])
                      (list 'ok (f state msg)))
               [(list 'ok new-state)
                (set-actor-state! a new-state)
                (loop)]
               [(list 'err exn)
                ;; exit with error
                (async-channel-put
                 (actor-exitsignal a)
                 (list 'err exn))])
             (begin
               (printf
                "actor is stopping normally\n")
               ;; exit with normal
               (async-channel-put
                (actor-exitsignal a)
                'normal))))))))

;; === Test ===

(define a (make-actor 0))
(actor-start a
             (λ (state msg)
               (match msg
                 [(list 'add val) (+ state val)]
                 [(list 'del val)
                  (- state val)])))
(for ([i 4])
  (printf "send msg to actor...\n")
  (actor-send a (list 'add 10))
  (sleep 1)
  (printf "actor state: ~a\n" (actor-state a)))

(printf "stop actor...\n")
(actor-stop a)
(printf "actor was stopped with: ~a\n"
        (actor-monitor a))
