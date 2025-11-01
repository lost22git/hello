#!/usr/bin/env -S racket -r

(require net/http-client
         json)

(define-values (status header body-in)
  (http-sendrecv "localhost"
                 "/json"
                 #:port 9090
                 #:ssl? false))
(dynamic-wind
 (λ () (displayln "=== receive data ==="))
 (λ ()
   (printf "status: ~a\n" status)
   (for ([i (in-list header)])
     (printf "header: ~a\n" i))
   (printf "body: ~a\n" (read-json body-in)))
 (λ () (close-input-port body-in)))
