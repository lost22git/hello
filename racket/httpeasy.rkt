#!/usr/bin/env -S racket -r

;; raco pkg install http-easy-lib

(require net/http-easy)

(define (with-retry f #:retry-max [retry-max 0])
  (λ ()
    (let loop ([retry-count 0])
      (with-handlers
          ([exn:fail?
            (λ (exn)
              (if (= retry-count retry-max)
                  (raise exn)
                  (begin
                    (printf "retry #~a\n"
                            (+ retry-count 1))
                    (loop (+ retry-count 1)))))])
        (f)))))

(define (upload path)
  (with-input-from-file
   path
   (λ ()
     (printf "uploading... \n")
     (response-json
      (post
       "https://api.imgur.com/3/image?client_id=546c25a59c58ad7"
       #:headers
       (hasheq 'Referer "https://imgur.com/")
       #:data (current-input-port))))))

(define upload-with-retry
  (with-retry (λ () (upload "./test.jpg"))
              #:retry-max 3))

(pretty-print (upload-with-retry))
