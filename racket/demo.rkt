#!/usr/bin/env -S racket -r

;; === json ===

(require json)

(struct person (name age) #:transparent)

(define (map->person m)
  (person (hash-ref m 'name) (hash-ref m 'age)))

(define json->person
  (compose map->person string->jsexpr))

(let* ([s "{\"name\": \"foo\", \"age\": 11}"]
       [p (json->person s)])
  (displayln p))

;; === regex ===

(require racket/base)
(let* ([re #px"(\\w+)-(\\d+)"]
       [text "jkg-1223 ppio-2342"]
       [match-list (regexp-match* re
                                  text
                                  #:match-select
                                  (λ (match)
                                    match))])
  (displayln match-list))
