#!/usr/bin/env -S racket -r

(require json)

(struct person (name age) #:transparent)

(define (map->person m)
  (person (hash-ref m 'name) (hash-ref m 'age)))

(define json->person
  (compose map->person string->jsexpr))

(let* ([s "{\"name\": \"foo\", \"age\": 11}"]
       [p (json->person s)])
  (displayln p))
