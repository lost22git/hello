#!/usr/bin/env -S racket -r

(define (hello [name "alice"] #:times [times 1])
  "hello to someone"
  (for ([i (in-range 0 times)])
    (printf "Hello ~A\n" (string-titlecase name))))

(hello)
(hello "bob" #:times 2)
(hello #:times 2)
(hello #:times 2 "cindy")

(for ([c (in-string "hello")])
  (printf "~A\n" c))

(for ([e (in-list '(("h" . 1) ("l" . 2)))])
  (printf "~A\n" e))

(for ([(k v) (in-hash #hash(("h" . 1) ("l" . 2)))])
  (printf "~A=>~A\n" k v))

(for ([k (in-hash-keys #hash(("h" . 1) ("l" . 2)))])
  (printf "~A\n" k))

(for ([k (in-hash-values #hash(("h" . 1) ("l" . 2)))])
  (printf "~A\n" k))
