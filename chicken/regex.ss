#!/usr/bin/env -S chicken-csi -s

(import (chicken irregex))

;; irregex-search
(define (irregex-match-list irx str #!key (pos 0))
  "Find match list.
  Compared wtih irregex-extract, it returns list of match instead of list of substring of match.
  "
  (let ([m (irregex-search irx str pos)])
    (if m
        (cons m
              (irregex-match-list
               irx
               str
               #:pos (irregex-match-end-index m)))
        '())))

;; irregex-extract
(equal? (irregex-extract "\\d+" "110.120.0.1")
        (map irregex-match-substring
             (irregex-match-list "\\d+"
                                 "110.120.0.1")))

(equal?
 '(("110.120.0.1" "24") ("110.120.1.1" "24"))
 (map (lambda (m)
        (list (irregex-match-substring m 1)
              (irregex-match-substring m 3)))
      (irregex-match-list
       "((\\d+\\.){3}\\d+)/(\\d+)"
       "110.120.0.1/24 110.120.1.1/24")))

;; irregex-match
(let ([m (irregex-match
          "(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)"
          "110.120.0.1")])
  (string=? "110" (irregex-match-substring m 1))
  (string=? "120" (irregex-match-substring m 2))
  (string=? "0" (irregex-match-substring m 3))
  (string=? "1" (irregex-match-substring m 4)))

;; irregex
(eq? #f (irregex-match (irregex "foo") "FOO"))
;; ignore-case
(irregex-match (irregex "foo" 'i) "FOO")

;; irregex-replace
(string=? "zoobar"
          (irregex-replace "foo" "foobar" "zoo"))
;; irregex-replace/all
(string=? "110.120.0.1 110.120.1.1"
          (irregex-replace/all
           "((\\d+\\.){3}\\d+)/(\\d+)"
           "110.120.0.1/24 110.120.1.1/24"
           (lambda (m)
             (irregex-match-substring m 1))))

;; irregex-split
(equal? '("110" "120" "0" "1")
        (irregex-split "\\." "110.120.0.1"))
