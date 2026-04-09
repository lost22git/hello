#!/usr/bin/env -S racket -r

(require racket/base)

;; regexp-match-exact?
(regexp-match-exact? #px"(\\d+\\.){3}\\d+"
                     "110.120.0.1")

;; regexp-match*
(equal?
 '(("110.120.0.1" "24") ("110.120.1.1" "24"))
 (regexp-match* #px"((\\d+\\.){3}\\d+)/(\\d+)"
                "110.120.0.1/24 110.120.1.1/24"
                #:match-select
                (λ (match)
                  ;; (println match)
                  (list (cadr match) ;; group 1
                        (cadddr match)) ;; group 3
                  )))

;; regexp-split
(equal? '("110" "120" "0" "1")
        (regexp-split #px"\\." "110.120.0.1"))

;; regexp-replace*
(string=?
 "110.120.0.1 110.120.1.1"
 (regexp-replace* #px"((\\d+\\.){3}\\d+)/(\\d+)"
                  "110.120.0.1/24 110.120.1.1/24"
                  (λ (g0 g1 g2 g3) g1))
 (regexp-replace* #px"((\\d+\\.){3}\\d+)/(\\d+)"
                  "110.120.0.1/24 110.120.1.1/24"
                  "\\1"))
