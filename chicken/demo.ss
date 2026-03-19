#!/usr/bin/env -S chicken-csi -s
(import apropos)

(display "Hello, chicken scheme\n")
(print "Hello, " "chicken " "scheme.")

(import (chicken format))
(printf "~A~N" 1)
(format #t "~A~N" 1)

(null? '())
(null? #())

(length '(1 2))
(vector-length #(1 2))
(string-length "λ")
(import (prefix utf8 utf8:))
(utf8:string-length "λ")

;; === text block ===
(display #<<DOC
  Hello, this is 
  Chicken Scheme
  :>
DOC
;; the last DOC must no indentation
         )

;; === text block with interpolation ===
(let ((name "Chicken"))
  (display #<#DOC

The scheme implementation is #{name}.
DOC
;; the last DOC must no indentation
  ))

