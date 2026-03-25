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

(apropos "hash" #:macros? #t)
