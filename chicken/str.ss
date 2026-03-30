#!/usr/bin/env -S chicken-csi -s

;; === text block ===
(display #<<DOC

              HELLO,
              CHICKEN
              :>

DOC
;; the last DOC must no indentation
         )

;; === text block with interpolation ===
(let ([name "Chicken"])
  (display #<#DOC
HELLO, #{name}. 
DOC
;; the last DOC must no indentation
;; and no suffix in same line
))

(import (chicken string)
        (chicken format)
        (chicken port))
(string-split "h-e-l--l-o" "-" #f)

(->string 1.1)
(->string '(1 2))
(string->number "1")
(string->number "1.1")
(string->number "1e-1")
(string->number "11" 16)

(format "~A HEX is 0x~X~%" 11 11)
;; sprintf => format / format #f
(sprintf "~A HEX is 0x~X~%" 11 11)
;; fprintf port => format port
(call-with-output-string
 (lambda (port)
   (fprintf port "~A HEX is 0x~X~%" 11 11)))
;; printf => format #t
(with-output-to-string
 (lambda () (printf "~A HEX is 0x~X~%" 11 11)))
