#!/usr/bin/env -S chicken-csi -s

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
