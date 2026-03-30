#!/usr/bin/env -S chicken-csi -s

(import (chicken format))

(define-record vimpackspec src name version)

(let ([vps (make-vimpackspec
            "https://github.com/Olical/conjure"
            "conjure"
            "master")])

  (vimpackspec-version-set! vps "main")
  (format
   #t
   "#<vimpackspec src=~S name=~S version=~S>~%"
   (vimpackspec-src vps)
   (vimpackspec-name vps)
   (vimpackspec-version vps)))
