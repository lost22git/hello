#!/usr/bin/env -S sbcl --script

(defmacro λ (args &body body)
  `(lambda ,args ,@body))

;; remove symbol

(defun remove-symbol (s &optional (p *package*))
  (unintern s p)
  (makunbound s))

(remove-symbol 'book)

(concatenate 'string
             (concatenate 'list "Hello, SBCL."))

(class-of 'abc)
(type-of 'abc)

 ;; keyword a is an intern symbol of gloabl KEYWORD package
(eq :a :a)
