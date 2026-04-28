#!/usr/bin/env -S sbcl --script

(defun fib (n)
  (if (< n 2) 1
      (+ (fib (- n 1)) (fib (- n 2)))))

(let ((n 40))
  (format t "~A~%" (fib n)))


