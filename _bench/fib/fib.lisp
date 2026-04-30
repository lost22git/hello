(declaim (optimize (speed 3) (debug 0) (safety 0))
         (ftype (function (fixnum) fixnum) fib))
(defun fib (n)
  (if (< n 2) 1
      (+ (fib (- n 1)) (fib (- n 2)))))

(defun main ()
  (let ((n 40))
    (format t "~A~%" (fib n))))

#+sbcl
 (sb-ext:save-lisp-and-die "fib_sbcl.exe"
                           :toplevel #'main
                           :executable t
                           :compression 9)
