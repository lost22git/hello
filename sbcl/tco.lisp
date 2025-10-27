#!/usr/bin/env -S sbcl --script

(defun fib (n)
  (if (<= n 1)
    1
    (+ (fib (- n 1)) (fib (- n 2)))))

; (format t "fib(0) = ~A~%" (fib 0))
; (format t "fib(1) = ~A~%" (fib 1))
; (format t "fib(11) = ~A~%" (fib 11))

(defun fib-tail-recur (n)
  (defun visit (i a b)
    (if (= i 0)
      a
      (visit (- i 1) (+  a b) a)))

  (visit n 1 0))

; (format t "fib-tail-recur(0) = ~A~%" (fib-tail-recur 0))
; (format t "fib-tail-recur(1) = ~A~%" (fib-tail-recur 1))
; (format t "fib-tail-recur(11) = ~A~%" (fib-tail-recur 11))
; (format t "fib-tail-recur(111) = ~A~%" (fib-tail-recur 111))
; (format t "fib-tail-recur(1111) = ~A~%" (fib-tail-recur 1111))

(defun iota (end &optional (start 0) (step 1))
  (loop :for n :from start :below end :by step :collect n))
(defun rf (lst i)
  (list (reduce #'+ lst) (first lst)))
(defun fib-reduce (n)
  (first (reduce #'rf (iota n) :initial-value (list 1 0))))

; (format t "fib-reduce(0) = ~A~%" (fib-reduce 0))
; (format t "fib-reduce(1) = ~A~%" (fib-reduce 1))
; (format t "fib-reduce(11) = ~A~%" (fib-reduce 11))
; (format t "fib-reduce(111) = ~A~%" (fib-reduce 111))
; (format t "fib-reduce(1111) = ~A~%" (fib-reduce 1111))

(defun fib-loop (n)
  (let ((data '(1 0)))
    (loop repeat n
          do (setf data (list (apply #'+ data) (first data))))
    (first data)))

; (format t "fib-loop(0) = ~A~%" (fib-loop 0))
; (format t "fib-loop(1) = ~A~%" (fib-loop 1))
; (format t "fib-loop(11) = ~A~%" (fib-loop 11))
; (format t "fib-loop(111) = ~A~%" (fib-loop 111))
; (format t "fib-loop(1111) = ~A~%" (fib-loop 1111))

; (let ((n 111111))
;   (format t "--- fib-tail-recur ----------------------")
;   (time (fib-tail-recur n))
;   (format t "--- fib-reduce --------------------------")
;   (time (fib-reduce n))
;   (format t "--- fib-loop ----------------------------")
;   (time (fib-loop n)))


