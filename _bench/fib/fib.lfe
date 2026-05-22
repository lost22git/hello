(defmodule fib 
  (export (main 0)))

(defun fib 
  ([0] 1)
  ([1] 1)
  ([n] 
    (+ (fib (- n 1)) (fib (- n 2)))))

(defun main ()
  (let [(n 40)]
    (io:format "~p~n" (list (fib n)))))
