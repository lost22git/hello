
(defmodule foo (export all))

(defun double (x) (* x 2))

(defun fib
  ((x) (when (is_list x)) 
    (fib (list_to_integer x)))
  ((0) 1)
  ((1) 1)
  ((x) (when (is_integer x)) 
    (+ (fib (- x 1)) (fib (- x 2)))))


(defun fib-handler0 ()
  (receive
    ((tuple pid n) 
     (timer:sleep 1000)
     (! pid (tuple 'ok (fib n)))
     (fib-handler))
    ((tuple 'EXIT from reason) 
     (lfe_io:format "fib-handler: received a [EXIT] signal from: ~p with reason: ~p~n" (list from reason)))))
 
 
; a fib handler for running on a process 
(defun fib-handler ()
  (process_flag 'trap_exit 'true) ; trap EXIT signal
  (fib-handler0))

;=============================================

(defun fib-request0 (n) 
  (! 'fib-server (tuple (self) n))
  (receive
    ((tuple 'ok r) 
     (lfe_io:format "fib(~w) -> ~w~n" (list n r)))
    (after 1000
      (lfe_io:format "fib(~w) -> timeout~n" (list n))
      ; receive and drop a timeout message
      (receive (_ ())))))

; a fib request for running on a process 
(defun fib-request (fiber-server-pid) 
  ; link 'fib-handler with current process 
  ; if one throws an error, another will also throw
  ; if one exits, another will receive the EXIT signal when trap_exit
  (link fiber-server-pid)
  (fib-request0 11)
  (fib-request0 12)
  (exit 'fiber-request))
