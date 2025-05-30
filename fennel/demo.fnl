;; [ 0 0 ]
;; ERR: first is 1 but got 0
;; [ 0 1 ]
;; ERR: first is 1 but got 0
;; [ 1 1 ]
;; ERR: second is 0 but got 1
;; [ 1 0 ]
;; PASS
(each [_ [first second] (ipairs [[0 0] [0 1] [1 1] [1 0]])]
  (print "[" first second "]")
  ;; case-try or match-try
  ;; pattern matching chain
  (case-try first
    1 second
    0 (print "PASS")
    (catch 0 (print "ERR: first is 1 but got 0") ;;
           1 (print "ERR: second is 0 but got 1") ;;
           _ (print "ERR: got a value which neither 1 nor 0"))))
