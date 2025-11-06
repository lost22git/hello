(defun hello (&optional (name "lisper"))
  (format t "HELLO ~A~&" name))

(defun main ()
  (hello "lisper"))

; Build an executable binary in shell:
;
; sbcl --load hello.lisp <<EOF 
; (sb-ext:save-lisp-and-die "hello" 
;                           :toplevel #'main
;                           :executable t
;                           :compression 22)
; EOF

