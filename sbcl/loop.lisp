#!/usr/bin/env -S sbcl --script

(defun split (s &key (sep " ") (eq-fn #'equalp) (trim? t) (max-count))
  "Split a [s]tring with [sep]erator"
  (loop with len = (length s)
        repeat (+ 1 len)
        with mark = 0
        with from = 0
        for to = (+ from (length sep))
          unless (or (not max-count) (> max-count 1)) do (return (list s))
          if (<= to len)
            if (funcall eq-fn sep (subseq s from to))
              ;; found seperator
              if (and  trim? (= mark from)) 
                ;; skip empty
                do (setf mark to)
                and do (setf from to) ;; go on next round
              else 
                ;; accept empty 
                if (and max-count (= (- max-count 2) (length result)))
                  ;; reach max-size - 1 
                  collect (subseq s mark from) into result
                  and do (setf mark to)
                  and do (setf from len)  ;; collect last one on next round
                else
                  collect (subseq s mark from) into result
                  and do (setf mark to)
                  and do (setf from to) ;; go on next round
            else ;; not found seperator,
              do (incf from) ;; go on next round
          else ;; collect last one 
            if (or (not trim?) (< mark len))
              ;; accept empty
              collect (subseq s mark) into result
              and do (return result)
        finally (return result)))



(assert (equalp
         '("hello" "lisp")
         (split " hello  lisp ")))
(assert (equalp
         '("" "hello" "" "lisp" "")
         (split " hello  lisp " :trim? nil)))
(assert (equalp
         '(" hello  lisp ")
         (split " hello  lisp " :trim? nil :max-count -1)))
(assert (equalp
         '(" hello  lisp ")
         (split " hello  lisp " :trim? nil :max-count 0)))
(assert (equalp
         '(" hello  lisp ")
         (split " hello  lisp " :trim? nil :max-count 1)))
(assert (equalp
         '("" "hello  lisp ")
         (split " hello  lisp " :trim? nil :max-count 2)))
(assert (equalp
         '("" "hello"  " lisp ")
         (split " hello  lisp " :trim? nil :max-count 3)))
(assert (equalp
         '("" "hello" ""  "lisp ")
         (split " hello  lisp " :trim? nil :max-count 4)))
(assert (equalp
         '("" "hello" ""  "lisp" "")
         (split " hello  lisp " :trim? nil :max-count 5)))
(assert (equalp
         '("he" "o lisp")
         (split "hello lisp" :sep "ll")))
(assert (equalp
         '("hello lisp")
         (split "hello lisp" :sep "ll" :max-count 1)))
