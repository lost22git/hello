#!/usr/bin/env -S sbcl --script

(defmacro λ (args &body body)
  `(lambda ,args ,@body))

;; format t -> printf
;; format nil -> sprintf

(format t "~R~%" 99880)
(format t "~X~%" 99880)
(format t "~D~%" 99880)
(format t "~B~%" 99880)
(format t "~A~%" 99880)

(defun print-div-mod (m n)
  (format t "~A / ~A = ~A~%" m n (/ m n))
  (format t "~A % ~A = ~A~%" m n (mod m n)))

(print-div-mod 10 3)
(print-div-mod 10 -3)
(print-div-mod -10 -3)
(print-div-mod -10 3)

(let ((lisp (list "sbcl" "clojure")))
  (format t "car: ~A~%" (car lisp))
  (format t "cdr: ~A~%" (cdr lisp))
  (format t "cadr: ~A~%" (cadr lisp)) ;; car of cdr
  (format t "cddr: ~A~%" (cddr lisp)) ;; cdr of cdr
  (format t "first: ~A~%" (first lisp))
  (format t "second: ~A~%" (second lisp))
  (format t "last: ~A~%" (last lisp))
  (format t "rest: ~A~%" (rest lisp)))

(cdr (cons 1 2))
(last (cons 1 2))
(rest (cons 1 2))

(cdr (list 1 2))
(last (list 1 2))
(rest (list 1 2))

(cdr (cons 1 '(2)))
(last (cons 1 '(2)))
(rest (cons 1 '(2)))

(equal (list 1 2) (cons 1 (cons 2 nil)))
(equal (list 1 2) (cons 1 '(2)))
(equal (list 1 2) (cons 1 2))

(pprint (find "foo" '("bar" "zar" "foo") :test #'equal))

;; === LOOP ===

;; loop list using `in`
(loop for i in (list "sbcl" "clojure" "janet")
      do (pprint i))
;; loop vector using `across`
(loop for i across (vector "sbcl" "clojure" "jaent")
      do (pprint i))

;; loop string using  `across`
(loop for c across "HELLO" do (pprint c))

;; loop hash-table
(defparameter *lang-to-country* (make-hash-table))
(setf (gethash :lua *lang-to-country*) "Brazil")
(setf (gethash :ruby *lang-to-country*) "Japan")
(loop for k being the hash-key of *lang-to-country*
      do (pprint k))
(loop for v being the hash-value of *lang-to-country*
      do (pprint v))
(loop for k being the hash-key of *lang-to-country*
      using (hash-value v)
      do (format t "~A => ~A~&" k v))

;; with-hash-table-iterator 
(with-hash-table-iterator (next *lang-to-country*)
  (loop (multiple-value-bind (ok k v) (next)
                             (unless ok (return))
                             (format t "~A => ~A~&" k v))))

;; maphash
(maphash (λ (k v)
            (format t "~A => ~A~&" k v))
         *lang-to-country*)

;; === IO ===

(with-open-file (in #p"demo.lisp" :direction :input :if-does-not-exist nil)
  (when in
    (loop for line = (read-line in nil nil)
          while line
          do (format t "~A~&" line))))

(assert (string=
         "HELLO-LISP"
         (with-output-to-string (s)
           (write-string "HELLO" s)
           (write-char #\- s)
           (write-string "LISP" s))))

;; === destructing ===

;; destructing list

(destructuring-bind (a b) '("foo" "bar")
                    (print a)
                    (print b))

;; destructing multiple-value

(multiple-value-bind (a b) (values 1 2)
                     (+ a b))

;; destructing struct

(defstruct book isbn title)

(let ((b (make-book :isbn "abc" :title "On Lisp")))
  (with-slots  (isbn title)  b
    (print isbn)
    (print title)))

;; === remove symbol ===

(defun remove-symbol (s &optional (p *package*))
  (unintern s p)
  (makunbound s))

(remove-symbol 'book)

