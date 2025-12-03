#!/usr/bin/env -S chez --script

;; === print ===

(display "display print: hello chez\n")
(format #t "format print: ~A~%" 42)

;; === function ===

((lambda (a b) (+ a b)) 1 2)
(define (add a b)
  (+ a b))
(add 1 2)
(apply add '(1 2))
(fold-left add 0 '(1 2))
(fold-right add 0 '(1 2))

;; === constantly ===

(define (constantly x)
  "Returns a function that takes any number of arguments and returns x.
  "
  (lambda args x))

((constantly 1))
((constantly 1) 1)
((constantly 1) "1" 1)

;; === identity ===

(define (identity x)
  "Returns its argument.
  "
  x)

(assert (= 1 (identity 1)))

;; === complement ===

(define (complement f)
  "Takes a fn f and returns a fn that takes the same arguments as f,
   has the same effects, if any, and returns the opposite truth value.
  "
  (lambda xs
          (not (apply f xs))))

(assert ((complement even?) 1))

;; === partial ===

(define (partial f . args)
  "Takes a function f and fewer than the normal arguments to f, and
   returns a fn that takes a variable number of additional args. When
   called, the returned function calls f with args + additional argsj.
   "

  (lambda xs
          (apply f (append args xs))))

(define partial-zero (partial string-upcase))
(assert (string=? "HELLO" (partial-zero "hello")))
(define partial-one (partial string-upcase "hello"))
(assert (string=? "HELLO" (partial-one)))

;; === comp ===
;; sequentialize fns

(define (comp f . gs)
  "Takes a set of functions and returns a fn that is the composition
   of those fns.  The returned fn takes a variable number of args,
   applies the rightmost of fns to the args, the next
   fn (right-to-left) to the result, etc.
   "

  (letrec ((rf (lambda (state cur)
                       (lambda (x)
                               (state (cur x))))))

          (fold-left rf f gs)))

;; compose one function
(define comp-one (comp
                  string-upcase))
(assert (string=? "HELLO"  (comp-one "hello")))

;; compose two function
(define comp-two
  (comp
   string-upcase
   (lambda (x)
           (substring x 1 (string-length x)))))
(assert (string=? "ELLO" (comp-two "hello")))

;; compose tree function
(define comp-three
  (comp
   (lambda (x) (string-ref x 0))
   string-upcase
   (lambda (x)
           (substring x 1 (string-length x)))))

(assert (char=? #\E (comp-three "hello")))

;; === juxt ===
;; parallelize fns

(define (juxt f . gs)
  "Takes a set of functions and returns a fn that is the juxtaposition
   of those fns.  The returned fn takes a variable number of args, and
   returns a vector containing the result of applying each fn to the
   args (left-to-right).
   ((juxt a b c) x) => [(a x) (b x) (c x)]
  "
  (lambda xs
          (map (lambda (f)
                       (apply f xs))
               (cons f gs))))

(define (hash-1 x) (mod x 3))
(define (hash-2 x) (mod x 7))
(define (hash-3 x) (mod x 11))
((juxt hash-1) 109)
((juxt hash-1 hash-2) 109)
((juxt hash-1 hash-2 hash-3) 109)

;; === some-fn ===

(define (some-fn . ps)
  "Takes a set of predicates and returns a function f that returns the first logical true value
  returned by one of its composing predicates against any of its arguments, else it returns
  logical false. Note that f is short-circuiting in that it will stop execution on the first
  argument that triggers a logical true result against the original predicates.
  "
  (lambda xs
          (let loop ((lst ps))
               (and
                (not (null? lst))
                (or
                 (apply (car lst) xs)
                 (loop (cdr lst)))))))

((some-fn) 1) ;; #f
((some-fn even?) 1) ;; #f
((some-fn even?) 2) ;; #t
((some-fn even? positive?) 1) ;; #t
((some-fn even? positive?) -1) ;; #f

;; === transducer ===

(define (x-map f)
  "Returns a transducer"
  (lambda (rf)
          (lambda (s e)
                  (rf s (f e)))))

(define (x-filter f)
  "Returns a transducer"
  (lambda (rf)
          (lambda (s e)
                  (if (f e)
                    (rf s e)
                    s))))

(define (transduce xf f init col)
  (fold-left (xf f) init col))

(let* ((xf (comp
            (x-filter odd?)
            (x-map (lambda (x)
                           (format #t "map x=~A\n" x)
                           (* x x)))))
       (f (lambda (s e) (format #t "rf s=~A e=~A\n" s e) (+ s e)))
       (res (transduce xf f 0 '(1 2 3 4 5))))
      (format #t "transduce result: ~A~%" res))



