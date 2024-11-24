; define immutable variable
(local imvar 1)
;(set imvar 2) ; compile error!
(assert (= imvar 1))

; define mutable variable
(var mvar 1)
(assert (= mvar 1))
(set mvar 2)
(assert (= mvar 2))

; define a function
(fn add [x y]
  (+ x y))

(assert (= 3 (add 1 2)))
(assert (= 3 (add (table.unpack [1 2]))))

; pattern matching to handle error (multi-return-values) 
(fn mydiv [x y]
  (if (= 0 y)
      (values nil "cannot div 0")
      (values (/ x y) nil)))

(case (mydiv 1 0)
  (nil err-msg) (print "Got error: " err-msg)
  r (print "Got value:" r))

; for 
(for [i 1 10 2]
  (print i))

; each
(each [i v (ipairs ["halo" "fennel"])]
  (print i v))

(each [k v (pairs {:foo "halo" :bar "fennel"})]
  (print k v))

; tables not= forever
(assert (not= [1 2] [1 2]))
(assert (not= {:a 1 :b 2} {:a 1 :b 2}))
