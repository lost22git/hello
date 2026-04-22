#!/usr/bin/env janet

(comment
  # repeat
  (repeat 3
    (print "HELLO"))

  # while
  (do
    (var n 3)
    (while (> n 0)
      (print n)
      (-= n 1)))

  # forever
  (do
    (var n 3)
    (forever (when (zero? n)
               (break))
      (print n)
      (-= n 1)))

  # for-range
  (for i 1 5 (print i))

  # each
  (each i (range 1 5) (print i))
  (each [k v] (pairs {:a 1 :b 2}) (print k "=>" v))
  (eachp [k v] {:a 1 :b 2} (print k "=>" v))

  # loop
  (loop [i :range [1 5 1] :when (odd? i)] (print i))
  (loop [i :in (range 1 5 1) :while (odd? i)] (print i))
  (loop [[k v] :pairs {:a 1 :b 2}] (print k "=>" v))

  # seq
  (seq [i :range [1 5 1] :when (odd? i)] i)
  (seq [i :in (range 1 5 1) :while (odd? i)] i)
  (seq [[k v] :pairs {:a 1 :b 2}] [k (* 2 v)])

  # generate
  (let [gen
        (generate [i :in (range 1 10)] i)]
    (print (resume gen))
    (print (resume gen))
    (each i gen (print i)))

  (let [generator
        (coro (yield 1) (yield 10) (yield 100))]
    (each v generator
      (print v)))

  # end comment
  )
