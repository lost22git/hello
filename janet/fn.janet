#!/usr/bin/env janet

(comment
  # https://janet-lang.org/docs/functions.html

  (defn hello
    "Hello to given name"
    [name]
    (print "Hello " name " !!"))

  (def name :private "The name of Janet" "Janet")
  (hello name)

  # lambda |()
  (->> ["Janet" "Fennel"]
       (map |(string/ascii-upper $)))

  # &keys params
  (defn add-keys [&keys {:a a :b b}]
    (+ a b))

  (add-keys :a 1 :b 2)
  (add-keys ;(kvs {:a 1 :b 2}))
  (apply add-keys (kvs {:a 1 :b 2}))

  # &named params
  (defn add-named [&named a b]
    (+ a b))

  (add-named :a 1 :b 2)
  (add-named ;(kvs {:a 1 :b 2}))
  (apply add-named (kvs {:a 1 :b 2}))

  # end comment
  )

(comment
  # partial
  ((partial + 10) 3)

  # complement
  ((complement odd?) 2)

  # comp
  ((comp |(apply array/concat $) map) |(range 1 $) (range 1 4))

  # juxt
  ((juxt odd? pos?) 1)

  # end comment
  )
