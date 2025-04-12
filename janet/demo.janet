#!/usr/bin/env janet

(doc pp) # find symbol `pp`
(print (string/repeat "=" 22))
(doc "dir") # fuzzy find symbol matched "dir"
(print (string/repeat "=" 22))


(eprint "I'm printed to STDERR")
(print "I'm printed to STDIN")


# multi-lines raw string (text block)
(print ``
# Languages
  - `Janet`
  - `Fennel`
``)


# array (mutable) => @[]
# tuple (immutable) => []
(-> ["Janet" "Fennel"]
    type
    pp)

(-> @["Janet"]
    (array/push "Fennel")
    type
    pp)


# table (mutable) => @{}
# struct (immutable) => {}
(-> {:name "Janet" :family :Lisp}
    type
    pp)

(-> @{:name "janet" :family :Lisp}
    (put :name "Janet")
    type
    pp)


# ; => splice
(def langs [;["Fennel" "Clojure"] "Janet"])
(pp langs)


(comment
  (try
    (do
      (def n (getline "INPUT A NUMBER: "))
      (print "YOUR NUMBER IS: " (int/s64 (string/trim n))))
    ([e] (eprint "ERROR: " e))))


# for i [1,5) do-effect
(for i 1 5
  (print i))

# loop bindings do-effect
(loop [i :range [1 5 1]]
  (print i))
(loop [i :in (range 1 5 1)]
  (print i))
(each i (range 1 5 1)
  (print i))

# === functions ===
#
# https://janet-lang.org/docs/functions.html

(defn hello
  "Hello to given name"
  [name]
  (print "Hello " name " !!"))

(def name :private "The name of Janet" "Janet")
(hello name)

# lambda |()
(->> ["Janet" "Fennel"]
     (map |(string/ascii-upper $))
     pp)

# fib
(defn fib [n]
  (case n
    0 1
    1 1
    (+ (fib (- n 1)) (fib (- n 2)))))

(print "fib(11): " (fib 11))

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
