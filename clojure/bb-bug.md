
```clj
(def my-trampoline (fn [f & args]
                      (prn args)
                     (let [r (apply f args)]
                       (if (fn? r) (recur r) r))))
```

- Clojure (v1.12.0.1479 linux-arm64)

```
; --------------------------------------------------------------------------------
; eval (root-form): (def my-trampoline (...
; (err) Syntax error (IllegalArgumentException) compiling recur at (4ever.clj:794:36).
; (err) Mismatched argument count to recur, expected: 2 args, got: 1
```

- Babashka (v1.12.215 linux-arm64)

evaluated and no error, however this would cause the `args` to not be overwritten.

when go on to call it in the code

```clj
(= (letfn [(triple [x] #(sub-two (* 3 x)))
           (sub-two [x] #(stop? (- x 2)))
           (stop? [x] (if (> x 50) x #(triple x)))]
     (my-trampoline triple 2))
   82)
```

it would print the same `args` twice

```
; eval (root-form): (= (letfn [(triple [...
; (out) (2)
; (out) (2)
; (err) clojure.lang.ArityException: Wrong number of args (1) passed to: sci.impl.fns/fun/arity-0--1230 user /data/data/com.termux/files/home/code/hello/clojure/4ever.clj:3:30
```
