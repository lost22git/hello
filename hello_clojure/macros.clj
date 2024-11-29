#!/usr/bin/env -S clj -M

(defmacro pp [expr]
  `(println ~(str expr) "=>" ~expr))

(pp (-> (Thread/currentThread)
        (.getName)))

(defmacro pp2 [expr]
  `(println (str '~expr) "=>" ~expr))

(pp2 (-> (Thread/currentThread)
         (.getName)))

