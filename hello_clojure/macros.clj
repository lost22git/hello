#!/usr/bin/env -S clj -M

; define macro
(defmacro pp [expr]
  `(println ~(str expr) "=>" ~expr))

; expand macro
(macroexpand '(pp (-> (Thread/currentThread)
                      (.getName))))

; call macro
(pp (-> (Thread/currentThread)
        (.getName)))

(defmacro pp2 [expr]
  `(println (str '~expr) "=>" ~expr))

(macroexpand '(pp2 (-> (Thread/currentThread)
                       (.getName))))

(pp2 (-> (Thread/currentThread)
         (.getName)))

