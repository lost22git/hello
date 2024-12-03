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

; pp2
(macroexpand '(pp2 (-> (Thread/currentThread)
                       (.getName))))

(pp2 (-> (Thread/currentThread)
         (.getName)))

; pp3
(defmacro pp3 [expr]
  (list 'println (str expr) "=>" expr))

(macroexpand '(pp3 (-> (Thread/currentThread)
                       (.getName))))

(pp3 (-> (Thread/currentThread)
         (.getName)))
