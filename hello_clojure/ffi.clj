#!/usr/bin/env -S clj -M

 ; invoke java constructor
(def mylist (java.util.ArrayList.))

; invoke java methods
(assert (= 0 (.size mylist)))
(doto mylist
  (.add  "foo")
  (.add  "bar"))
(assert (= 2 (.size mylist)))
(assert (= mylist ["foo" "bar"]))

; invoke java static methods
; java.lang is preluded so FQDN not needed 
(assert (= 11 (Integer/parseInt "11")))

; Clojure can't know type of variables/arguments during compile time, 
; so clojure use java reflection to invoke methods/fields.
; If we want to avoid java reflection to improve performance
; we should add type hints to tell compiler type info.
;
; [java interop performance](https://m.youtube.com/watch?v=s_xjnXB994w)

(set! *warn-on-reflection* true)

;; use java reflection
; (defn my-upcase [s] (.toUpperCase s))
; (->  (str "ha" "lo")
;      my-upcase
;      (= "HALO")
;      assert)

; invoke virtual java/lang/String#toUpperCase
(defn my-upcase ^String [^String s] (.toUpperCase s))
(-> (str "ha" "lo")
    my-upcase
    (= "HALO")
    assert)
