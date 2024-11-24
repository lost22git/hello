#!/usr/bin/env -S clj -M
; nil
(assert (= (type nil) nil))
; keyword
(assert (= (type :halo) clojure.lang.Keyword))
; bool
(assert (= (type true) java.lang.Boolean))
; number
(assert (= (type 1) java.lang.Long))
(assert (= (type (int 1)) java.lang.Integer))
(assert (= (type (short 1)) java.lang.Short))
(assert (= (type (byte 1)) java.lang.Byte))
(assert (= (type 1.0) java.lang.Double))
(assert (= (type (float 1.0)) java.lang.Float))
(assert (= (type 1N) clojure.lang.BigInt))
(assert (= (type 1M) java.math.BigDecimal))
(assert (= (type 1/3) clojure.lang.Ratio))
; char
(assert (= (type \c) java.lang.Character))
(assert (= (type "halo") java.lang.String))
; collection
(assert (= (type [1 2]) clojure.lang.PersistentVector))
(assert (= (type '(1 2)) clojure.lang.PersistentList))
(assert (= (type #{1 2}) clojure.lang.PersistentHashSet))
(assert (= (type {1 2}) clojure.lang.PersistentArrayMap)) ; entries <= 8 -> liner search
(assert (= (type {1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9}) clojure.lang.PersistentHashMap)) ; entries > 8 -> hash search
; record
(defrecord Book [name tags])
(assert (= (type (->Book "the clojure book" ["programming"])) user.Book))
; function
; (assert (= (type +) clojure.core$_PLUS_))

; equality
(assert (= 1 1))
(assert (= 1 (int 1)))
(assert (not= 1 1.0))
(assert (= 1.0 1.0))
(assert (= 1.0 (float 1.0)))
; vector eq list
(assert (= [1 2] '(1 2)))
; seq eq requires order
(assert (not= [1 2] '(2 1)))
; set eq not requires order
(assert (= #{1 2} #{2 1}))
; map eq not requires order
(assert (= {1 1 2 2} {2 2 1 1}))
(assert (= {1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9} {9 9 2 2 3 3 4 4 5 5 6 6 7 7 8 8 1 1}))
; record != map
(assert (not= (->Book "the clojure book" ["programming"]) {:name "the clojure book" :tags ["programming"]}))

; 
(assert (= "clojure.lang.Atom" (type (atom 0))))
(assert (= "clojure.lang.Ref" (type (ref 0))))
(assert (= "clojure.lang.Agent" (type (agent 0))))
