#!/usr/bin/env -S clj -M
 ; nil
(assert (= (type nil) nil))

; symbol
(assert (= (type 'sdfsdfsdf) clojure.lang.Symbol))

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
; vector
(assert (= (type [1 2])
           clojure.lang.PersistentVector))
; list
(assert (= (type '(1 2))
           clojure.lang.PersistentList))

; cons
(assert (= (type (cons 1 '(2 3)))
           clojure.lang.Cons))

; set
(assert (= (type #{1 2})
           clojure.lang.PersistentHashSet))

; array map: entries <= 8 -> linear search
(assert (= (type {1 2})
           clojure.lang.PersistentArrayMap))

; hash map: entries > 8 -> hash search
(assert (= (type {1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9})
           clojure.lang.PersistentHashMap))

; seq
(assert (= (type (seq [1 2]))
           clojure.lang.PersistentVector$ChunkedSeq))
(assert (= (type (seq '(1 2)))
           clojure.lang.PersistentList))
(assert (= (type (seq #{1 2}))
           clojure.lang.APersistentMap$KeySeq))
(assert (= (type (seq {:a 1 :b 2}))
           clojure.lang.PersistentArrayMap$Seq))

; lazy-seq
(assert (= (type (lazy-seq [1 2]))
           clojure.lang.LazySeq))

; record
(defrecord Book [name tags])
(assert (= (type (->Book "the clojure book" ["programming"]))
           user.Book))

; function
(assert (= (type str) clojure.core$str))

; regex
(assert (= (type #"\d+")
           java.util.regex.Pattern))

; uuid
(assert (= (type #uuid "790d1277-c125-4bf7-afee-0111b07e6ece")
           java.util.UUID))

; date
(assert (= (type #inst "2022-09-09T02:20:30Z")
           java.util.Date))

; concurrent types
; future
(assert (contains? (supers (type (future 1)))
                   java.util.concurrent.Future))

; ; promise
; (supers (type (promise)))

; atom
(assert (= (type (atom 0))
           clojure.lang.Atom))
; ref
(assert (= (type (ref 0))
           clojure.lang.Ref))
; agent
(assert (= (type (agent 0))
           clojure.lang.Agent))

; supers: find type of super class and interfaces
; (println (supers (type str)))

;;;;;;;;;;;;;;
;; equality ;;
;;;;;;;;;;;;;;

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

