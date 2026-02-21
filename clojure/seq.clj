;; seq
;; check if implements clojure.lang.ISeq
;; https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/ISeq.java

(= true (seq? '(1)))
(= false (seq? [1]))
(= true (seq? (seq [1])))
(= true (seq? (cons 1 [])))

;; chunked-seq
;; check if implements clojure.lang.IChunkedSeq
;; https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/IChunkedSeq.java

(= false (chunked-seq? '(1)))
(= true (chunked-seq? (seq [1])))
(= false (chunked-seq? (seq #{1})))
(= false (chunked-seq? (seq {:a 1})))
(= false (chunked-seq? (cons 1 [])))

;; check if implements clojure.lang.Seqable
;; https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/Seqable.java

(= true (seqable? '(1)))
(= true (seqable? [1]))
(= true (seqable? #{1}))
(= true (seqable? {:a 1}))
(= true (seqable? (seq {:a 1})))
(= true (seqable? (cons 1 [])))

;; lazy-seq
;; https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/LazySeq.java
;; https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/Iterate.java

(= clojure.lang.LazySeq (type (lazy-seq [1])))
(= clojure.lang.LazySeq (type (map inc '(1))))
(= clojure.lang.LazySeq (type (sequence (map inc) [1])))
(= clojure.lang.LazySeq (type (for [e [1]] (inc e))))
(= clojure.lang.Iterate (type (iterate inc 1)))
(= clojure.lang.Iterate (type (range)))

(= true (chunked-seq? (seq [1])))
(= false (chunked-seq? (lazy-seq [1])))

;; check if implements clojure.lang.Sequential
;; https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/Sequential.java

(= true (sequential? '(1)))
(= true (sequential? [1]))
(= false (sequential? #{1}))
(= false (sequential? {:a 1}))
(= true (sequential? (seq {:a 1})))
(= true (sequential? (cons 1 [])))
