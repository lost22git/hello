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

;; not chunked-seq vs chunked-seq
;;
;; N is requests, M is loads
;;
;; (map) is chunked-seq if the mapped seq is chunked-seq
;; not chunked-seq map : M = N
;; chunked-seq map     : M = (* 32 (ceil (/ N 32)))
;; 
;; (pmap) use (map) internally, is chunked-seq if the pmappd seq is chunked-seq
;; load (+ 2 N-core) when request the first, then only load 1 since request the second
;; not chunked-seq pmap : M = (+ N (+ 2 N-core))
;; chunked-seq pmap     : M = (* 32 (ceil (/ (+ N (+ 2 N-core)) 32)))

;; not chunked-seq map
(->> (iterate inc 0)
     (map #(doto % println))
     (drop 4)
     (take 4)
     vec)

;; chunked-seq map
(->> (range 33)
     (map #(doto % println))
     (drop 4)
     (take 4)
     vec)

;; not chunked-seq pmap
(->> (iterate inc 0)
     (pmap (fn [x] (Thread/sleep 1000) (printf "%d\n" x) (flush) x))
     (drop 4)
     (take 4)
     vec)

;; chunked-seq pmap
(->> (range 33)
     (pmap (fn [x] (Thread/sleep 1000) (printf "%d\n" x) (flush) x))
     (drop 4)
     (take 4)
     vec)
