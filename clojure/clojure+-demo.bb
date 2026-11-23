#!/usr/bin/env bb

(require '[babashka.deps :as deps])
(deps/add-deps
 '{:deps
   {io.github.tonsky/clojure-plus {:mvn/version "1.8.0"}}})

;;;; hashp

(require 'clojure+.hashp)
(clojure+.hashp/install! {:color? true})

(->> (range 10)
     #p (filter odd?)
     #p (drop 3)
     doall)

;;;; core

(use 'clojure+.core)

(if+ (and
      (= 1 2)
      :let [x 3
            y (+ x 4)]
      (> y x))
     ;; then
     (+ x y 5)
     ;; else
     6)

(when+ (and
        (= 1 2)
        :let [x 3
              y (+ x 4)]
        (> y x))
       ;; then
       (+ x y 5))

(cond+
 false   :false
 :let    [x 1]
 (= 1 x) (str x)) ; => \"1\"

(let [a 2]
  (cond+
   (= 1 a) :false
   :do     (println "a =" a)
   :else   :true))

(cond+
 (and
  (= 1 1)
  :let [x 2
        y (+ x 1)]
  (> y x))
 [x y]) ;; => [2 3]

;;;; test

(require 'clojure+.test)
(clojure+.test/install!)

(use 'clojure.test)
(deftest foo
  (testing "testing a is odd?"
    (let [a 11]
      (is (odd? a))))
  (testing "testing b is odd?"
    (let [b 10]
      (is (odd? b)))))

(clojure+.test/run #'foo)

;;;; walk

(require 'clojure+.walk)

(let [m {:a 1, :b 2}]
  #p (identical? m (clojure.walk/postwalk identity m))
  #p (identical? m (clojure+.walk/postwalk identity m)))

;;;; print 
;;;; not work to BABASHKA

(require 'clojure+.print)
; (err) java.lang.Exception: Unable to resolve classname: clojure.lang.ATransientSet [at clojure+/print.clj:7:3]
(clojure+.print/install!)

;;;; error 
;;;; not work to BABASHKA

(require 'clojure+.error)
(clojure+.error/install!)
; (err) Method addMethod on class sci.impl.io.proxy$clojure.lang.MultiFn$ff19274a not allowed! [at /data/data/com.termux/files/home/code/hello/clojure/clojure+-demo.bb]
