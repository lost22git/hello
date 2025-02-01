#!/usr/bin/env -S clj -M

(require '[clojure.pprint :refer [pprint]])

(defn thread-name []
  (->>
   (Thread/currentThread)
   ((juxt #(.getName %) #(.getId %)))
   (clojure.string/join "#")))

(defn inspect-tap-loop
  []
  (let
   [t @@#'clojure.core/tap-loop]
    {:type (type t)
     :id (.getId t)
     :name (.getName t)
     :state (str (.getState t))}))

(defn pprint-tap-handle [v]
  (->
   (str "(pprint)" "(" (thread-name) ")>" v)
   pprint))

(defn println-tap-handle [v]
  (->
   (str "(println)" "(" (thread-name) ")>" v)
   println))

(println "tap-loop:" (inspect-tap-loop))

;; add-tap handler
(add-tap pprint-tap-handle)
(add-tap println-tap-handle)

(println "tap-loop (after call add-tap):" (inspect-tap-loop))

(assert
 (tap> "1"))

;; remote-tap handler 
(remove-tap pprint-tap-handle)

(assert
 (tap> "2"))

(Thread/sleep 1000)

(remove-tap println-tap-handle)
(println "tap-loop (after remove all tap handlers):" (inspect-tap-loop))

(Thread/sleep 1000)

