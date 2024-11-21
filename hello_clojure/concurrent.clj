#!/usr/bin/env -S clj -M

; atom
(def cnt (atom 0 :validator #(>= % 0)))
(dotimes [_ 1000]
  (future (swap! cnt inc)))
(Thread/sleep 500)
(assert (= @cnt 1000))

; ref
(defn transfer [from to cnt]
  (dosync
   (alter from - cnt)
   (alter to + cnt)))

(def from (ref 1000 :validator #(>= % 0)))
(def to (ref 1000 :validator #(>= % 0)))

(dotimes [_ 1000]
  (future (transfer from to 1)))
(Thread/sleep 500)
(assert (= @cnt 1000))
(assert (= @from 0))
(assert (= @to 2000))

; agent
(def cnt (agent 0 :validator #(>= % 0)))
(send cnt inc)
(send cnt inc)
(send cnt inc)
(Thread/sleep 500)
(assert (= @cnt 3))

; java object locking
(def list (java.util.ArrayList.))
(assert (= (.size list) 0))
(dotimes [i 1000]
  (future (locking list
            (.add list i))))
(Thread/sleep 500)
(assert (= (.size list) 1000))

; pmap vs map
(defn long-time-uppercase [x]
  (Thread/sleep 1000)
  (clojure.string/upper-case x))
(time (doall (map long-time-uppercase ["concurrent" "clojure" "lisp"])))
(time (doall (pmap long-time-uppercase ["concurrent" "clojure" "lisp"])))

