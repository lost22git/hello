#!/usr/bin/env -S clj -M

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; atom 
;; single state 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def cnt (atom 0 :validator #(>= % 0)))
(dotimes [_ 1000]
  (future (swap! cnt inc)))
(Thread/sleep 500)
(assert (= @cnt 1000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ref
;; STM (Software Transaction Memory)
;; multiple states
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn transfer [from to cnt]
  (dosync
   (alter from - cnt)
   (alter to + cnt)))

(def from (ref 1000 :validator #(>= % 0)))
(def to (ref 1000 :validator #(>= % 0)))

(dotimes [_ 1000]
  (future (transfer from to 1)))
(Thread/sleep 500)
(assert (= @from 0))
(assert (= @to 2000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; agent
;; single state sequential asynchronous computations
;; could simulate java.util.concurrent.CompletableFuture 
;; # functions
;; - send / send-off / send-via
;; - await / await-for
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comment (apropos "agent"))
(def a (agent 0 :validator #(>= % 0)))
(send-off a (fn [state id]
              (let [t (Thread/currentThread)]
                (assert (= 0
                           state))
                (println (System/currentTimeMillis) (.getName t) id)
                (Thread/sleep 1000)
                id)) 1)
(send-off a (fn [state id]
              (let [t (Thread/currentThread)]
                (assert (= 1
                           state))
                (println (System/currentTimeMillis) (.getName t) id)
                (Thread/sleep 300)
                id)) 2)
(await a)
(assert (= 2
           @a))

;; simulate java.util.concurrent.CompletableFuture
(def a (agent 0))
(-> a
    (send (fn [s]
            (assert (= 0
                       s))
            (print-str "a" s)))
    (send-off (fn [s]
                (assert (= "a 0"
                           s))
                (Thread/sleep 1000)
                (clojure.string/upper-case s)))
    (send count))

(await a)
(assert (= 3
           @a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; java object locking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def list (java.util.ArrayList.))
(assert (= (.size list) 0))
(dotimes [i 1000]
  (future (locking list
            (.add list i))))
(Thread/sleep 500)
(assert (= (.size list) 1000))

(shutdown-agents)
