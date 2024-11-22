#!/usr/bin/env bb
(require '[clojure.core.async :as a :refer [>!! >! <!! <!]])
; chan: channel
; thread: submit fn to executor and return `channel` to receive result
; future: submit fn to executor and return `future` to receive result
; >!!: send blocking
; >!: send nonblocking (used in go block)
; <!!: receive blocking
; <!: receive nonblocking (used in go block)

(defn current-thread-name []
  (let [t (Thread/currentThread)
        id (.getId t)
        name (.getName t)]
    (str "[" id "-" name "]")))

(println ";;;;;;;;;;;;")
(println ";; thread ;;")
(println ";;;;;;;;;;;;")
(let [c (a/chan 2)
      receiver (a/thread
                 (dotimes [_ 10]
                   (println (str (current-thread-name) " received: " (<!! c)))))
      sender1  (a/thread
                 (dotimes [i 5]
                   (println (str (current-thread-name) " send: " i))
                   (>!! c i)))
      sender2 (a/thread
                (doseq [i (range 5 10)]
                  (println (str (current-thread-name) " send: " i))
                  (>!! c i)))]
  ; alts: select
  (dotimes [_ 3]
    (let [[v c] (a/alts!! [receiver sender1 sender2])]
      (println (str (current-thread-name) " channel: " (hash c) " done with " v)))))

(println ";;;;;;;;")
(println ";; go ;;")
(println ";;;;;;;;")
(let [c (a/chan 2)
      receiver (a/go
                 (dotimes [_ 10]
                   (println (str (current-thread-name) " received: " (<! c)))))
      sender1  (a/go
                 (dotimes [i 5]
                   (println (str (current-thread-name) " send: " i))
                   (>! c i)))
      sender2 (a/go
                (doseq [i (range 5 10)]
                  (println (str (current-thread-name) " send: " i))
                  (>! c i)))]
  ; alts: select
  (dotimes [_ 3]
    (let [[v c] (a/alts!! [receiver sender1 sender2])]
      (println
       (str (current-thread-name) " channel: " (hash c) " done with " v)))))

