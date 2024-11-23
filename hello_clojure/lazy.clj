#!/usr/bin/env clj -M

; delay: lazy value
(def a (delay (println "[delay] only execute me at the first time") (random-uuid)))
; (assert (= (force a) (force a)))
(assert (= @a @a))

; memoize function
; cache: args -> result
(defn area [w h] (let [r (format "[%s] (%d x %d = %d)" (random-uuid) w h (* w h))] (println r) r ))
(def marea (memoize area))
(assert (= (marea 3 4) (marea 3 4)))
(assert (not= (marea 3 4) (marea 4 3)))
(assert (= (marea 4 3) (marea 4 3)))


