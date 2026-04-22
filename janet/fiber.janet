#!/usr/bin/env janet

(comment
  (let [f (fiber/current)]
    (print "=== fiber current ===")
    (pp (fiber/status f))
    (pp (fiber/getenv f))
    (pp (fiber/maxstack f))
    (pp (fiber/last-value f)))

  (let [f (fiber/root)]
    (print "=== fiber root ===")
    (pp (fiber/status f))
    (pp (fiber/getenv f))
    (pp (fiber/maxstack f))
    (pp (fiber/last-value f)))

  (let [ch (ev/chan)
        supervisor (ev/chan)
        n 4
        giver-fib (ev/spawn (do (repeat n
                                  (ev/sleep 1)
                                  (ev/give ch (os/time)))
                              (ev/chan-close ch)))
        taker-fib (ev/go |(forever
                            (if-let [v (ev/take $)]
                              (print v) (break))) ch supervisor)]

    (pp (ev/take supervisor))
    (ev/chan-close supervisor)
    (print "taker-fib" taker-fib ": " (fiber/status taker-fib))
    (print "giver-fib" giver-fib ": " (fiber/status giver-fib)))

  # timeout
  (try
    (ev/with-deadline 3
      (do
        (ev/sleep 1)
        (print "first sleep done")
        (ev/sleep 1)
        (print "second sleep done")
        (ev/sleep 1)
        (print "third sleep done")))
    ([e f]
      (print "last-value: " (fiber/last-value f))
      (debug/stacktrace f e "")
      nil))

  # end comment
  )
