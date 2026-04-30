(ns fib
  (:gen-class))

(set! *unchecked-math* :warn-on-boxed)

(defn fib ^long [^long n]
  (if (< n 2)
    1
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn -main [& args]
  (let [n 40]
    (println (fib n))))
