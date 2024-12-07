(ns demo.core
  (:use clojure.tools.logging))

(defn add [^long a ^long b]
  (info "calling [add] with" a b)
  (+ a b))

(defn -main [& args]
  (->> args
       (map parse-long)
       (apply add)
       (println "result =>")))


