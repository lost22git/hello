(ns lost.shadow-cljs-demo)

; #?(:node (println "Hello NodeJS, Love from Shadow-cljs")
;    :cljs
;    (js/alert "Hello Browser, Love from Shadow-cljs"))

(js/alert "Hello Browser, Love from Shadow-cljs")

(defn main [& args]
  (println "Hello NodeJS, Love from Shadow-cljs"))
