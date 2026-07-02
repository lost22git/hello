;; symbol / var / ns
(def a 1)
;; var from symbol
(assert (= #'user/a
           (var a)
           (find-var 'user/a)
           (resolve 'a)
           (ns-resolve 'user 'a)))
;; get val from var
(assert (= 1
           a
           @#'a
           (deref #'a)
           (var-get #'a)))

;; resolve symbol as var
(assert (= #'clojure.core/map
           (resolve 'map)
           (ns-resolve 'clojure.core 'map)))
(assert (= java.lang.Exception
           (resolve 'Exception)
           (ns-resolve *ns* 'Exception)))
(assert
 (fn? @#'map))
(assert
 (fn? map))
