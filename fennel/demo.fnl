; define immutable variable
(local imvar 1)
;(set imvar 2) ; compile error!
(assert (= imvar 1))

; define mutable variable
(var mvar 1)
(assert (= mvar 1))
(set mvar 2)
(assert (= mvar 2))

; tables not= forever
(assert (not= [1 2] [1 2]))
(assert (not= {:a 1 :b 2} {:a 1 :b 2}))

; get set table
(local books [{:title "the fennel book" :tags ["fennel"]}])

; books.1.title
(-> (. books 1 :title)
    (= "the fennel book")
    assert)

; books.1.tags.1
(-> (. books 1 :tags 1)
    (= "fennel")
    assert)

; book?.2?.title
(-> (?. books 2 :title)
    (= nil)
    assert)

; define a function
(fn add [x y]
  (+ x y))

(assert (= 3 (add 1 2)))
(assert (= 3 (add (table.unpack [1 2]))))

; pattern matching to handle error (multi-return-values) 
(fn mydiv [x y]
  (if (= 0 y)
      (values nil "cannot div 0")
      (values (/ x y) nil)))

; case
(case (mydiv 1 0)
  (nil err-msg) (print "Got error: " err-msg)
  r (print "Got value:" r))

; match 
(match (mydiv 1 0)
  ((nil err-msg) ? (not= "" err-msg)) (print "Got error: " err-msg)
  r (print "Got value:" r))

; for 
(for [i 1 10 2]
  (print i))

; each
(each [i v (ipairs ["halo" "fennel"])]
  (print i v))

(each [k v (pairs {:foo "halo" :bar "fennel"})]
  (print k v))

; iterate and return array
(icollect [i v (ipairs [:a "foo" :b "bar"])]
  (when (= v "foo")
    (.. i "#" v)))

; iterate and return table
(collect [k v (pairs {:a 1 :b 2})]
  (when (= k :a)
    (values k v)))

; range iterate and return array
(fcollect [i 0 10 2]
  (when (> i 4)
    i))

; acc
(accumulate [sum 0 _ v (ipairs (fcollect [i 0 10 2] i))]
  (+ sum v))

; range acc
(faccumulate [sum 0 i 0 10 2]
  (+ sum i))

; doto
(let [t {}]
  (doto t (tset :a "halo")
    (tset :b "fennel"))
  (assert (= "halo" (. t :a)))
  (assert (= "fennel" (. t :b))))

; pipe (aka. Threading macros)
; ->
(assert (= "halofennel" (-> "halo" (.. "fennel"))))
; ->>
(assert (= "fennelhalo" (->> "halo" (.. "fennel"))))
; -?>
(assert (= "HALO" (-?> {:a "halo"} (. :a) (string.upper))))
(assert (= nil (-?> {:a "halo"} (. :b) (string.upper))))
; -?>> 

(fn os.run [cmd ?strip]
  "Run cmd and return cmd's execution result. 
  Return empty string if failed.
  Strip result's leading and trailing spaces if strip is true."
  (let [s (with-open [f (io.popen cmd "r")] (f:read "*a"))]
    (case (or ?strip false)
      false s
      true (-> s
               (string.gsub "^%s+" "")
               (string.gsub "%s+$" "")))))

(os.run "fennel --version" true)
