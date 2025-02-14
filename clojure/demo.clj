#!/usr/bin/env -S clj -M

;; infinity
(assert (infinite? ##Inf))
(assert (infinite? ##-Inf))

; def 
(def m 1)
(def n 2)
; cal function
(assert (= 3 (+ m n)))

; let binding
(let [m 1]
  (let [n 2]
    (assert (= 3 (+ m n)))))

; binding: rebind dynamic variables in scope
(def ^:dynamic lang "Java")
(assert ((meta #'lang) :dynamic))
(binding [lang "Clojure"]
  (assert (= lang "Clojure"))
  (set! lang  "Javascript") ;; set! only work in binding scope
  (assert (= lang "Javascript")))
(assert (= lang "Java"))

;; list
;; singly-linked-list '()
(def mylist '("alex" "bob"))
(def mylist (conj mylist "cindy")) ; cnoj: prepend
(assert (= (count mylist) 3))
(assert (= (first mylist) "cindy"))
(assert (= (rest mylist) '("alex" "bob")))
(assert (= (last mylist) "bob"))
(assert (= ["fred" "douglas" "cindy" "alex" "bob"]
           (into  mylist ["douglas" "fred"])))   ; into: union

;; vector

;; map

;; set

;; seq

;; record
(defrecord Book [name pages])
(def book (->Book "The Clojure Book" 111))
; (def book (map->Book {:name "The Clojure Book" :pages 111}))
(assert (= (:name book) "The Clojure Book"))
(assert (= (:pages book) 111))

;; doto
(let [list (java.util.ArrayList.)]
  (doto list (.add 10) (.add 100) (.add 1000))
  (assert (= list [10 100 1000])))

;; pipe (aka. Threading Macros)
; -> (prev result as next first param)
(assert (= "HALO_CLOJURE"
           (-> "halo"
               (str "_")
               (str "clojure")
               clojure.string/upper-case)))
; ->> (prev result as next last param)
(assert (= "CLOJURE_HALO"
           (->> "halo"
                (str "_")
                (str "clojure")
                clojure.string/upper-case)))
 ; ->>
(assert (= "HaloClojure"
           (->> ["halo" "clojure"]
                (map clojure.string/capitalize)
                (reduce str ""))))
; as-> (prev result as next nth param)
(assert (= "_clojure_"
           (as-> "clojure" r
             (str r "_")
             (str "_" r))))
; some->
; some->>
; cond->
; cond->>

;; transduce = transform + reduce
(def xf (comp (filter #(.startsWith % "b"))
              (map clojure.string/upper-case)))
(-> (transduce xf conj ["foo" "bar"])
    (= ["BAR"])
    assert)

;; more seq ops
(assert (= (take 4 (repeat "ha"))
           (take 4 (repeatedly (fn [] "ha")))
           ["ha" "ha" "ha" "ha"]))

;; lazy-seq
(defn cons-lazy-seq
  ([] (cons-lazy-seq 0))
  ([n] (cons n (lazy-seq (cons-lazy-seq (+ n 2))))))

(assert (= (take 4 (cons-lazy-seq))
           [0 2 4 6]))

;; run!
(run! (comp println #(str "user-" %) clojure.string/upper-case)
      "JOse")

;; juxt
(->> ((juxt #(.toUpperCase %) #(.toLowerCase %)) "cLoJuRe")
     (= ["CLOJURE" "clojure"])
     assert)
(->> [1 2 3 4 5]
     ((juxt filter remove) even?)
     (= [[2 4] [1 3 5]])
     (assert))

;; for 
(->
 (for [{:keys [fid name]} [{:fid 1 :name "foo"} {:fid 2 :name "bar"} {:fid 3 :name "zoo"}]]
   [fid name])
 (= [[1 "foo"] [2 "bar"] [3 "zoo"]])
 (assert))
;; for :when
(->
 (for [{:keys [fid name]} [{:fid 1 :name "foo"} {:fid 2 :name "bar"} {:fid 3 :name "zoo"}]
       :when (odd? fid)]
   [fid name])
 (= [[1 "foo"] [3 "zoo"]])
 (assert))
;; for :while
(->
 (for [{:keys [fid name]} [{:fid 1 :name "foo"} {:fid 2 :name "bar"} {:fid 3 :name "zoo"}]
       :while (odd? fid)]
   [fid name])
 (= [[1 "foo"]])
 (assert))
;; for nested
(->
 (for [i (range 10)
       j (range 10)]
   [i j])
 count
 (= (* 10 10))
 assert)

;; when/when-not/when-some/when-let

;; if/if-not/if-some/if-let

;; case
(assert (= "t"
           (case (= 1 1)
             true "t"
             false "f")))

;; cond

;; condp

;; datafy VS str
(->
 #uuid "f0babccb-e33e-49a9-bc38-46a9c6267dcc"
 ((juxt clojure.core.protocols/datafy clojure.datafy/datafy str)))

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

(comment
  (use '[clojure.repl])
  ;; apropos
  (->> "symbol"
       (apropos)
       (filter #(clojure.string/starts-with? % "clojure")))
  ;; doc
  (doc with-open)
  ;; source
  (source with-open)
  ;; dir
  (->>
   (dir clojure.java.io)
   (with-out-str)
   (clojure.string/split-lines)
   (map symbol)))
