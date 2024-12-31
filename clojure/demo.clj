#!/usr/bin/env -S clj -M

; def variables
(def m 1)
(def n 2)
; cal function
(assert (= 3 (+ m n)))

; let binding
; scope variables
(let [m 1]
  (let [n 2]
    (assert (= 3 (+ m n)))))

; binding: rebind dynamic variables in scope
(def ^:dynamic lang "Java")
(assert ((meta #'lang) :dynamic))
(binding [lang "Clojure"]
  (assert (= lang "Clojure")))
(assert (= lang "Java"))

;
; def functions
; ver1
(defn myhash
  [data] (.hashCode data))
; ver2
(def myhash (fn [data] (.hashCode data)))
; ver3
(def myhash #(.hashCode %))
(assert (= (.hashCode "haha") (myhash "haha")))

;; singly-linked-list '()
(def mylist '("alex" "bob"))
(def mylist (conj mylist "cindy")) ; cnoj: prepend
(assert (= (count mylist) 3))
(assert (= (first mylist) "cindy"))
(assert (= (rest mylist) '("alex" "bob")))
(assert (= (last mylist) "bob"))
(assert (= ["fred" "douglas" "cindy" "alex" "bob"]
           (into  mylist ["douglas" "fred"])))   ; into: union

;; set
(def myset #{"alex" "bob"})
(def myset (conj myset "cindy")) ; conj: add
(assert (= (count myset) 3))
(assert (contains? myset "cindy"))
(def myset (disj myset "cindy")) ; disj: del
(assert (= (count myset) 2))
(assert (not (contains? myset "cindy")))
(def myset (into myset '("cindy"))) ; into: A + B
(assert (= (count myset) 3))
(assert (contains? myset "cindy"))

;; map
(def mymap {"alex" 22 "bob" 33})
(def mymap (assoc mymap "cindy" 44)) ; assoc: add
(assert (= (count mymap) 3))
(assert (contains? mymap "cindy"))
(assert (= (get mymap "cindy") 44)) ; get
(assert (= (mymap "cindy") 44)) ; get
(assert (nil? (mymap "douglas"))) ; get not exists
(assert (= (mymap "douglas" 0) 0)) ; get not exists with default
(def mymap (zipmap (keys mymap) (vals mymap))) ; zipmap keys vals
(assert (= (count mymap) 3))
(def mymap (dissoc mymap "cindy")) ; dissoc: del
(assert (= (count mymap) 2))
(assert (nil? (mymap "cindy")))
(def mymap (merge mymap {"cindy" 44 "douglas" 55})) ; merge
(assert (= (count mymap) 4))
(assert (= (mymap "cindy") 44))
(assert (= (mymap "douglas") 55))
(def mymap (merge-with + mymap {"douglas" 11})) ; merge-with
(assert (= (count mymap) 4))
(assert (= (mymap "douglas") (+ 55 11)))

;; record
(defrecord Book [name pages])
(def book (->Book "The Clojure Book" 111))
; (def book (map->Book {:name "The Clojure Book" :pages 111}))
(assert (= (:name book) "The Clojure Book"))
(assert (= (:pages book) 111))

;; let destructing 
(let [[a b] [1 2]]
  (assert (= (+ a b) 3)))

(let [{:keys [first last]} {:first "foo" :last "bar"}]
  (assert (= first "foo"))
  (assert (= last "bar")))

;; function params destructing
(def add
  (fn [{:keys [x y]}]
    (+ x y)))
(assert (= (add {:x 1 :y 2 :z 0}) 3))
(defrecord Point [x y z])
(assert (= (add (->Point 1 2 0)) 3))

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

;; complement
(assert (= [2 4] (filter even? [1 2 3 4 5])))
(assert (= [1 3 5] (filter (complement even?) [1 2 3 4 5])))

(assert (= 1 ((constantly 1) 1)))
(assert (= 1 ((constantly 1) 1 2)))
(assert (= 1 ((constantly 1) 1 2 "sdsf")))

;; fnil default param
(defn foo [x] (str "x=" x))
(assert (= "x=bar" ((fnil foo "bar") nil)))
(assert (= "x=foo" ((fnil foo "bar") "foo")))

;; for 
(->
 (for [{:keys [fid name]} [{:fid 1 :name "foo"} {:fid 2 :name "bar"}]]
   [fid name])
 (= [[1 "foo"] [2 "bar"]])
 (assert))

;; infinity
(assert (infinite? ##Inf))
(assert (infinite? ##-Inf))

;; Clojure History
;;
;; persistent data structures
;; STM (software transaction management)
;; concurrent primities
;; multi-methods
;; protocols 
;; core.async
;; transducer

