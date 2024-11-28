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

; singly-linked-list '()
(def mylist '("alex" "bob"))
(def mylist (conj mylist "cindy")) ; cnoj: prepend
(assert (= (count mylist) 3))
(assert (= (first mylist) "cindy"))
(assert (= (rest mylist) '("alex" "bob")))
(assert (= (last mylist) "bob"))
(assert (= ["fred" "douglas" "cindy" "alex" "bob"]
           (into  mylist ["douglas" "fred"])))   ; into: union

; set
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

; map
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

; record
(defrecord Book [name pages])
(def book (->Book "The Clojure Book" 111))
(assert (= (:name book) "The Clojure Book"))
(assert (= (:pages book) 111))

; let destructing 
(let [[a b] [1 2]]
  (assert (= (+ a b) 3)))

(let [{:keys [first last]} {:first "foo" :last "bar"}]
  (assert (= first "foo"))
  (assert (= last "bar")))

; function params destructing
(def add
  (fn [{:keys [x y]}]
    (+ x y)))
(assert (= (add {:x 1 :y 2 :z 0}) 3))
(defrecord Point [x y z])
(assert (= (add (->Point 1 2 0)) 3))

; doto
(let [list (java.util.ArrayList.)]
  (doto list (.add 10) (.add 100) (.add 1000))
  (assert (= list [10 100 1000])))

; pipe (aka. Threading Macros)
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

; transient

; slow
(time
 (loop [current 1000000 state []]
   (if (zero? current)
     state
     (recur (dec current) (conj state current)))))

; fast
(time
 (loop [current 1000000 state (transient [])]
   (if (zero? current)
     (persistent! state)
     (recur (dec current) (conj! state current)))))

; transduce = transform + reduce
(def xf (comp
         (filter #(.startsWith % "b"))
         (map clojure.string/upper-case)))
(-> (transduce xf conj ["foo" "bar"])
    (= ["BAR"])
    assert)

