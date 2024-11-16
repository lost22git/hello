;  ; def variables
; (def m 1)
; (def n 2)
; ; cal function
; (print (+ m n))

; ; let binding
; ; scope variables
; (let [m 1]
;   (let [n 2]
;     (print (+ m n))))

; ; call methods
; (println (.length "haha"))
; (println (.substring "haha" 0 3))
; (println (.getClass (.split "ha:ha" ":")))
; (println (.getClass '("haha")))
; (println (.getClass ["haha"]))

; def functions
; ver1
(defn myhash 
  [data] (.hashCode data))
; ver2
(def myhash (fn [data] (.hashCode data)))
; ver3
(def myhash #(.hashCode %))
(assert (== (.hashCode "haha") (myhash "haha")))

; singly-linked-list '()
(def mylist '("alex" "bob"))
(def mylist (conj mylist "cindy")) ; cnoj: prepend
(assert (== (count mylist) 3))
(assert (.equals (first mylist) "cindy"))
(assert (.contains (rest mylist) "alex")) 
(assert (.contains (rest mylist) "bob"))
(assert (.equals (last mylist) "bob"))

; set
(def myset #{"alex" "bob"})
(def myset (conj myset "cindy")) ; conj: add
(assert (== (count myset) 3))
(assert (contains? myset "cindy"))
(def myset (disj myset "cindy")) ; disj: del
(assert (== (count myset) 2))
(assert (not (contains? myset "cindy")))
(def myset (into myset '("cindy"))) ; into: A + B
(assert (== (count myset) 3))
(assert (contains? myset "cindy"))

; map
(def mymap {"alex" 22, "bob" 33}) ; ',' can be omitted
(def mymap (assoc mymap "cindy" 44)) ; assoc: add
(assert (== (count mymap) 3))
(assert (contains? mymap "cindy"))
(assert (== (get mymap "cindy") 44)) ; get
(assert (== (mymap "cindy") 44)) ; get
(assert (nil? (mymap "douglas"))) ; get not exists
(assert (== (mymap "douglas" 0) 0)) ; get not exists with default
(def mymap (zipmap (keys mymap) (vals mymap))) ; zipmap keys vals
(assert (== (count mymap) 3))
(assert (== (count mymap) 3))
(def mymap (dissoc mymap "cindy")) ; dissoc: del
(assert (== (count mymap) 2))
(assert (nil? (mymap "cindy")))
(def mymap (merge mymap {"cindy" 44 "douglas" 55})) ; merge
(assert (== (count mymap) 4))
(assert (== (mymap "cindy") 44))
(assert (== (mymap "douglas") 55))
(def mymap (merge-with + mymap {"douglas" 11})) ; merge-with
(assert (== (count mymap) 4))
(assert (== (mymap "douglas") (+ 55 11)))

; record
(defrecord Book [name pages])
(def book (->Book "The Clojure Book" 111))
(assert (.equals (:name book) "The Clojure Book"))
(assert (== (:pages book) 111))
