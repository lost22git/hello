;; Simple Nath

(= (- 10 (* 2 3)) 4)

;; Strings

(= "HELLO WORLD" (.toUpperCase "hello world"))

;; Lists

(= (list :a :b :c) '(:a :b :c))

;; conj on lists

(= '(1 2 3 4) (conj '(2 3 4) 1))

(= '(1 2 3 4) (conj '(3 4) 2 1))

;; Vectors

(= [:a :b :c] (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c))

;; conj on vectors

(= [1 2 3 4] (conj [1 2 3] 4))

(= [1 2 3 4] (conj [1 2] 3 4))

;; Sets

(= #{:a :b :c :d} (set '(:a :a :b :c :c :c :c :d :d)))

(= #{:a :b :c :d} (clojure.set/union #{:a :b :c} #{:b :c :d}))

;; conj on sets

(= #{1 2 3 4} (conj #{1 4 3} 2))

;; Maps

(= 20 ((hash-map :a 10, :b 20, :c 30) :b))

(= 20 (:b {:a 10, :b 20, :c 30}))

;; conj on maps

(= {:a 1, :b 2, :c 3} (conj {:a 1} [:b 2] [:c 3]))

;; Sequences

(= 3 (first '(3 2 1)))

(= 3 (second [2 3 4]))

(= 3 (last (list 1 2 3)))

;; rest

(= '(20 30 40) (rest [10 20 30 40]))

;; Functions

(= 8 ((fn add-five [x] (+ x 5)) 3))

(= 8 ((fn [x] (+ x 5)) 3))

(= 8 (#(+ % 5) 3))

(= 8 ((partial + 5) 3))

;; Double Down

(= ((partial * 2) 3) 6)

;; Hello World

(= (#(str "Hello, " % "!") "Dave") "Hello, Dave!")

;; map

(= (lazy-seq '(6 7 8)) (map #(+ % 5) '(1 2 3)))

;; filter

(= (lazy-seq '(6 7)) (filter #(> % 5) '(3 4 5 6 7)))

;; Last Element

(def my-last #(if-let [n (next %)] (recur n) (first %)))

(= (my-last '()) nil)

(= (my-last '(1 2 3)) 3)

;; Penultimate Element;

(def my-second-last #(loop [[h & t] % r nil]
                       (if t
                         (recur t h)
                         r)))

(= (my-second-last nil) nil)
(= (my-second-last '(1)) nil)
(= (my-second-last '(1 2)) 1)
(= (my-second-last '(1 2 3)) 2)
(= (my-second-last '(1 2 3 4)) 3)

;; Nth Element

(def my-nth (fn [n [h & t]]
              (if (< n 0)
                nil
                (if (= n 0)
                  h
                  (if t
                    (recur (dec n) t)
                    nil)))))

(= (my-nth -1 '(1 2 3)) nil)
(= (my-nth 0 '(1 2 3)) 1)
(= (my-nth 1 '(1 2 3)) 2)
(= (my-nth 2 '(1 2 3)) 3)
(= (my-nth 3 '(1 2 3)) nil)
(= (my-nth 0 '()) nil)
(= (my-nth 0 nil) nil)

;; Count a Sequence

(def my-count (fn [coll]
                (loop [[_ & t :as c] coll n 0]
                  (if (empty? c)
                    n
                    (recur t (inc n))))))

(= (my-count nil) 0)
(= (my-count '()) 0)
(= (my-count '(1)) 1)
(= (my-count '(1 2)) 2)
(= (my-count '(nil)) 1)
(= (my-count '(nil nil)) 2)

;; Reverse a Sequence

(def my-reverse (fn [coll]
                  (loop [[h & t :as c] coll r '()]
                    (if (empty? c)
                      r
                      (recur t (conj r h))))))

(= (my-reverse nil) '())
(= (my-reverse '()) '())
(= (my-reverse '(1)) '(1))
(= (my-reverse '(1 2 3)) '(3 2 1))
(= (my-reverse '(1 2 3 nil)) '(nil 3 2 1))
(= (my-reverse '(1 2 3 nil '())) '('() nil 3 2 1))

;; Sum It All up

(def my-sum (fn [coll]
              (loop [[h & t :as c] coll acc 0]
                (if (empty? c)
                  acc
                  (recur t (+ acc h))))))

(= (my-sum nil) 0)
(= (my-sum []) 0)
(= (my-sum [1]) 1)
(= (my-sum [1 2 3]) 6)
(= (my-sum #{1 2 3}) 6)

;; Find the odd numbers

(def my-find-odd  (fn [coll]
                    (loop [[h & t :as c] coll acc []]
                      (if (empty? c)
                        acc
                        (recur t (if (odd? h)
                                   (conj acc h)
                                   acc))))))

(= (my-find-odd nil) [])
(= (my-find-odd #{}) [])
(= (my-find-odd #{1}) [1])
(= (my-find-odd #{2}) [])
(= (my-find-odd #{1 2 4 5}) [1 5])

;; Fib sequence

(def my-fib-seq (fn [n]
                  (let [xf (comp
                            (map first)
                            (take n))]
                    (sequence xf (iterate (fn [[a b]] [(+ a b) a]) '(1 0))))))

(= '() (my-fib-seq 0))
(= '(1) (my-fib-seq 1))
(= '(1 1) (my-fib-seq 2))
(= '(1 1 2) (my-fib-seq 3))
(= '(1 1 2 3 5 8 13 21 34 55 89) (my-fib-seq 11))
(= '() (my-fib-seq -11))

(def my-fib-seq2 #(take % ((fn aux [a b]
                             (lazy-seq
                              (cons a (aux b (+ a b))))) 1 1)))

(= '() (my-fib-seq2 0))
(= '(1) (my-fib-seq2 1))
(= '(1 1) (my-fib-seq2 2))
(= '(1 1 2) (my-fib-seq2 3))
(= '(1 1 2 3 5 8 13 21 34 55 89) (my-fib-seq2 11))
(= '() (my-fib-seq2 -11))

;; Palindrome Detector

(def my-palindrome-detector (fn [coll]
                              (= (sequence coll) (reverse coll))))

(= true (my-palindrome-detector nil))
(= true (my-palindrome-detector '()))
(= true (my-palindrome-detector '(1)))
(= true (my-palindrome-detector '(1 2 2 1)))
(= true (my-palindrome-detector '(1 2 3 2 1)))
(= true (my-palindrome-detector "carrac"))
(= false (my-palindrome-detector '(1 2)))

(def my-palindrome-detector2 (fn [coll]
                               (if (or (empty? coll) (nil? (next coll)))
                                 true
                                 (and (= (first coll) (last coll))
                                      (recur (butlast (rest coll)))))))

(= true (my-palindrome-detector2 nil))
(= true (my-palindrome-detector2 '()))
(= true (my-palindrome-detector2 '(1)))
(= true (my-palindrome-detector2 '(1 2 2 1)))
(= true (my-palindrome-detector2 '(1 2 3 2 1)))
(= true (my-palindrome-detector2 "carrac"))
(= false (my-palindrome-detector2 '(1 2)))

;; Flattern a Sequence

(def my-flatten (fn [coll]
                  (if-not (sequential? coll)
                    '()
                    ((fn aux [coll]
                       (if (sequential? coll)
                         (mapcat aux coll)
                         (list coll))) coll))))

(= '() (my-flatten nil))
(= '() (my-flatten '()))
(= '() (my-flatten '(((())))))
(= '(1 2 3 4) (my-flatten '((1) 2 ((3) (((4)))))))

;; Compress a Sequence

(def my-compress (fn [coll]
                   (loop [[h & t :as s] (seq coll)
                          r '()]
                     (if (empty? s)
                       (reverse r)
                       (recur t (cond
                                  (empty? r) (conj r h)
                                  (not= (first r) h)  (conj r h)
                                  :else r))))))

(= '() (my-compress nil))
(= '() (my-compress '()))
(= '(1) (my-compress '(1)))
(= '(1 2) (my-compress '(1 2)))
(= '(1 2 nil 4 1) (my-compress '(1 2 2 2 2 2 nil nil 4 1)))
(= '(nil 1 2 nil) (my-compress '(nil nil 1 2 nil nil)))

(def my-compress2 (fn [coll]
                    (lazy-seq
                     (when-let [s (seq coll)]
                       (let [[h & t] s]
                         (if (and t (= h (first t)))
                           (my-compress2 t)
                           (cons h (my-compress2 t))))))))

(= '() (my-compress2 nil))
(= '() (my-compress2 '()))
(= '(1) (my-compress2 '(1)))
(= '(1 2) (my-compress2 '(1 2)))
(= '(1 2 nil 4 1) (my-compress2 '(1 2 2 2 2 2 nil nil 4 1)))
(= '(nil 1 2 nil) (my-compress2 '(nil nil 1 2 nil nil)))

;; Pack a Sequence

(def my-pack (fn [coll]
               ((fn aux [c pack]
                  (lazy-seq
                   (if-let [s (seq c)]
                     (let [[h & t] s]
                       (cond
                         (empty? pack) (aux t (list h))
                         (= (first pack) h) (aux t (conj pack h))
                         :else (cons pack (aux t (list h)))))
                     (when-not (empty? pack) (cons pack nil)))))
                coll '())))

(= '() (my-pack nil))
(= '() (my-pack '()))
(= '((nil nil) ([1 2] [1 2]) (3 3) (4) (nil)) (my-pack '(nil nil [1 2] [1 2] 3 3 4 nil)))

;; Duplicate a Sequence

(def my-dup (fn [coll]
              (if-let [s (seq coll)]
                (mapcat (partial repeat 2) s)
                '())))

(= '() (my-dup nil))
(= '() (my-dup '()))
(= '(nil nil) (my-dup '(nil)))
(= '(1 1) (my-dup '(1)))
(= '(1 1 2 2) (my-dup '(1 2)))
(= '([1 2] [1 2]) (my-dup '([1 2])))

;; Replicate a Sequence

(def my-replicate (fn [coll n]
                    (if-let [s (seq coll)]
                      (mapcat (partial repeat n) s)
                      '())))

(= (my-replicate [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4]))

;; Implement Range

(def my-range (fn [l r]
                (lazy-seq
                 (when (< l r)
                   (cons l (my-range (inc l) r))))))

(= '(1) (my-range 1 2))
(= '(1 2 3) (my-range 1 4))
(= '() (my-range 1 -4))

;; Regular Expressions

(= "ABC" (apply str (re-seq #"[A-Z]+" "bA1B3Ce ")))

;; Maximum Value

(def my-max (fn aux [h & t]
              (cond
                (empty? t) h
                (<= h (first t)) (apply aux t)
                :esle (apply aux h (next t)))))

(= (my-max 1 8 3 4) 8)
(= (my-max 30 20) 30)
(= (my-max 45 67 11) 67)

;; Interleave two Seqs

(def my-interleave (partial mapcat list))

(= (my-interleave [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
(= (my-interleave [1 2] [3 4 5 6]) '(1 3 2 4))
(= (my-interleave [1 2 3 4] [5]) [1 5])
(= (my-interleave [30 20] [25 15]) [30 25 20 15])

(def my-interleave2 (fn aux [a b]
                      (lazy-seq
                       (if (and a b)
                         (cons (first a)
                               (cons (first b)
                                     (aux (next a) (next b))))))))

(= (my-interleave2 [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
(= (my-interleave2 [1 2] [3 4 5 6]) '(1 3 2 4))
(= (my-interleave2 [1 2 3 4] [5]) [1 5])
(= (my-interleave2 [30 20] [25 15]) [30 25 20 15])

;; Interpose a Seq

(def my-interpose #(drop-last 1 (mapcat list %2 (repeat %1))))

(= (my-interpose 0 [1 2 3]) [1 0 2 0 3])
(= (apply str (my-interpose ", " ["one" "two" "three"])) "one, two, three")
(= (my-interpose :z [:a :b :c :d]) [:a :z :b :z :c :z :d])

;; Drop Every Nth Item

(def my-drop-every-nth (fn [coll n]
                         ((fn aux [[h & t :as s] i]
                            (lazy-seq
                             (when s
                               (if (= 1 i)
                                 (aux t n)
                                 (cons h (aux t (dec i)))))))
                          (seq coll) n)))

(= (my-drop-every-nth [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
(= (my-drop-every-nth [:a :b :c :d :e :f] 2) [:a :c :e])
(= (my-drop-every-nth [1 2 3 4 5 6] 4) [1 2 3 5 6])

;; Factorial Fun

(def my-factorial #(reduce * (range 1 (inc %))))

(= (my-factorial 1) 1)
(= (my-factorial 3) 6)
(= (my-factorial 5) 120)
(= (my-factorial 8) 40320)

;; Reverse Interleave

(def my-reverse-interleave (fn [coll n]
                             (let [r (make-array java.lang.Object n)]
                               (loop [[h & t :as s] (seq coll)
                                      i 0]
                                 (if-not s
                                   (map reverse r)
                                   (let [idx (mod i n)]
                                     (aset r idx (conj (aget r idx) h))
                                     (recur t (inc idx))))))))

(= (my-reverse-interleave [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
(= (my-reverse-interleave (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
(= (my-reverse-interleave (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))

(def my-reverse-interleave2 (fn [coll n]
                              (loop [r '()
                                     i n
                                     [_ & t :as s] (seq coll)]
                                (cond
                                  (empty? s) (reverse r)
                                  (zero? i) (reverse r)
                                  :else (recur (conj r (take-nth n s))
                                               (dec i)
                                               t)))))

(= (my-reverse-interleave2 [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
(= (my-reverse-interleave2 (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
(= (my-reverse-interleave2 (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))

;; Rotate Sequence

(def my-rotate #(let [c (count %2)
                      dc (mod %1 c)]
                  (->>
                   (cycle %2)
                   (drop dc)
                   (take c))))

(= (my-rotate 2 [1 2 3 4 5]) '(3 4 5 1 2))
(= (my-rotate -2 [1 2 3 4 5]) '(4 5 1 2 3))
(= (my-rotate 6 [1 2 3 4 5]) '(2 3 4 5 1))
(= (my-rotate 1 '(:a :b :c)) '(:b :c :a))
(= (my-rotate -4 '(:a :b :c)) '(:c :a :b))

;; Flipping out

(def my-flipping-out (fn [f]
                       (fn [& xs]
                         (apply f (reverse xs)))))

(= 3 ((my-flipping-out nth) 2 [1 2 3 4 5]))
(= true ((my-flipping-out >) 7 8))
(= 4 ((my-flipping-out quot) 2 8))
(= [1 2 3] ((my-flipping-out take) [1 2 3 4 5] 3))

;; Split a Sequence

(def my-split #(list (take %1 %2) (drop %1 %2)))

(= (my-split 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
(= (my-split 1 [:a :b :c :d]) [[:a] [:b :c :d]])
(= (my-split 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])

;; Split by Type

(def my-split-by-type (fn [coll]
                        (loop [[h & t :as s] (seq coll) r {}]
                          (if-not s
                            (map reverse (vals r))
                            (recur t
                                   (update r (type h) conj h))))))

(= (set (my-split-by-type [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
(= (set (my-split-by-type [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})
(= (set (my-split-by-type [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})

;; Longest Increasing Sub-Seq

(def my-longest-increasing-sub-seq
  (fn [coll]
    (loop [[h & t :as s] (seq coll)
           [ph & pt :as p] nil
           r nil]
      (if-not s
        (let [mr (if (> (count p) (count r)) p r)]
          (if (> (count mr) 1) (reverse mr) '()))
        (cond
          (nil? h) (recur t p r)
          (or (nil? ph) (> h ph)) (recur t (conj p h) r)
          :else
          (if (> (count p) (count r))
            (recur t (list h) p)
            (recur t (list h) r)))))))

(= (my-longest-increasing-sub-seq [1 0 1 2 3 0 4 5]) [0 1 2 3])
(= (my-longest-increasing-sub-seq [5 6 1 3 2 7]) [5 6])
(= (my-longest-increasing-sub-seq [2 3 3 4 5]) [3 4 5])
(= (my-longest-increasing-sub-seq [7 6 5 4]) [])

;; Partition a Sequence

(def my-partition (fn [n coll]
                    (lazy-seq
                     (when-let [s (seq coll)]
                       (let [ss (take n s)]
                         (when (= (count ss) n)
                           (cons ss (my-partition n (drop n s)))))))))

(= (my-partition 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))
(= (my-partition 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))
(= (my-partition 3 (range 8)) '((0 1 2) (3 4 5)))

;; Count Occurences

(def my-count-occurences #(reduce
                           (fn [acc el]
                             (update acc el (fnil inc 0)))
                           {} %))

(= (my-count-occurences [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
(= (my-count-occurences [:b :a :b :a :b]) {:a 2, :b 3})
(= (my-count-occurences '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})

;; Find Distinct Items

(def my-distinct (fn [coll]
                   ((fn aux [ex [h & t :as s]]
                      (lazy-seq
                       (when s
                         (if (contains? ex h)
                           (aux ex t)
                           (cons h (aux (conj ex h) t))))))
                    #{} (seq coll))))

(= (my-distinct [1 2 1 3 1 2 4]) [1 2 3 4])
(= (my-distinct [:a :a :b :b :c :c]) [:a :b :c])
(= (my-distinct '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3]))
(= (my-distinct (range 50)) (range 50))

;; Function Composition

(def my-comp (fn [& fs]
               (reduce
                (fn [acc el]
                  (fn [& xs]
                    (acc (apply el xs))))
                fs)))

(defn my-comp2
  ([] identity)
  ([f] f)
  ([f g] (fn [& xs] (f (apply g xs))))
  ([f g & fs]
   (apply my-comp2 (my-comp2 f g) fs)))

(= [3 2 1] ((my-comp rest reverse) [1 2 3 4]))
(= 5 ((my-comp (partial + 3) second) [1 2 3 4]))
(= true ((my-comp zero? #(mod % 8) +) 3 5 7 9))
(= "HELLO" ((my-comp #(.toUpperCase %) #(apply str %) take) 5 "hello world"))

(= [3 2 1] ((my-comp2 rest reverse) [1 2 3 4]))
(= 5 ((my-comp2 (partial + 3) second) [1 2 3 4]))
(= true ((my-comp2 zero? #(mod % 8) +) 3 5 7 9))
(= "HELLO" ((my-comp2 #(.toUpperCase %) #(apply str %) take) 5 "hello world"))

;; Juxtaposition

(def my-juxt (fn [& fs]
               (fn [& xs]
                 (map #(apply % xs) fs))))

(= [21 6 1] ((my-juxt + max min) 2 3 5 1 6 4))
(= ["HELLO" 5] ((my-juxt #(.toUpperCase %) count) "hello"))
(= [2 6 4] ((my-juxt :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))

;; Sequence Reductions

(defn my-seq-reductions
  ([f coll] (my-seq-reductions f (first coll) (next coll)))
  ([f acc coll]
   (lazy-seq
    (if-some [[h & t] (seq coll)]
      (cons acc (my-seq-reductions f (f acc h) t))
      (cons acc nil)))))

(= (take 5 (my-seq-reductions + (range))) [0 1 3 6 10])
(= (my-seq-reductions conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])
(= (last (my-seq-reductions * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)

;; Re-implement iteration

(def my-iterate (fn [f x]
                  (lazy-seq
                   (cons x (my-iterate f (f x))))))

(= (take 5 (my-iterate #(* 2 %) 1)) [1 2 4 8 16])
(= (take 100 (my-iterate inc 0)) (take 100 (range)))
(= (take 9 (my-iterate #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))

;; Black Box Testing

(def my-black-box (fn [x]
                    (let [xx (conj (empty x) [:a true] [:a true] [:b false])]
                      (if (= 2 (count xx))
                        (if (:a xx) :map :set)
                        (if (second (first xx)) :vector :list)))))

(= :map (my-black-box {:a 1, :b 2}))
(= :list (my-black-box (range (rand-int 20))))
(= :vector (my-black-box [1 2 3 4 5 6]))
(= :set (my-black-box #{10 (rand-int 5)}))
(= [:map :set :vector :list] (map my-black-box [{} #{} [] ()]))

;; gcd

(def my-gcd (fn [x y]
              (cond
                (zero? x) y
                (zero? y) x
                (< x y) (recur x (mod y x))
                :else (recur y (mod x y)))))

(= (my-gcd 2 4) 2)
(= (my-gcd 10 5) 5)
(= (my-gcd 5 7) 1)
(= (my-gcd 1023 858) 33)

;; Prime Numbers

(def my-prime (fn [n]
                (->>
                 (range)
                 (filter (fn [x]
                           (case x
                             0 false
                             1 false
                             2 true
                             (->>
                              (range 2 (inc (clojure.math/sqrt x)))
                              (every? #(not= 0 (mod x %)))))))
                 (take n))))

(= (my-prime 2) [2 3])
(= (my-prime 5) [2 3 5 7 11])
(= (last (my-prime 100)) 541)

;; Merge with a Function

(def my-merge (fn [f x & xs]
                (if-some [[h & t] (seq xs)]
                  (let [n (reduce (fn [acc [k v]]
                                    (assoc acc k
                                           (if (contains? acc k)
                                             (f (acc k) v)
                                             v)))
                                  x h)]
                    (recur f n t))
                  x)))

(= (my-merge * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
   {:a 4, :b 6, :c 20})
(= (my-merge - {1 10, 2 20} {1 3, 2 10, 3 15})
   {1 7, 2 10, 3 15})
(= (my-merge concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
   {:a [3 4 5], :b [6 7], :c [8 9]})

;; Word Sorting

(def my-word-sorting (fn [x]
                       (->>
                        (clojure.string/split x #"\W+")
                        (sort-by clojure.string/lower-case))))

(= (my-word-sorting  "Have a nice day.")
   ["a" "day" "Have" "nice"])
(= (my-word-sorting  "Clojure is a fun language!")
   ["a" "Clojure" "fun" "is" "language"])
(= (my-word-sorting  "Fools fall for foolish follies.")
   ["fall" "follies" "foolish" "Fools" "for"])

;; Analyze a Tic-Tac-Toe Board

(def my-tic-tac-toe (fn [coll]

                      (let [f (fn [coll]
                                (some->> coll
                                         (filter #(and (not= :e (first %))
                                                       (every? (partial = (first %)) %)))
                                         first
                                         first))]

                        (or
                        ;; row
                         (f coll)
                        ;; column
                         (f (apply (partial map list) coll))
                         ;; cross
                         (let [i (volatile! -1)]
                           (or
                            (f (list (map #(nth % (vswap! i inc)) coll)))
                            (do
                              (vswap! i inc)
                              (f (list (map #(nth % (vswap! i dec)) coll))))))))))

(= nil (my-tic-tac-toe [[:e :e :e]
                        [:e :e :e]
                        [:e :e :e]]))

(= :x (my-tic-tac-toe [[:x :e :o]
                       [:x :e :e]
                       [:x :e :o]]))

(= :o (my-tic-tac-toe [[:e :x :e]
                       [:o :o :o]
                       [:x :e :x]]))

(= nil (my-tic-tac-toe [[:x :e :o]
                        [:x :x :e]
                        [:o :x :o]]))

(= :x (my-tic-tac-toe [[:x :e :e]
                       [:o :x :e]
                       [:o :e :x]]))

(= :o (my-tic-tac-toe [[:x :e :o]
                       [:x :o :e]
                       [:o :e :x]]))

(= nil (my-tic-tac-toe [[:x :o :x]
                        [:x :o :x]
                        [:o :x :o]]))

;; Euler's Totient Function

(def my-euler-totient (fn [n]
                        (if (= 1 n)
                          1
                          (count
                           ((fn aux [m]
                              (lazy-seq
                               (when (< m n)
                                 (let [gcd (loop [a m b n]
                                             (if (zero? a) b (recur (mod b a) a)))]
                                   (if (> gcd 1)
                                     (aux (inc m))
                                     (cons m (aux (inc m))))))))
                            1)))))

(= (my-euler-totient 1) 1)
(= (my-euler-totient 10) (count '(1 3 7 9)) 4)
(= (my-euler-totient 40) 16)
(= (my-euler-totient 99) 60)

;; Intro to Trampoline

(= [1 3 5 7 9 11]
   (letfn
    [(foo [x y] #(bar (conj x y) y))
     (bar [x y] (if (> (last x) 10)
                  x
                  #(foo x (+ 2 y))))]
     (trampoline foo [] 1)))

;; Anagram Finder

(def my-anagram-finder (fn [coll]
                         (->> coll
                              (group-by set)
                              vals
                              (filter #(< 1 (count %)))
                              (map set)
                              set)))

(= (my-anagram-finder ["meat" "mat" "team" "mate" "eat"])
   #{#{"meat" "team" "mate"}})
(= (my-anagram-finder ["veer" "lake" "item" "kale" "mite" "ever"])
   #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})

;; Re-implement Trampoline

(def my-trampoline (fn [f & args]
                     (let [r (apply f args)]
                       (if (fn? r) (recur r nil) r))))

(= (letfn [(triple [x] #(sub-two (* 3 x)))
           (sub-two [x] #(stop? (- x 2)))
           (stop? [x] (if (> x 50) x #(triple x)))]
     (my-trampoline triple 2))
   82)

(= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
           (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
     (map (partial my-trampoline my-even?) (range 6)))
   [true false true false true false])

;; Perfect Numbers

(def my-perfect-number (fn [x]
                         (->> (range 1 (inc (int (/ x 2))))
                              (filter #(zero? (mod x %)))
                              (apply +)
                              (= x))))

(= (my-perfect-number 6) true)
(= (my-perfect-number 7) false)
(= (my-perfect-number 496) true)
(= (my-perfect-number 500) false)
(= (my-perfect-number 8128) true)

;; Set intersection
(def my-set-intersection (fn [& xs]
                           (reduce #(set (filter %2 %)) xs)))

(= (my-set-intersection #{0 1 2 3} #{2 3 4 5}) #{2 3})
(= (my-set-intersection #{0 1 2} #{3 4 5}) #{})
(= (my-set-intersection #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})

;; Word Chains
;; TODO

;; A Half-Truth

(def my-a-half-truth (fn [x & xs]
                       (not-every? #(= x %) xs)))

(= false (my-a-half-truth false false))
(= true (my-a-half-truth true false))
(= false (my-a-half-truth true))
(= true (my-a-half-truth false true false))
(= false (my-a-half-truth true true true))
(= true (my-a-half-truth true true true false))

;; Transitive Closure

(def my-transitive-closure (fn [coll]))

