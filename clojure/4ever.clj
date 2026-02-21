

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

(def my-second-last #(loop [[fst & nxt] % r nil]
                       (if nxt
                         (recur nxt fst)
                         r)))

(= (my-second-last nil) nil)
(= (my-second-last '(1)) nil)
(= (my-second-last '(1 2)) 1)
(= (my-second-last '(1 2 3)) 2)
(= (my-second-last '(1 2 3 4)) 3)

;; Nth Element

(def my-nth (fn [n [fst & nxt]]
              (if (< n 0)
                nil
                (if (= n 0)
                  fst
                  (if nxt
                    (recur (dec n) nxt)
                    nil)))))

(= (my-nth -1 '(1 2 3)) nil)
(= (my-nth 0 '(1 2 3)) 1)
(= (my-nth 1 '(1 2 3)) 2)
(= (my-nth 2 '(1 2 3)) 3)
(= (my-nth 3 '(1 2 3)) nil)
(= (my-nth 0 '()) nil)
(= (my-nth 0 nil) nil)

;; Count a Sequence

(def my-count (fn [col]
                (loop [[_ & nxt :as c] col n 0]
                  (if (empty? c)
                    n
                    (recur nxt (inc n))))))

(= (my-count nil) 0)
(= (my-count '()) 0)
(= (my-count '(1)) 1)
(= (my-count '(1 2)) 2)
(= (my-count '(nil)) 1)
(= (my-count '(nil nil)) 2)

;; Reverse a Sequence

(def my-reverse (fn [col]
                  (loop [[fst & nxt :as c] col r '()]
                    (if (empty? c)
                      r
                      (recur nxt (conj r fst))))))

(= (my-reverse nil) '())
(= (my-reverse '()) '())
(= (my-reverse '(1)) '(1))
(= (my-reverse '(1 2 3)) '(3 2 1))
(= (my-reverse '(1 2 3 nil)) '(nil 3 2 1))
(= (my-reverse '(1 2 3 nil '())) '('() nil 3 2 1))

;; Sum It All up

(def my-sum (fn [col]
              (loop [[fst & nxt :as c] col acc 0]
                (if (empty? c)
                  acc
                  (recur nxt (+ acc fst))))))

(= (my-sum nil) 0)
(= (my-sum []) 0)
(= (my-sum [1]) 1)
(= (my-sum [1 2 3]) 6)
(= (my-sum #{1 2 3}) 6)

;; Find the odd numbers

(def my-find-odd  (fn [col]
                    (loop [[fst & nxt :as c] col acc []]
                      (if (empty? c)
                        acc
                        (recur nxt (if (odd? fst)
                                     (conj acc fst)
                                     acc))))))

(= (my-find-odd nil) [])
(= (my-find-odd #{}) [])
(= (my-find-odd #{1}) [1])
(= (my-find-odd #{2}) [])
(= (my-find-odd #{1 2 4 5}) [1 5])
