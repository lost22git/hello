(defmacro swap
  "Swap values of index `m` and index `n` in array `a`."
  [a m n]
  `(let [t# (aget ~a ~m)]
     (aset ~a ~m (aget ~a ~n))
     (aset ~a ~n t#)))

(defn- qsort-partition
  "Partition for Qsort"
  ^long [a ^long l ^long r]
  (let [pivot (aget a r)]
    (loop [p l i l]
      (if (= i r)
        (do (swap a p r) p)
        (if (< (aget a i) pivot)
          (do (swap a p i)
              (recur (inc p) (inc i)))
          (recur p (inc i)))))))

(defn qsort
  "Qsort"
  ([a] (qsort a 0 (dec (alength a))))
  ([a ^long l ^long r]
   (when (< l r)
     (let [p (qsort-partition a l r)]
       (qsort a l (dec p))
       (qsort a (inc p) r)))))

(comment
  (let [a (->>
           (iterate (fn [_] (rand-int 100)) 0)
           (take 10)
           int-array)]
    (printf "a = [%s]\n"
            (clojure.string/join ", " a))
    (qsort a)
    (printf "a = [%s]\n"
            (clojure.string/join ", " a))))
