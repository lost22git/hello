

(defn print-array
  [a w]
  (.write w "[")
  (let [len (alength a)]
    (dotimes [n len]
      (if *print-dup*
        (print-dup (aget a n) w)
        (print-method (aget a n) w))
      (when (< n (dec len))
        (.write w ", "))))
  (.write w "]"))

; implement print method for int/1 (int[])

(defmethod print-method int/1
  [a w]
  (.write w "I")
  (print-array a w))

(defmethod print-dup int/1
  [a w]
  (print-method a w))

(comment
  (println (int-array 9 3)))

(defmethod print-method java.lang.String/1
  [a w]
  (.write w "S")
  (print-array a w))

(defmethod print-dup java.lang.String/1
  [a w]
  (print-method a w))

(comment (doto
          (make-array String 3)
           (aset 0 "foo")
           (aset 1 "bar")
           (aset 2 "baz")
           println
           prn))



