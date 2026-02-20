(comment
  (let [xf (comp (filter #(.startsWith % "b"))
                 (map clojure.string/upper-case))]
    (-> (transduce xf conj ["foo" "bar"])
        (= ["BAR"])
        assert))

  ; transducer:
  ; a function of reduce-fn -> reduce-fn (aka. reduce-fn transformer)
  (def transducer (fn [reduce-fn]
                    (fn
                      ([] (reduce-fn))
                      ([acc] (reduce-fn acc))
                      ([acc e]
                      ; transform logic ...
                       (reduce-fn acc e)))))

  (defn filter-transducer [filter-fn]
    (fn [reduce-fn]
      (fn
        ([] (reduce-fn))
        ([acc] (reduce-fn acc))
        ([acc e]
         (if (filter-fn e)
           (reduce-fn acc e)
           acc)))))

  (defn map-transducer [map-fn]
    (fn [reduce-fn]
      (fn
        ([] (reduce-fn))
        ([acc] (reduce-fn acc))
        ([acc e]
         (reduce-fn acc (map-fn e))))))

  (let [xf (comp (filter-transducer #(.startsWith % "b"))
                 (map-transducer clojure.string/upper-case))]
    (-> (transduce xf conj ["foo" "bar"])
        (= ["BAR"])
        assert)))

; sequence VS eduction

(let [xf (comp
          (filter odd?)
          (map #(doto % println))
          (map #(* % %)))

      s (->> (range 10)
             (sequence xf))

      e (->> (range 10)
             (eduction xf))]

  (println "=== sequence would cache ===")
  (into [] s)
  (into [] s)

  (println "=== eduction would not cache ===")
  (into [] e)
  (into [] e))
