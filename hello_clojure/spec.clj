#!/usr/bin/env bb

(comment
  "https://clojure.org/guides/spec")

(require '[babashka.deps :as deps])
(deps/add-deps '{:deps {org.clojure/spec.alpha {:mvn/version "0.5.238"}}})
(require '[clojure.spec.alpha :as s])
(require '[clojure.pprint :as p])

; validate function arguments and return value
(s/def :int/pos (s/and int? pos?))
(defn my-inc
  ([x] (my-inc x 1))
  ([x y]
   {:pre [(s/valid? :int/pos x) (s/valid? :int/pos y)]
    :post [(s/valid? :int/pos %)]}
   (+ x y)))

; (my-inc -2)
; (my-inc 2 -2)
(my-inc 2)

; validate record
(defrecord Book [^UUID id ^String name])
(s/def :book/id uuid?)
(s/def :book/name (s/and string? #(re-matches #"\w{4,20}" %)))
(s/def :book/book (s/keys :req-un [:book/id :book/name]))

(->> (->Book 1 1)
     (s/valid? :book/book)
     not
     assert)

; (->> (->Book 1 1)
;      (s/explain :book/book))

(->> (->Book 1 1)
     (s/explain-data :book/book)
     :clojure.spec.alpha/problems
     (p/print-table [:path :pred :val :via :in]))

(->> (->Book (random-uuid) "hello_clojure")
     (s/valid? :book/book)
     assert)

; validate map
(->> {:id 1 :name 1}
     (s/valid? :book/book)
     not
     assert)

; (->> {:id 1 :name 1}
;      (s/explain :book/book))

(->> {:id 1 :name 1}
     (s/explain-data :book/book)
     :clojure.spec.alpha/problems
     (p/print-table [:path :pred :val :via :in]))

(->> {:id (random-uuid) :name "hello_clojure"}
     (s/valid? :book/book)
     assert)
