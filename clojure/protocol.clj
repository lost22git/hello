#!/usr/bin/env -S clj -M

(defrecord Info [code title description])

(defprotocol InfoWriter
  (write [this info])
  (close [this]))

(defrecord SimpleInfoWriter [writer]
  InfoWriter
  (write [this info]
    (doto (:writer this)
      (.write (prn-str info))
      (.flush)))
  (close [this]
    (println "close is calling")
    (let [w (:writer this)]
      (when (not= *out* w)
        (.close w)))))

(with-open [writer (SimpleInfoWriter. *out*)]
  (let [info (map->Info {:code 111 :name "yalisa" :description "yalisa"})]
    (write writer info)))


