#!/usr/bin/env nbb

(ns nbb-demo
  (:require [promesa.core :as p]))

(defn json-stringify-data []
  (->
   #js {:name "nbb" :time (js/Date.)}
   (js/JSON.stringify)))

(defn fetch-data []
  (p/-> (js/fetch "https://httpbin.org/post"
                  #js {:method "POST"
                       :headers #js {:content-type "application/json"}
                       :body (json-stringify-data)})
        (.json)))

(p/-> (fetch-data)
      js/console.log)
