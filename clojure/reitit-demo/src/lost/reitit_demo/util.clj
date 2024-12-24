(ns lost.reitit-demo.util)

(defonce id (atom 0))
(defn gen-id
  "generate id"
  ^long []
  (swap! id inc))
