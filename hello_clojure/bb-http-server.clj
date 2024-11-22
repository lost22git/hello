#!/usr/bin/env bb

(comment
  "https://github.com/ring-clojure/ring/wiki")

(require '[cheshire.core :as json])
(require '[babashka.deps :as deps])
(deps/add-deps '{:deps {ring/ring-core {:mvn/version "1.13.0"}
                        ring/ring-jetty-adapter {:mvn/version "1.13.0"}}})

(use 'ring.middleware.params)
(use 'ring.adapter.jetty)

(defn halo [req]
  {:status 200
   :headers {:content-type "application/json"}
   :body (-> {:msg (str "Halo " (get-in req [:query-params :name]))}
             json/encode)})
(def app
  (-> halo
      wrap-params))
(run-jetty app {:port 8000})
