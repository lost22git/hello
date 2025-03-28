#!/usr/bin/env -S bb -Djdk.httpclient.HttpClient.log=request,headers,content -Djdk.httpclient.allowRestrictedHeaders=connection

(comment
  :ref
  {:babashka "https://book.babashka.org/#usage"
   :logging "https://github.com/clojure/tools.logging"
   :json "https://github.com/dakrone/cheshire"
   :ring "https://github.com/ring-clojure/ring/wiki"
   :http-kit "https://github.com/http-kit/http-kit/wiki"
   :http-client ["https://github.com/babashka/http-client"
                 "https://github.com/babashka/http-client/blob/main/API.md"]}
  :test {:halo "curl -X GET http://localhost:8000/halo?name=clojure"
         :proxy ["curl -X GET http://localhost:8000/proxy?target=https://httpbin.org/ip"
                 "curl -X POST http://localhost:8000/proxy?target=https://httpbin.org/post -d '{\"foo\":\"bar\"}' -H 'content-type application/json'"]})

(require '[babashka.deps :as deps])

; add deps
(deps/add-deps '{:deps {ring/ring-core {:mvn/version "1.13.0"}}})

(use 'clojure.pprint)
(require '[cheshire.core :as json])
(require '[babashka.http-client :as http])
(use 'clojure.tools.logging)
(use 'ring.middleware.params)
(use 'org.httpkit.server)

; halo handler
(defn handle-halo [req]
  {:status 200
   :headers {"content-type" "application/json"}
   :body (-> {:msg (str "Halo " (get-in req [:query-params "name"]))}
             json/encode)})

(defonce client
  (delay (info "initializing http client") (http/client {:follow-redirects :always})))

; proxy handler
(defn handle-proxy [req]
  (let [method (name (:request-method req))
        target-uri  (get-in req [:query-params "target"])]
    ; (pprint req)
    (infof "fetching remote: %s" {method target-uri})
    (http/request {:method method
                   :uri target-uri
                   :headers (dissoc (:headers req) "host" "content-length")
                   :body (:body req)
                   :throw false
                   :client @client})))

; routing (manual)
(defn router [req]
  (case [(:request-method req) (:uri req)]
    [:get "/halo"] (handle-halo req)
    [_ "/proxy"] (handle-proxy req)
    {:status 404 :body (str "NOT FOUND " (:uri req))}))

; wrapping middlewares
(def app
  (-> router
      wrap-params))

(run-server app {:port 8000})
(info "Server is listenning at :8000")
@(promise)
