(ns lost.reitit-demo
  (:gen-class)
  (:require
   [lost.reitit-demo.book-api :as book-api]
   [org.httpkit.server :as hk]
   [reitit.ring :as ring]
   ;; logging
   [taoensso.telemere :as t]
   ;; dev
   [reitit.dev.pretty :as pretty]
   [reitit.ring.middleware.dev :refer [print-request-diffs]]
   [reitit.ring.spec :as rrs]
   ;; exception
   [reitit.ring.middleware.exception :as exception]
   ;; content negotiation
   [reitit.ring.middleware.muuntaja :as muuntaja]
   [muuntaja.core :as m]
   ;; coercion and spec
   [reitit.ring.coercion :as coercion]
   [reitit.coercion.spec :as coercion-spec]
   ;; openapi
   [reitit.openapi :as openapi]
   [reitit.swagger-ui :as swagger-ui]))

(defn hi [_req]
  {:status 200 :body "hi!"})

(defn bug [_req]
  (throw (ex-info "bug!" {:foo "bar"})))

; === logging ===

(defn start-logging []
  (t/set-min-level! :info))

; === error-handler ===

(defn default-error-handler [err req]
  (t/error! err)
  {:status 500
   :body {:err.msg (ex-message err)
          :err.data (ex-data err)
          :time (java.time.Instant/now)
          :req.method (:request-method req)
          :req.uri (:uri req)}})

(defn coercion-error-handler [err req]
  {:status 500
   :body {:err.msg "coercion error"
          :err.data (:body ((exception/create-coercion-handler 500) err req))
          :time (java.time.Instant/now)
          :req.method (:request-method req)
          :req.uri (:uri req)}})

(def error-middleware
  (exception/create-exception-middleware
   (merge
    exception/default-handlers
    {:reitit.coercion/request-coercion coercion-error-handler
     ::exception/default default-error-handler})))

;; === openapi and swagger-ui ===

(def swagger-ui-path-prefix "/swagger-ui")

(def openapi-route
  [(str swagger-ui-path-prefix "/openapi.json")
   {:get {:no-doc true
          :openapi {:info {:title "my-api"
                           :description "openapi3 docs with reitit-ring"
                           :version "0.0.1"}
                    :components {:securitySchemes {"auth" {:type :apiKey
                                                           :in :header
                                                           :name "Example-Api-Key"}}}}
          :handler (openapi/create-openapi-handler)}}])

(def swagger-ui-handler
  (ring/routes
   (swagger-ui/create-swagger-ui-handler
    {:path swagger-ui-path-prefix
     :config {:validatorUrl nil
              :urls [{:name "openapi", :url "openapi.json"}]
              :urls.primaryName "openapi"
              :operationsSorter "alpha"}})
   (ring/create-default-handler)))

; === routes ===

(def routes
  [openapi-route
   ["/hi" {:get hi}]
   ["/bug" {:get bug}]
   book-api/routes])

(def routes-options
  {:validate rrs/validate
   :exception pretty/exception
   :reitit.middleware/transform print-request-diffs
   :data {:muuntaja m/instance
          :coercion coercion-spec/coercion
            ;; middleware order: IO -> APP
          :middleware [openapi/openapi-feature
                       muuntaja/format-middleware
                       error-middleware
                       coercion/coerce-request-middleware
                       coercion/coerce-response-middleware]}})

; === ring-handler ===

(def ring-handler
  (ring/ring-handler
   (ring/router routes routes-options)
   ;; swagger-ui
   swagger-ui-handler
   ;; redirect slash handler: /halo/ -> 302 Location: /halo
   (ring/redirect-trailing-slash-handler)))

; === start-server ===

(defonce server (atom nil))

(defn stop-server []
  (when-let [s @server]
    (hk/server-stop! s {:timeout 100})
    (reset! server nil)))

(defn start-server []
  (let [s (hk/run-server #'ring-handler
                         {:port 8080
                          :legacy-return-value? false
      ; :worker-pool (java.util.concurrent.Executors/newVirtualThreadPerTaskExecutor)
                          })]
    (reset! server s)
    (t/log! :info (str "Server is listening on :" (hk/server-port s)))))

(defn restart-server []
  (stop-server)
  (start-server))

(comment
  (do
    (start-logging)
    (restart-server)))

(comment
  (stop-server))

; === main ===

(defn -main
  [& args]
  (start-logging)
  (start-server)
  @(promise))

; TODO: 
; 1. integrant
; 2. repl
; 3. request logging
