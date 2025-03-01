(ns lost.reitit-demo
  (:gen-class)
  (:require
   [lost.reitit-demo.book-api :as book-api]
   [taoensso.telemere :as t]
   [org.httpkit.server :as hk]
   [reitit.dev.pretty :as pretty]
   [reitit.ring :as ring]
   [reitit.ring.middleware.exception :as exception]
   ;; content negotiation
   [reitit.ring.middleware.muuntaja :as muuntaja]
   [muuntaja.core :as m]
   ;; coercion and spec
   [reitit.ring.coercion :as coercion]
   [reitit.coercion.spec :as spec]
   ;; openapi
   [reitit.openapi :as openapi]
   [reitit.swagger-ui :as swagger-ui]))

(defn halo [_req]
  {:status 200 :body "halo!"})

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

; === routes ===

(defonce openapi-route
  ["/swagger-ui/openapi.json"
   {:get {:no-doc true
          :openapi {:info {:title "my-api"
                           :description "openapi3 docs with reitit-ring"
                           :version "0.0.1"}
                    :components {:securitySchemes {"auth" {:type :apiKey
                                                           :in :header
                                                           :name "Example-Api-Key"}}}}
          :handler (openapi/create-openapi-handler)}}])

(def routes-data
  [openapi-route
   ["/halo" {:get halo}]
   ["/bug" {:get bug}]
   book-api/routes-data])

(def routes-options
  {:exception pretty/exception
   :data {:muuntaja m/instance
          :coercion spec/coercion
            ;; middleware order: IO -> APP
          :middleware [openapi/openapi-feature
                       muuntaja/format-middleware
                       error-middleware
                       coercion/coerce-request-middleware
                       coercion/coerce-response-middleware]}})

; === ring-handler ===

(def ring-handler
  (ring/ring-handler
   (ring/router routes-data routes-options)
   ;; swagger-ui
   (ring/routes
    (swagger-ui/create-swagger-ui-handler
     {:path "/swagger-ui"
      :config {:validatorUrl nil
               :urls [{:name "openapi", :url "openapi.json"}]
               :urls.primaryName "openapi"
               :operationsSorter "alpha"}})
    ;; default handler
    (ring/create-default-handler))
   ;; redirect slash handler: /halo/ -> 302 Location: /halo
   (ring/redirect-trailing-slash-handler)))

; === start-server ===

(defonce server (atom nil))

(defn stop-server []
  (when-let [s @server]
    (hk/server-stop! s {:timeout 100})
    (reset! server nil)))

(defn start-server []
  (let
   [s (hk/run-server #'ring-handler
                     {:port 8080
                      :legacy-return-value? false
      ; :worker-pool (java.util.concurrent.Executors/newVirtualThreadPerTaskExecutor)
                      })]
    (reset! server s)
    (t/log! :info (str "Server is listening on :" (hk/server-port s)))))

(defn restart-server []
  (stop-server)
  (start-server))

; === main ===

(defn -main
  [& args]
  (start-logging)
  (start-server)
  @(promise))

; TODO: 
; 1. use integrant
; 2. request logging
