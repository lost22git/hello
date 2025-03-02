(ns lost.reitit-demo
  (:gen-class)
  (:require
   [lost.reitit-demo.user-api :as user-api]
   [org.httpkit.server :as hk]
   [reitit.ring :as ring]
   ;; integrant
   [integrant.core :as ig]
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

; === ig config ===

(def config
  (ig/read-string (slurp "config.edn")))

; === ig logging ===

(defmethod ig/init-key ::logging
  [_ {:keys [min-level]}]
  (t/set-min-level! min-level))

; === ig server ===

(defmethod ig/init-key ::server
  [_ {:keys [port handler]}]
  (let [server (hk/run-server handler
                              {:port port
                               :legacy-return-value? false
      ; :worker-pool (java.util.concurrent.Executors/newVirtualThreadPerTaskExecutor)
                               })
        _ (t/log! :info (str "Server is listening on :" (hk/server-port server)))]
    server))

(defmethod ig/halt-key! ::server
  [_ server]
  (do
    (hk/server-stop! server {:timeout 100})
    (t/log! :info "Server was stopped")))

; === ig ring-handler ===

(defmethod ig/init-key ::ring-handler
  [_ {:keys [openapi-support]}]
  (ring/ring-handler
   (ring/router
    [(:openapi-route openapi-support)
     ["/hi" {:get (fn [_] {:status 200 :body "hi!"})}]
     ["/bug" {:get (fn [_] (throw (ex-info "bug!" {:foo "bar"})))}]
     user-api/routes]

    {:validate rrs/validate
     :exception pretty/exception
     ; :reitit.middleware/transform print-request-diffs
     :data {:muuntaja m/instance
            :coercion coercion-spec/coercion
            ;; middleware order: IO -> APP
            :middleware [openapi/openapi-feature
                         muuntaja/format-middleware
                         error-middleware
                         coercion/coerce-request-middleware
                         coercion/coerce-response-middleware]}})

   ;; swagger-ui
   (:swagger-ui-handler openapi-support)
   ;; redirect slash handler: /halo/ -> 302 Location: /halo
   (ring/redirect-trailing-slash-handler)))

; === ig openapi-support ===

(defmethod ig/init-key ::openapi-support
  [_ {:keys [swagger-ui openapi]}]
  {:swagger-ui-handler
   (ring/routes
    (swagger-ui/create-swagger-ui-handler swagger-ui)
    (ring/create-default-handler))
   :openapi-route
   [(str (:path swagger-ui) "/openapi.json")
    {:get {:no-doc true
           :openapi openapi
           :handler (openapi/create-openapi-handler)}}]})

; === main ===

(defn -main
  [& args]
  (ig/init config))

; TODO: 
; - [ ] bb.edn
; - [ ] profiles
; - [ ] request logging
; - [ ] auth

