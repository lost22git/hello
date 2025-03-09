(ns lost.reitit-demo
  (:gen-class)
  (:require
   [clojure.java.io :as io]
   [lost.reitit-demo.user-api :as user-api]
   [lost.reitit-demo.error :as error]
   [org.httpkit.server :as hk]
   [reitit.ring :as ring]
   ;; integrant
   [integrant.core :as ig]
   ;; logging
   [taoensso.telemere :as t]
   ;; dev
   [reitit.dev.pretty :as pretty]
   ; [reitit.ring.middleware.dev :refer [print-request-diffs]]
   [reitit.ring.spec :as rrs]
   ;; content negotiation
   [reitit.ring.middleware.muuntaja :as muuntaja]
   [muuntaja.core :as m]
   ;; coercion and spec
   [reitit.ring.coercion :as coercion]
   [reitit.coercion.spec :as coercion-spec]
   ;; openapi
   [reitit.openapi :as openapi]
   [reitit.swagger-ui :as swagger-ui]))

(set! *warn-on-reflection* true)

; === ig config ===

(def config
  "read config in these order
  1. config.edn in local fs
  2. config.edn in jar fs
  "
  (ig/read-string
   (try
     (slurp "config.edn")
     (catch Exception _e
       (println "Read config from config.edn in jar")
       (slurp (io/resource "config.edn"))))))

; === ig logging ===

(defmethod ig/init-key ::logging
  [_ {:keys [min-level]}]
  #_{:clj-kondo/ignore [:unresolved-var]}
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
   ; router
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
            ; middleware order: IO -> APP
            :middleware [openapi/openapi-feature
                         muuntaja/format-middleware
                         error/error-middleware
                         coercion/coerce-request-middleware
                         coercion/coerce-response-middleware]}})

   ; default handler
   (ring/routes
    (:swagger-ui-handler openapi-support)
    ; redirect slash handler: /users/ -> 301 Moved Permanently - Location: /users
    (ring/redirect-trailing-slash-handler)
    (ring/create-default-handler))))

; === ig openapi-support ===

(defmethod ig/init-key ::openapi-support
  [_ {:keys [swagger-ui openapi]}]
  {:swagger-ui-handler
   (swagger-ui/create-swagger-ui-handler swagger-ui)
   :openapi-route
   [(str (:path swagger-ui) "/openapi.json")
    {:get {:no-doc true
           :openapi openapi
           :handler (openapi/create-openapi-handler)}}]})

; === main ===

(defn -main
  [& _args]
  (ig/init config))

; TODO: 
; - [ ] profiles
; - [ ] request logging
; - [ ] auth

