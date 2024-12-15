(ns lost.reitit-demo
  (:gen-class)
  (:require
   [lost.reitit-demo.book-api :as book-api]
   [org.httpkit.server :as hk]
   [reitit.dev.pretty :as pretty]
   [reitit.ring :as ring]
   [reitit.ring.middleware.exception :as exception]
   ;; content negotiation
   [reitit.ring.middleware.muuntaja :as muuntaja]
   [muuntaja.core :as m]
   ;; coercion and spec
   [reitit.ring.coercion :as coercion]
   [reitit.coercion.spec :as spec]))

(defn halo [_req]
  {:status 200 :body "halo!"})

(defn bug [_req]
  (throw (ex-info "bug!" {:foo "bar"})))

;;---------------;;
;; error handler ;;
;;---------------;;

(defn handle-error [tag err req]
  {:status 500
   :body {:err.msg (ex-message err)
          :err.data (ex-data err)
          :time (java.time.Instant/now)
          :req.method (:request-method req)
          :req.uri (:uri req)}})

(def error-middleware
  (exception/create-exception-middleware
   (merge
    exception/default-handlers
    {::exception/default (partial handle-error :default)
     ::exception/wrap (fn [error-handler err req]
                        (println "ERROR" (pr-str (:uri req)))
                        (error-handler err req))})))

;;--------;;
;; routes ;;
;;--------;;

(def routes-data
  [["/halo" {:get halo}]
   ["/bug" {:get bug}]
   book-api/routes-data])

(def routes-options
  {:exception pretty/exception
   :data {:muuntaja m/instance
          :coercion spec/coercion
            ;; middleware order: IO -> APP
          :middleware [muuntaja/format-middleware
                       error-middleware
                       coercion/coerce-exceptions-middleware
                       coercion/coerce-request-middleware
                       coercion/coerce-response-middleware]}})

;;---------;;
;; handler ;;
;;---------;;

(def handler
  (ring/ring-handler
   (ring/router routes-data routes-options)
   ;; redirect slash handler: /halo/ -> 302 Location: /halo
   (ring/redirect-trailing-slash-handler)
   ;; default handler
   (ring/create-default-handler)))

;;--------------;;
;; start-server ;;
;;--------------;;

(defonce server (atom nil))

(defn stop-server []
  (when-let [s @server]
    (hk/server-stop! s {:timeout 100})
    (reset! server nil)))

(defn start-server []
  (let
   [s (hk/run-server #'handler
                     {:port 8000
                      :legacy-return-value? false
      ; :worker-pool (java.util.concurrent.Executors/newVirtualThreadPerTaskExecutor)
                      })]
    (reset! server s)
    (println "Server is listening on :" (hk/server-port s))))

(defn restart-server []
  (stop-server)
  (start-server))

(comment
  (restart-server))
(comment
  (stop-server))

;;------;;
;; main ;;
;;------;;

(defn -main
  [& args]
  (start-server)
  @(promise))
