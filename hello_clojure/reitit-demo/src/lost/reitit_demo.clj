(ns lost.reitit-demo
  (:require
   [org.httpkit.server :as hk]
   [reitit.dev.pretty :as pretty]
   [reitit.ring :as ring]
   [reitit.ring.middleware.exception :as exception]
   ; content negotiation
   [reitit.ring.middleware.muuntaja :as muuntaja]
   [muuntaja.core :as m]
   ; coercion and spec
   [reitit.ring.coercion :as coercion]
   [reitit.coercion.spec :as spec])
  (:gen-class))

(defn halo [_req]
  {:status 200 :body "halo!"})

(defn bug [_req]
  (throw (Exception. "bug!")))

(defrecord Book [id title created-at])

(defn get-book-by-id [{{{:keys [id]} :path} :parameters}]
  (->> (map->Book {:id id :title "the book"})
       (assoc {:status 200} :body)))

(def handler
  (ring/ring-handler
    ; router handler
   (ring/router
    [["/halo" {:get halo}]
     ["/bug" {:get bug}]
     ["/book"
      ["/:id" {:get {:summary "get book by id"
                     :parameters {:path {:id int?}}
                     :handler get-book-by-id}}]]]
    {:exception pretty/exception
     :data {:muuntaja m/instance
            :coercion spec/coercion
            ; middleware order: IO -> APP
            :middleware [muuntaja/format-middleware
                         exception/exception-middleware
                         coercion/coerce-exceptions-middleware
                         coercion/coerce-request-middleware
                         coercion/coerce-response-middleware]}})
    ; redirect slash handler: /halo/ -> 302 Location: /halo
   (ring/redirect-trailing-slash-handler)
   ; default handler
   (ring/create-default-handler)))

(defn start-server []
  (hk/run-server handler {:port 8000
                          :legacy-return-value? false
                          ; :worker-pool (java.util.concurrent.Executors/newVirtualThreadPerTaskExecutor)
                          }))
(defn -main
  [& args]
  (let [server (start-server)]
    (println "Server is listening on :" (hk/server-port server))
    @(promise)))
