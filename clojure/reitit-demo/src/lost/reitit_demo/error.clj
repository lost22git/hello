(ns lost.reitit-demo.error
  (:require
   ;; logging
   [taoensso.telemere :as t]
   ;; exception
   [reitit.ring.middleware.exception :as exception]))

(defn default-error-handler [err req]
  (t/error! err)
  {:status 500
   :body {:err {:msg (ex-message err)
                :data (ex-data err)}
          :time (java.time.Instant/now)
          :method (:request-method req)
          :uri (:uri req)}})

(defn coercion-error-handler [err req]
  {:status 500
   :body {:err {:msg "coercion error"
                :data (:body ((exception/create-coercion-handler 500) err req))}
          :time (java.time.Instant/now)
          :method (:request-method req)
          :uri (:uri req)}})

(def error-middleware
  (exception/create-exception-middleware
   (merge
    exception/default-handlers
    {:reitit.coercion/request-coercion coercion-error-handler
     ::exception/default default-error-handler})))
