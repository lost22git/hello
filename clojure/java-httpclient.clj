#!/usr/bin/env -S clj -M

(import '[java.net URI Proxy ProxySelector InetSocketAddress])
(import '[java.net.http
          HttpClient
          HttpClient$Version
          HttpRequest
          HttpResponse
          HttpResponse$BodyHandlers])
(import '[java.nio ByteBuffer])
(import '[java.util.concurrent Executors Callable])

(require '[clojure.string :as str])

(set! *warn-on-reflection* true)

(defmacro printfn
  "A variant of `printf` with newline and autoflush"
  [^String fmt & args]
  `(do (printf (str ~fmt "\n") ~@args)
       (flush)))

(defmacro str+
  "(str ...) but 'nil' instead of '' for nil"
  [& args]
  (let [xs (map #(list 'or % "nil") args)]
    `(str ~@xs)))

(comment
  (str+ nil)
  (str+ 11 "/" (first nil) "/" nil))

(defmacro or+
  [x p & body]
  `(if (~p ~x) ~x (do ~@body)))

(comment
  (or+ 2 odd? (println "compute default value") 42)
  (or+ 2 even? (println "compute default value") 42))

(defn current-thread-name
  "Current thread name:
  - thread name
  - threadgroupname-threadid
  "
  []
  (let [t (Thread/currentThread)
        t-name (.getName t)]
    (if (and (some? t-name) (not= "" t-name))
      t-name
      (->> t
           ((juxt #(or (some-> %
                               (Thread/.getThreadGroup)
                               (ThreadGroup/.getName))
                       "nil")
                  Thread/.threadId))
           (apply format "%s-%d")))))

(comment
  (current-thread-name))

(defn callable
  "Returns a new function which has args of `f`,
  and returns java.util.concurrent.Callable applied `f`"
  [f]
  (comp
   bound-fn*
   (fn ^java.util.concurrent.Callable
     ([] f)
     ([x] (fn [] (f x)))
     ([x y] (fn [] (f x y)))
     ([x y & args] (fn [] (apply f x y args))))))

(defn vpmap
  "(pmap ...) but dispatch to virtual-threads"
  [f coll]
  (let [mf (callable f)
        calls (mapv mf coll)
        ;; here will blocking wait,
        ;; and all of futs will be done
        futs (with-open [executor (Executors/newVirtualThreadPerTaskExecutor)]
               (.invokeAll executor calls))]
    ;; returns lazy-seq of values of futs
    (map deref futs)))

(defn call-wtih-retry
  "Retry `f` with `config`"
  [{:keys [retry-on
           retry-duration-ms]}
   f]
  (loop [n 0]
    (let [[tag val] (try [:ok (f)]
                         (catch Exception exn
                           [:err exn]))]
      (case tag
        :ok val
        :err (let [err val
                   retry? (try (retry-on n err)
                               (catch Exception exn
                                 (throw (ex-info  "RETRY-ON-ERROR" {} exn))))]
               (if retry?
                 (let [^long ms (try (retry-duration-ms n err)
                                     (catch Exception exn
                                       (throw (ex-info "RETRY-DURATION-MS-ERROR" {} exn))))]
                   (Thread/sleep ms)
                   (recur (inc n)))
                 (throw err)))))))

(defmacro with-retry
  "Retry `body` with `config`"
  [config & body]
  `(call-wtih-retry ~config (^:once fn* [] ~@body)))

(comment
  (with-retry {:retry-on (fn [n e] (and (some? e) (< n 3)))
               :retry-duration-ms (fn [n _] 1000)}
    (printfn "[%d] run..." (System/currentTimeMillis))
    (/ 1 0)))

(defn call-with-fallback
  [fallback-f f]
  (try (f)
       (catch Exception exn
         (fallback-f exn))))

(defmacro with-fallback
  [fallback-f & body]
  `(call-with-fallback ~fallback-f (^:once fn* [] ~@body)))

;; === Http ===

(def ^:dynamic ^ProxySelector *http-proxy* (ProxySelector/getDefault))

(defonce local-http-proxy (some->>
                           (System/getenv "HTTP_PROXY")
                           (URI/create)
                           ((juxt URI/.getHost URI/.getPort))
                           (apply ^[String,int] InetSocketAddress/createUnresolved)
                           (ProxySelector/of)))

(defn- inspect-response [^HttpResponse response]
  (printfn
   "=======================================
VERSION : %s
STATUS  : %s
BODY    : %s
"
   (str+ (.version response))
   (str+ (.statusCode response))
   (str+ (.body response))))

(defn random-bytes
  "Random `n` bytes"
  ^bytes [^long n]
  (with-open [http-client (->
                           (HttpClient/newBuilder)
                           (.proxy *http-proxy*)
                           (.build))]
    (let [url (format "https://httpbin.org/bytes/%d" n)
          uri (URI/create url)
          _ (printfn "PROXY-SELECT: %s" (-> *http-proxy*
                                            (.select uri)
                                            ^Proxy (.get 0)
                                            (.address)))
          request (-> uri
                      (HttpRequest/newBuilder)
                      (.version HttpClient$Version/HTTP_1_1)
                      (.header "Accept" "application/octet-stream")
                      (.GET)
                      (.build))
          response (.send http-client
                          request
                          (HttpResponse$BodyHandlers/ofByteArray))]

      (inspect-response response)

      (when (not= 200 (.statusCode response))
        (throw (ex-info "HTTP-STATUS-ERROR" {:status-code (.statusCode response)})))
      (.body response))))

(defn random-int
  "Random int"
  ^long []
  (-> ^bytes (random-bytes 4)
      (doto (-> alength (= 4) assert))
      (ByteBuffer/wrap)
      (.getInt)))

(comment
  (println (random-int)))

(defn random-ints
  "Random `n` ints"
  ^ints [^long n]
  (let [f (fn [i]
            (printfn "[%s] [random-ints] #%d ..."
                     (current-thread-name) i)
            (->>
             (random-int)
             (with-retry {:retry-on (fn [n e] (and (some? e) (< n 3)))
                          :retry-duration-ms (fn [n e] 1000)})
             (with-fallback (fn [exn] 0))))]
    (->> (range n) (vpmap f) (int-array))))

;; === Main ===

(binding [*http-proxy*
          (or local-http-proxy *http-proxy*)]
  (let [n 4
        a (random-ints n)]
    (printfn "(random-ints %d) = [%s]" n
             (str/join ", " a))))
