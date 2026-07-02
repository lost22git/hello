#!/usr/bin/env -S clj -M

(require '[clojure.pprint :refer [pprint print-table]])
(require '[clojure.string :refer [join]])

(defn print-title
  [title  & {:keys [len] :or {len 44}}]
  (println "\n" title "\n"  (join "" (repeat len \=))))

(defn thread->map
  "Converto java Thread to map"
  [^Thread t]
  {:id (.getId t)
   :name (.getName t)
   :group (.getName (.getThreadGroup t))
   :state (str (.getState t))
   :priority (.getPriority t)})

(defn get-thread-table
  "Get thread table in current runtime"
  []
  (->> (Thread/getAllStackTraces)
       (.keySet)
       (map thread->map)
       (sort-by :id)
       (into [])))

(defn inspect-threads
  "Inspect threads"
  []
  (print-title "Thread Table")
  (print-table (get-thread-table))

  ;; add-tap: tap-loop thread
  (add-tap println)
  (print-title "Thread Table (after call add-tap)")
  (print-table (get-thread-table))

  ;; future
  ;; see agent send-off executor
  (future (fn [] 1))
  (print-title "Thread Table (after call future)")
  (print-table (get-thread-table))

  ;; send-off (for blocking operations)
  ;;  - pool-name: clojure-agent-send-off-pool
  ;;  - pool-var: clojure.lang.Agent/soloExecutor (default: CacheThreadPoolExecutor)
  ;;  - set pool: (set-agent-send-off-executor! custom-executor)
  ;; send (for pure computation operations)
  ;;  - pool-name: clojure-agent-send-off-pool
  ;;  - pool-var: clojure.lang.Agent/poolExecutor (default: CacheThreadPoolExecutor)
  ;;  - set pool: (set-agent-send-executor! custom-executor)
  (let [a (agent 1)]
    (dotimes [_ 1000]
      (send a inc))
    (print-title "Thread Table (after call sned agent)")
    (print-table (get-thread-table))

    (dotimes [_ 1000]
      (send-off a inc))
    (print-title "Thread Table (after call sned-off agent)")
    (print-table (get-thread-table)))

  ;; pmap
  ;; see agent send-off executor
  (doall (pmap (fn [v] v) (range 0 1000)))
  (print-title "Thread Table (after call pmap)")
  (print-table (get-thread-table)))

(inspect-threads)

;; shutdown-agents
(shutdown-agents)
