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

  ;; future: clojure-agent-send-off-pool
  (future (fn [] 1))
  (print-title "Thread Table (after call future)")
  (print-table (get-thread-table))

  ;; send agent: clojure-agent-send-pool
  ;; send-off agent: clojure-agent-send-off-pool
  (let [a (agent 1)]
    (dotimes [_ 1000]
      (send a inc))
    (print-title "Thread Table (after call sned agent)")
    (print-table (get-thread-table))

    (dotimes [_ 1000]
      (send-off a inc))
    (print-title "Thread Table (after call sned-off agent)")
    (print-table (get-thread-table)))

;; pmap: clojure-agent-send-off-pool
  (doall (pmap (fn [v] v) (range 0 1000)))
  (print-title "Thread Table (after call pmap)")
  (print-table (get-thread-table)))

(inspect-threads)

;; shutdown-agents
(shutdown-agents)
