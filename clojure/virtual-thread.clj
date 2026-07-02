(require '[clojure.core.async :as a])

(defn virtual-thread?
  []
  (.isVirtual (Thread/currentThread)))

(a/<!!
 (a/go
   (println "(a/go ...) virtual-thread?" (virtual-thread?))))

(a/<!!
 (a/thread
   (println "(a/thread ...) virtual-thread?" (virtual-thread?))))

(a/<!!
 (a/io-thread
  (println "(a/io-thread ...) virtual-thread?" (virtual-thread?))))

(set-agent-send-off-executor!
 (java.util.concurrent.Executors/newVirtualThreadPerTaskExecutor))

@(future
   (println "(future ...) virtual-thread?" (virtual-thread?)))
