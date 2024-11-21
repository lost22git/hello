#!/usr/bin/env bb

(require '[clojure.tools.logging :as log])

(log/info "halo" "clojure")
(log/infof "%s %s" "halo" "clojure")
