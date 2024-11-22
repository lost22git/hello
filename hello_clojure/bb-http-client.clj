#!/usr/bin/env bb
(require '[babashka.http-client :as http])
(require '[cheshire.core :as json])

; get
(assert
 (= 888
    (:status (http/get "https://httpbin.org/status/888" {:throw false}))))

; post json and decode json resp body
(-> (http/post "https://httpbin.org/post"
                          {:header {:content-type "application/json"}
                           :body (json/encode {:foo "bar"})})
    :body
    (json/parse-string true)
    :data
    (json/parse-string true)
    :foo
    (= "bar")
    assert
 )

