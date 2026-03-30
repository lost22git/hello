(module main ()
  (import http-client
          uri-common
          intarweb
          (scheme)
          (chicken io)
          (chicken base))
  (let* ([uri "https://httpbin.org/post"]
         [_ (print "Fetching " uri)]
         [req (make-request
               #:method 'POST
               #:uri (uri-reference uri)
               #:headers
               (headers '((contnet-type
                           application/json))))]
         [req-body
          "{ \"msg\": \"HELLO FROM CHICKEN\" }"]
         [resp-body (with-input-from-request
                     req
                     req-body
                     read-string)])
    (print resp-body)))
