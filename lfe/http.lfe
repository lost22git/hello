#!/usr/bin/env lfe

(defun handle-result (result)
  "Handle http result"
  (let* [((tuple status headers body) result)
        (decoded_body (clj:-> body
                      (unicode:characters_to_binary)
                      (json:decode)))]
    (lfe_io:format "body: ~p~n" (list decoded_body))))


(defun handle-error (reason)
  "Handle http error"
  (lfe_io:format "error: ~p~n" (list reason)))


(defun request-sync () 
  "Request synchronously"
  (case (httpc:request "https://httpbin.org/ip")
    ((tuple 'ok result) 
      (handle-result result))
    ((tuple 'error reson) 
      (handle-error reason))
    (_ (lfe_io:format "I DON'T KNOW WHAT HAPPENED" ()))))


(defun request-async () 
  "Request asynchronously"
  (httpc:request 'get 
                 (tuple "https://httpbin.org/ip" ())
                 () 
                 (list (tuple 'sync 'false)))
  (receive
    ((tuple 'http (tuple request-id result)) 
      (handle-result result))
    ((tuple 'http (tuple request-id (tuple 'error reason))) 
      (handle-error reason))))


(inets:start)
(ssl:start)

(request-sync)
(request-async)
