#!/usr/bin/env -S racket -r

#lang racket

(require web-server/servlet-env
         web-server/http
         web-server/dispatch
         net/url-string
         json)

(define (index req)
  (response/xexpr
   `(html (head (title "hello racket"))
          (body (p "hello racket xxx")))))

(define (json req)
  (response/jsexpr #hash([title . "hello racket"]
                         [status . "OK"])))

;; use web-server/dispatch to url-based dispatch
(define-values (app url)
  (dispatch-rules [("") index] [("json") json]))

;; use match to url-based dispatch
;; (define request->path
;;   (compose path->string url->path request-uri))
;; (define (not-found-404 req)
;;   (response/full 404
;;                  #"NOT FOUND"
;;                  (current-seconds)
;;                  #"text/plain; charset=utf-8"
;;                  null ;; list of header
;;                  (list #"NOT FOUND")))
;; (define (app req)
;;   (let ([path (request->path req)])
;;     (match path
;;       ;; why not "/"
;;       ["/." (index req)]
;;       ["/json" (json req)]
;;       [else (not-found-404 req)])))

(serve/servlet app
               #:port 9090
               #:servlet-path "/"
               #:servlet-regexp #rx""
               #:launch-browser? false)
