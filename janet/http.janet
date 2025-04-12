#!/usr/bin/env janet

(import spork/http)

(def resp (http/request
            "GET"
            "http://api64.ipify.org"
            :headers
            {"User-Agent" "curl/8.12.1"
             "Accept" "*/*"}))

(case (resp :status)
  200
  (-> resp
      (get :body)
      pp)
  (-> resp
      (get :message)
      pp))
