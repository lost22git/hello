#!/usr/bin/env janet

(comment
  # === JSON ===

  (import spork/json)

  (let [buf @""
        data {:name "Janet"
              :tags @["Functional" "Lisp"]}]
    (json/encode data "" "" buf)
    (-> (json/decode buf true true)
        (table/to-struct)
        tracev
        (deep= data)
        assert))

  # end comment
  )


(comment
  # === HTTP ===

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

  # end comment
  )


(comment
  # ===  ArgParse ===

  (import spork/argparse :prefix "")
  (import spork/schema)

  (def cli-spec ["Greeting cli app"
                 "name" {:help "Name to greet"
                         :kind :option
                         :required true}
                 "times" {:help "Times of greeting"
                          :short "n"
                          :kind :option
                          :required false
                          :default "1"
                          :map scan-number}])

  (def cli-validator
    (schema/validator
      (props
        "name" (and :string (length 3 10))
        "times" (and :number (pred pos?)))))

  (with-dyns [:args
              @["app" "--name" "king" "-n" "-2"]]
    (let [{"name" name "times" times}
          (cli-validator (argparse ;cli-spec))]
      (repeat times
        (print "HELLO -> " name))))

  # end comment
  )
