#!/usr/bin/env -S sbcl --script

;; clingon
;; https://github.com/dnaeon/clingon

(load "~/quicklisp/setup.lisp")
(ql:quickload :clingon)
(sb-ext:add-package-local-nickname :cli :clingon)

(defun greet/options ()
  "options of greet command"
  (list
   (cli:make-option
    :string
    :key :name
    :long-name "to"
    :description "name to greet"
    :initial-value "stranger"
    :env-vars '("USER"))
   (cli:make-option
    :integer
    :key :times
    :short-name #\n
                 :long-name "times"
    :description "times of greeting"
    :initial-value 1)))

(defun greet/handler (cmd)
  "handler of greeet command"
  (let ((times (cli:getopt cmd :times))
        (name (cli:getopt cmd :name)))
    (loop repeat times
          do (format t "HELLO ~A~%" name))))

(defun greet/command ()
  "greet command"
  (cli:make-command
   :name "greet"
   :description "Greeting to someone with many times"
   :version "0.0.1"
   :authors '("lost")
   :options (greet/options)
   :handler #'greet/handler))

;; run greet command
(let ((app (greet/command)))
  (cli:run app))
