#!/usr/bin/env -S sbcl --script

(load "~/quicklisp/setup.lisp")
(ql:quickload :com.inuoe.jzon)
(sb-ext:add-package-local-nickname :json :com.inuoe.jzon)

(defstruct result
  status
  success
  data)

(defstruct result-data
  id
  deletehash
  type
  width
  height
  size
  datetime
  link
  tags)

(defun hash-table->result (m)
  (let ((status (gethash "status" m))
        (success (gethash "success" m))
        (data (gethash "data" m)))
    (make-result
     :status status
     :success success
     :data (hash-table->result-data data))))

(defun hash-table->result-data (m)
  (let ((id (gethash "id" m))
        (deletehash (gethash "deletehash" m))
        (type (gethash "type" m))
        (width (gethash "width" m))
        (height (gethash "height" m))
        (size (gethash "size" m))
        (datetime (gethash "datetime" m))
        (link (gethash "link" m))
        (tags (gethash "tags" m)))
    (make-result-data
     :id id
     :deletehash deletehash
     :type type
     :width width
     :height height
     :size size
     :datetime datetime
     :link link
     :tags tags)))

(let* ((r (make-result
           :status 1
           :success t
           :data (make-result-data
                  :id "sds"
                  :deletehash "sdfs"
                  :type "image/png"
                  :width 200
                  :height 200
                  :size 100010
                  :datetime 1750520472
                  :link "https://i.imgur.com/JEE8NZv.jpeg"
                  :tags #())))
       ;; encode
       (s (json:stringify r :stream nil :pretty t))
       ;; decode
       (r2  (hash-table->result (json:parse s))))
      (assert (equalp r r2)))


