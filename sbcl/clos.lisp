#!/usr/bin/env -S sbcl --script

;; :accessor - reader/writer method of slot
;; :initarg - key of slot on make-instance function
;; :initform - default value of slot on make-instance function

(defclass media ()
  ((id
    :accessor media-id
    :initarg :id
    :initform (error "slot id must be initialized."))
   (link
    :accessor media-link
    :initarg :link
    :initform (error "slot link must be initialized."))
   (create-at
    :initarg :create-at
    :initform "2022-02-02T00:00:00Z")))

;; a method calling after initialize-instance during make-instance
(defmethod initialize-instance :after ((obj media) &key)
  (with-slots (id) obj
    (unless (<= 3 (length id))
            (error "slot id's length must > 3."))))

(defclass video (media)
  ((format
    :accessor video-format
    :initarg :format
    :initform (error "slot format must be initialized."))
   (resolution
    :accessor video-resolution
    :initarg :resolution
    :initform (error "slot resolution must be initialized."))))

(defclass audio (media)
  ((format
    :accessor audio-format
    :initarg :format
    :initform (error "slot format must be initialized."))
   (bitrate
    :accessor audio-bitrate
    :initarg :bitrate
    :initform (error "slot bitrate must be initialized."))))

;; multi-methods
(defgeneric foo (obj)
  (:documentation "foo multi-methods")
  (:method-combination progn)
  (:method progn ((obj media)) (format t "foo media...~&"))
  (:method progn ((obj video)) (format t "foo video...~&"))
  (:method progn ((obj audio)) (format t "foo audio...~&")))

;; :before / :after / around methods
(defmethod foo :around ((obj video))
  (format t "begin around foo video...~&")
  (when (next-method-p)
    (call-next-method))
  (format t "end around foo video...~&"))

(trace foo :methods t)

;; === TEST ===

(let ((vid (make-instance 'video
                          :id "acb"
                          :link "https://cdn.io/v/abc.mp4"
                          :format "video/mp4"
                          :resolution "1920x1080"))
      (aud (make-instance 'audio
                          :id "xyz"
                          :link "https://cdn.io/a/xyz.mp3"
                          :format "audio/mp3"
                          :bitrate "192k")))

  (format t "===VIDEO===")
  (with-slots (id link create-at format resolution) vid
    (pprint id)
    (pprint link)
    (pprint create-at)
    (pprint format)
    (pprint resolution))
  (format t "~&===AUDIO===")
  (with-accessors ((id media-id) (link media-link)) aud
    (pprint id)
    (pprint link))
  (pprint (slot-value aud 'create-at))
  (pprint (audio-format aud))
  (pprint (audio-bitrate aud))
  (format t "~&===MULTI-METHODS===~&")
  (foo vid)
  (foo aud))
