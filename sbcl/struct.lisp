#!/usr/bin/env -S sbcl --script

(defstruct person
  (name "ken" :type string :read-only t))

(defstruct (hero (:include person))
  (level 10 :type (integer 0 100)))

(let ((h (make-hero)))
  (print (hero-name h))
  (print (slot-value h 'level))
  (incf (hero-level h) 20)
  (with-slots (name level) h
    (print name)
    (print level))
  (print h))
