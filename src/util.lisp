;;; A place to put useful functions that are shared between different files

(defpackage #:mh-util
  (:use #:cl #:wayland-server-core))

(in-package #:mh-util)

(define-condition initialization-error (error)
  ((text :initarg text :reader text))
  (:documentation "Used when initializaion goes wrong"))
