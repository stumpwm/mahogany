;;; A place to put useful functions that are shared between different files

(in-package #:mahogany/util)

(define-condition initialization-error (error)
  ((text :initarg text :reader text))
  (:documentation "Used when initializaion goes wrong"))

(define-condition invalid-operation (error)
  ((text :initarg text :reader text))
  (:documentation "Used when an invalid operation is attempted"))
