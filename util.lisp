;;; A place to put useful functions that are shared between different files
(defpackage #:mahogany/util
  (:use #:cl)
  (:export #:mahogany-error))

(in-package #:mahogany/util)

(define-condition mahogany-error (error)
  ()
  (:documentation "Generic error condition for mahogany"))

(define-condition initialization-error (mahogany-error)
  ((text :initarg text :reader text))
  (:documentation "Used when initializaion goes wrong"))

(define-condition invalid-operation (mahogany-error)
  ((text :initarg text :reader text))
  (:documentation "Used when an invalid operation is attempted"))
