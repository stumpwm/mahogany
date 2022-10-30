;;; A place to put useful functions that are shared between different files
(defpackage #:mahogany/util
  (:use #:cl)
  (:export #:mahogany-error
	   #:defglobal))

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

(defmacro defglobal (name value &optional doc)
  #+sbcl
  `(sb-ext:defglobal ,name ,value ,doc)
  #+ccl
  `(ccl:defstatic ,name ,value ,doc)
  #+(not (or ccl sbcl))
  `(defvar ,name ,value ,doc))
