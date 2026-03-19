;;; A place to put useful functions that are shared between different files
(defpackage #:mahogany/util
  (:use #:cl)
  (:export #:mahogany-error
           #:mahogany-panic
           #:defglobal
           #:disable-fpu-exceptions
           #:enable-debugger))

(in-package #:mahogany/util)

(define-condition mahogany-error (error)
  ()
  (:documentation "Generic error condition for mahogany"))

(define-condition mahogany-panic (mahogany-error)
  ((text :initarg text :reader text))
  (:documentation "Fatal error that cannot be recovered from.
When this error is signaled, the only appropriate thing to do is exit."))

(defmacro defglobal (name value &optional doc)
  #+sbcl
  `(sb-ext:defglobal ,name ,value ,doc)
  #+ccl
  `(ccl:defstatic ,name ,value ,doc)
  #+(not (or ccl sbcl))
  `(defvar ,name ,value ,doc))

(defun enable-debugger ()
  #+sbcl
  (sb-ext:enable-debugger))

(defun disable-fpu-exceptions ()
  #+sbcl
  (sb-int:set-floating-point-modes :traps nil)
  #+ccl
  (ccl:set-fpu-mode :overflow nil))
