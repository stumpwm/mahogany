;;; A place to put useful functions that are shared between different files
(defpackage #:mahogany/util
  (:use #:cl)
  (:export #:mahogany-error
	   #:defglobal
	   #:disable-fpu-exceptions
	   #:enable-debugger
	   #:if-let*))

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

(defun enable-debugger ()
  #+sbcl
  (sb-ext:enable-debugger))

(defun disable-fpu-exceptions ()
  #+sbcl
  (sb-int:set-floating-point-modes :traps nil)
  #+ccl
  (ccl:set-fpu-mode :overflow nil))

(defmacro if-let* (vars &body (then-form &optional else-form))
  (let* ((end-tag (gensym "exit"))
		 (var-reverse (reverse vars))
		 (let-body `(alexandria:when-let (,(car var-reverse))
                      ,then-form
					  (go ,end-tag))))
    (dolist (v (reverse (rest var-reverse)))
      (setf let-body `(alexandria:when-let (,v)
						,let-body)))
	`(tagbody
		,let-body
		,else-form
		,end-tag)))
