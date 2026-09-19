;;; A place to put useful functions that are shared between different files
(defpackage #:mahogany/util
  (:use #:cl)
  (:export #:mahogany-error
           #:mahogany-panic
           #:invalid-operation
           #:condition-text
           #:defglobal
           #:disable-fpu-exceptions
           #:enable-debugger
           #:find-free-number
           #:silence-notes
           #:rest-seq))

(in-package #:mahogany/util)

(define-condition mahogany-error (error)
  ((text :initarg :text :reader condition-text))
  (:documentation "Generic error condition for mahogany"))

(define-condition mahogany-panic (mahogany-error)
  ()
  (:documentation "Fatal error that cannot be recovered from.
When this error is signaled, the only appropriate thing to do is exit."))

(define-condition invalid-operation (mahogany-error)
  ()
  (:documentation "Used when an invalid operation was requested"))

(defmacro defglobal (name value &optional doc)
  #+sbcl
  `(sb-ext:defglobal ,name ,value ,doc)
  #+ccl
  `(ccl:defstatic ,name ,value ,doc)
  #+(not (or ccl sbcl))
  `(defvar ,name ,value ,doc))

(defun enable-debugger ()
  #+sbcl
  (sb-ext:enable-debugger)
  #+clasp
  (ext:enable-debugger))

(defmacro silence-notes (&body body)
  `(locally
       (declare
        #+sbcl
        (sb-ext:muffle-conditions sb-ext:compiler-note))
     ,@body))

#+sbcl
(declaim (sb-ext:maybe-inline rest-seq))
(defun rest-seq (seq)
  (declare (type sequence seq)
           (optimize (speed 3)))
  ;; SBCL complains about missed optimizations,
  ;; so break vector and simple-array types
  ;; into different cases, even though the code
  ;; is the same:
  (etypecase seq
    (simple-array (make-array (- (length seq) 1)
                              :displaced-to seq
                              :element-type (array-element-type seq)
                              :displaced-index-offset 1))
    (vector (make-array (- (length seq) 1)
                        :displaced-to seq
                        :element-type (array-element-type seq)
                        :displaced-index-offset 1))
    (list (cdr seq))))

(defun find-free-number (l &optional (min 0) dir)
  (declare (type integer min))
  "Return a number that is not in the list l. If dir is :negative then
look for a free number in the negative direction. anything else means
positive direction."
  (let* ((dirfn (if (eq dir :negative) '> '<))
         ;; sort it and crop numbers below/above min depending on dir
         (nums (sort (remove-if (lambda (n)
                                  (funcall dirfn n min))
                                l) dirfn))
         (max (car (last nums)))
         (inc (if (eq dir :negative) -1 1))
         (new-num (loop for n = min then (+ n inc)
                        for i in nums
                        when (/= n i)
                        do (return n))))
    (the integer (if new-num
                     new-num
                     ;; there was no space between the numbers, so use the max+inc
                     (if max
                         (+ inc max)
                         min)))))
