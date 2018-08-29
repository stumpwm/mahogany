(defpackage #:mh-util
  (:use #:cl #:autowrap #:plus-c))

(in-package #:mh-util)

(export '(free-from))

(defun free-from (object slot)
  (autowrap:free (slot-value object slot))
  ;; remove the invalid pointer from the object:
  (setf (slot-value object slot) nil))
