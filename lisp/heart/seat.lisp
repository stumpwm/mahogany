(in-package #:hrt)

(defun seat-grab (seat cursor-name)
  (declare (type string cursor-name)
           (type cffi:foreign-pointer seat))
  (cffi:with-foreign-string (str cursor-name)
    (hrt-seat-grab seat str)))

#-HRT-DEBUG
(declare (inline seat-grabbed-p))
(defun seat-grabbed-p (seat)
  (cffi:foreign-slot-value seat '(:struct hrt-seat)
                           'grabbed))
