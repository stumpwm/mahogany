(in-package #:hrt)

(defun seat-set-cursor-img (seat cursor-name)
  (declare (type string cursor-name)
           (type cffi:foreign-pointer seat))
  (cffi:with-foreign-string (str cursor-name)
    (hrt-seat-set-cursor-img seat str)))
