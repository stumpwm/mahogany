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

(defun seat-cursor-set-theme (seat theme-name cursor-size)
  "Set the theme of the cursor to THEME-NAME. If THEME-NAME is NIL,
use the system's default theme."
  (declare (type (or string null) cursor-name))
  (if theme-name
      (progn
        (cffi:with-foreign-string (str theme-name)
          (hrt-seat-cursor-set-theme seat str cursor-size)))
      (hrt-seat-cursor-set-theme seat (cffi:null-pointer) cursor-size)))
