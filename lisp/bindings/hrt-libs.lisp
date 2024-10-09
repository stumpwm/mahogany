(cl:in-package #:hrt)

(cffi:define-foreign-library libheart
  (:unix "libheart.so"))

(cffi:define-foreign-library libwlroots
  (:unix "libwlroots-0.18.so"))

(defun load-foreign-libraries ()
  (cffi:use-foreign-library libwlroots)
  (cffi:use-foreign-library libheart))
