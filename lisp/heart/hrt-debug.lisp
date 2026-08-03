(in-package #:hrt)

(cffi:defcfun ("hrt_add_output" hrt-add-output) :boolean
  (server (:pointer (:struct hrt-server))))
