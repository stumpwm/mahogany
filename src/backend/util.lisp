(defpackage #:backend/util
  (:use :cl :cffi :wayland-server-core))

(in-package #:backend/util)

(export '(get-listener-owner
	  register-listener
	  unregister-listener
	  remove-from-list
	  free-from
	  make-listener))

(defun get-listener-owner (listener table)
  (gethash (cffi:pointer-address listener) table))

(defun register-listener (listener owner table)
  (setf (gethash (cffi:pointer-address listener) table) owner))

(defun unregister-listener (listener table)
  (remhash (cffi:pointer-address  listener) table))

(defun remove-from-list (object place)
  (wl-list-remove (cffi:foreign-slot-pointer object
					     '(:struct wl_listener) place)))

(defun free-from (object slot)
  (cffi:foreign-free (slot-value object slot))
  ;; remove the invalid pointer from the object:
  (setf (slot-value object slot) nil))

(defmacro make-listener (callback)
  `(let ((listener (cffi:foreign-alloc '(:struct wl_listener))))
     (setf (cffi:foreign-slot-value listener '(:struct wl_listener) 'notify) (cffi:callback ,callback))
     listener))
