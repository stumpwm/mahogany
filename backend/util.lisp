(in-package :mahogany/backend)

(defparameter *listener-hash* (make-hash-table))

(defun container-of (ptr type member)
  (cffi:make-pointer (- (cffi:pointer-address ptr) (cffi:foreign-slot-offset type member))))

(defun get-listener-owner (listener table)
  (gethash (cffi:pointer-address listener) table))

(defun register-listener (listener owner table)
  (setf (gethash (cffi:pointer-address listener) table) owner))

(defun register-listeners (owner table &rest listeners)
  (dolist (listener listeners)
    (register-listener listener owner table)))

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

(defun cleanup-listener (listener-ptr listener-table)
  (unregister-listener listener-ptr listener-table)
  (wl-list-remove (foreign-slot-pointer listener-ptr
					'(:struct wl_listener) 'link))
  (foreign-free listener-ptr))

(define-condition initialization-error (error)
  ((text :initarg text :reader text))
  (:documentation "Used when initializaion goes wrong"))
