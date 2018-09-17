(defpackage :mahogany/backend/input/input-manager
  (:use :cl :cffi :mahogany/backend/util
	:mahogany/backend/input/keyboard
	:mahogany/backend/server-protocol)
  (:import-from :mahogany/log
		#:log-string)
  (:import-from :wayland-server-core
		#:wl-signal-add
		#:wl-list-remove
		#:wl_listener
		#:link))

(export '(input-manager
	  make-input-manager))

(in-package :mahogany/backend/input/input-manager)

(defclass input-manager ()
  ((server :initarg :server
	   :reader mh-server)
   ;; alist of seats:
   (seats :initarg :seats
	  :accessor input-seats
	  :type list)
   (keyboards :accessor keyboards
	      :initform ()
	      :type list)
   (input-listener :initarg :input-listener
		   :reader input-listener))
  (:default-initargs
   :seats ())
  (:documentation "Class used to manage seats and their devices"))

(defparameter *default-seat-name* :MAHOGANY-SEAT)

;; (defun get-desired-seat (input-manager device)
;;   ;; TODO: add functions, rules, ect to decide which seat the device
;;   ;; goes to.
;;   (if-let ((seat (first (assoc *default-seat-name* (input-seats input-manager)))))
;;     seat
;;     (let ((new-seat (make-seat

(cffi:defcallback keyboard-destroy-notify :void
    ((listener :pointer)
     (keyboard (:pointer (:struct wlr:input-device))))
  (declare (ignore listener keyboard))
  (format t "A keyboard was destroyed~%"))

(defun add-keyboard (device)
  (let ((destroy-listener (make-listener keyboard-destroy-notify))
	(new-keyboard (with-foreign-object (rules '(:struct xkb:rule-names))
			(init-default-keyboard-rules rules)
			(make-keyboard device rules))))
    (wl-signal-add (cffi:foreign-slot-pointer device '(:struct wlr:input-device)
					      :event-destroy)
		   destroy-listener)
    (register-listener destroy-listener new-keyboard *listener-hash*)
    new-keyboard))

(cffi:defcallback handle-new-input :void
    ((listener :pointer)
     (input (:pointer (:struct wlr:input-device))))
  (let ((listener-owner (get-listener-owner listener *listener-hash*)))
    (log-string :info "new input device ~A"
		(cffi:foreign-slot-value input
					 '(:struct wlr:input-device) :type))
    ;; get the seat we will use:
    ;;(
    (case (cffi:foreign-slot-value input
  				   '(:struct wlr:input-device) :type)
      (:keyboard (push (add-keyboard input) (keyboards listener-owner)))
      (t (log-string :info "Something that isn't a keyboard was added")))))

(defun make-input-manager (server)
  (let ((new-input-listener (make-listener handle-new-input)))
    (wayland-server-core:wl-signal-add (cffi:foreign-slot-pointer (get-backend server)
								  '(:struct wlr:backend)
								  :event-new-input)
				       new-input-listener)
    (let ((new-manager (make-instance 'input-manager
				      :input-listener new-input-listener
				      :server server)))
      (register-listener new-input-listener new-manager *listener-hash*)
      (the input-manager new-manager))))

(defun destroy-input-manager (input-manager)
  (with-accessors ((input-listener input-listener)) input-manager
    (unregister-listener input-listener *listener-hash*)
    (wl-list-remove (foreign-slot-pointer input-listener
					  '(:struct wl_listener) 'link))
    (foreign-free input-listener)))
