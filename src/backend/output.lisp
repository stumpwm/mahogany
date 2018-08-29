(defpackage #:mh/backend/output
  (:use :cl :cffi :wayland-server-core :backend/util)
  (:import-from :mh-log
		:log-string)
  (:import-from :wayland-server-core
		#:wl-signal-add
		#:link))

(in-package #:mh/backend/output)

(export '(make-output
	  destroy-output
	  output-output))

(defvar *listener-hash* (make-hash-table))

(defclass output ()
  ((output-damage :type wlr:output-damage
		  :initarg :output-damage
		  :reader output-output-damage)
   (damage-frame-listener :type wl_listener
			  :initarg :damage-frame-listener
			  :reader output-damage-frame-listener)
   (output :type wlr:output
	   :initarg :output
	   :accessor output-output)))

(defun make-output (output)
  (let* ((output-damage (wlr:output-damage-create output))
	 (damage-frame-listener (make-listener handle-damage-frame)))
    (wl-signal-add (foreign-slot-pointer output-damage '(:struct wlr:output-damage)
					 :event-damage-frame)
		   damage-frame-listener)
    (let ((new-output (make-instance 'output
				     :output output
				     :output-damage output-damage
				     :damage-frame-listener damage-frame-listener)))
      (register-listener damage-frame-listener new-output *listener-hash*)
      new-output)))

(defun destroy-output (output)
  (declare (type output output))
  (let ((damage-listener (output-damage-frame-listener output)))
    (unregister-listener damage-listener *listener-hash*)
    (wl-list-remove (cffi:foreign-slot-pointer damage-listener
					       '(:struct wl_listener) 'link))
    (cffi:foreign-free damage-listener)))

(defcallback handle-damage-frame :void
    ((listener :pointer)
     (data (:pointer wlr:output)))
  (log-string :debug "Damage Frame"))
;; (let ((output (get-listener-owner listener *listener-hash*)))
