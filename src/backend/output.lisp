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
    (wl-signal-add (foreign-slot-pointer output '(:struct wlr:output)
					 :event-frame)
		   damage-frame-listener)
    (when (not (wl-list-empty (foreign-slot-pointer output '(:struct wlr:output)
    						    :modes)))
      (let ((modes (foreign-slot-value output '(:struct wlr:output) :modes)))
    	(let ((mode (container-of (getf modes 'prev) '(:struct wlr:output_mode) link)))
    	  (log-string :debug "Mode set to: ~A" mode)
    	  (wlr:output-set-mode output mode))))
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
     (data (:pointer (:struct wlr:output))))
  (declare (ignore data))
  (log-string :debug "Damage Frame")
  (let* ((output (get-listener-owner listener *listener-hash*))
	 (wlr-output (output-output output))
	 (renderer (wlr:backend-get-renderer (foreign-slot-pointer wlr-output '(:struct wlr:output)
								   :backend))))
    (wlr:output-make-current wlr-output (cffi:null-pointer))

    (with-foreign-array (color #(0.4 0.4 0.4 1.0) '(:array :float 4))
      (wlr:renderer-clear renderer color))
    (wlr:output-swap-buffers wlr-output (cffi:null-pointer)
			     (cffi:null-pointer))
    (wlr:renderer-end renderer)))
