(defpackage #:mh/backend/simple
  (:use :cl :wayland-server-core :cffi #:backend/util :mh-log))

;; (defun get-listener-owner (listener table)
;;   (gethash (cffi:pointer-address listener) table))

;; (defun register-listener (listener owner table)
;;   (setf (gethash (cffi:pointer-address listener) table) owner))

;; (defun unregister-listener (listener table)
;;   (remhash (cffi:pointer-address  listener) table))

;; (defun remove-from-list (object place)
;;   (wl-list-remove (cffi:foreign-slot-pointer object
;; 					     '(:struct wl_listener) place)))
(in-package :mh/backend/simple)

(defvar *listener-hash* (make-hash-table))

(defstruct sample-state
  display
  backend)

(defstruct sample-output
  state
  output
  frame-listener
  destroy-listener)

(defvar *sample-state* nil)

(cffi:defcallback handle-new-input :void
      ((listener :pointer)
       (input :pointer))
  (declare (ignore listener input))
  (format t "new input device"))

(cffi:defcallback new-frame-notify :void
    ((listener :pointer)
     (output :pointer))
  (declare (ignore output))
  (let* ((output-owner (get-listener-owner listener *listener-hash*))
	 ;; (renderer (wlr:backend-get-renderer (sample-state-backend *sample-state*)))
	 (renderer (wlr:backend-get-renderer (foreign-slot-value (sample-output-output output-owner)
						       '(:struct wlr:output)
						       :backend))))
    (wlr:output-make-current (sample-output-output output-owner) (cffi:null-pointer))

    (with-foreign-array (color #(0.4 0.4 0.4 1.0) '(:array :float 4))
      (wlr:renderer-clear renderer color))
    (wlr:output-swap-buffers (sample-output-output output-owner) (cffi:null-pointer)
			     (cffi:null-pointer))
    (wlr:renderer-end renderer)
    ))

(cffi:defcallback destroy-output :void
    ((listener :pointer)
     (output :pointer))
  (declare (ignore listener output)))

(cffi:defcallback handle-new-output :void
    ((listener :pointer)
     (output (:pointer (:struct wlr:output))))
  (declare (ignore listener))
  (assert (not (cffi:null-pointer-p output)))
  (let ((frame-listener (make-listener new-frame-notify))
	(destroy-listener (make-listener destroy-output)))

    (log-string :debug "New output ~A~%" (foreign-slot-pointer output '(:struct wlr:output)
  							       :name))
    (assert (not (cffi:null-pointer-p frame-listener)))
    (assert (not (cffi:null-pointer-p destroy-listener)))
    (format t "Past print statement~%")
    (finish-output)
    (wayland-server-core:wl-signal-add (cffi:foreign-slot-pointer output
    								  '(:struct wlr:output)
    								  :event-frame)
    				       frame-listener)
    (wayland-server-core:wl-signal-add (cffi:foreign-slot-pointer output
    								  '(:struct wlr:output)
    								  :event-destroy)
    				       destroy-listener)
    (let ((new-output (make-sample-output :state *sample-state*
    					  :output output
    					  :frame-listener frame-listener
    					  :destroy-listener destroy-listener)))
      (register-listener destroy-listener new-output *listener-hash*)
      (register-listener frame-listener new-output *listener-hash*)))
  (format t "New output registered~%"))

(defvar *listener-hash* (make-hash-table))


;; (defvar *new-output-listener* (make-listener handle-new-input))


(defun main ()
  (cl-wlroots/util/log:log-init :log-debug (cffi:null-pointer))
  (let* ((display (wayland-server-core:wl-display-create))
	 (backend (wlr:backend-autocreate display (cffi:null-pointer)))
	 (renderer (wlr:backend-get-renderer backend))
	 (new-output-listener (make-listener handle-new-output))
	 (new-input-listener (make-listener handle-new-input)))
    (assert (not (cffi:null-pointer-p backend)))
    (assert (not (eql renderer (null-pointer))))
    (wlr:renderer-init-wl-display renderer display)

    (wayland-server-core:wl-signal-add (cffi:foreign-slot-pointer backend
								  '(:struct wlr:backend)
								  :event-new-input)
				       new-input-listener)
    (wayland-server-core:wl-signal-add (cffi:foreign-slot-pointer backend
								  '(:struct wlr:backend)
								  :event-new-output)
				       new-output-listener)
    (setf *sample-state* (make-sample-state :display display
					    :backend backend))
    (unless (wlr:backend-start backend)
      (format t "Failed to start backend")
      (wlr:backend-destroy backend)
      (uiop:quit 1))
    (wayland-server-core:wl-display-run display)
    (wayland-server-core:wl-display-destroy display)))
