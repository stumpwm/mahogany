(in-package :mahogany/backend)

(defclass client-manager ()
  ((wlr-xdg-shell :initarg :wlr-xdg-shell
		  :type wlr:xdg-shell
		  :reader client-manager-xdg-shell)
   (surfaces :initform ()
	     :accessor client-manager-surfaces)
   (new-surface-listener :initarg :new-surface-listener
			 :type wl_listener)
   (xdg-shell-destroy-listener :initarg :xdg-shell-destroy-listener
			       :type wl_listener)))

(defcallback view-destroy :void
    ((listener :pointer)
     (surface :pointer))
  (declare (ignore surface))
  (log-string :trace "View destroyed")
  (multiple-value-bind (view manager)
      (values-list (get-listener-owner listener *listener-hash*))
    (setf (client-manager-surfaces manager) (remove view
						    (client-manager-surfaces manager)))))

(defcallback view-map :void
    ((listener :pointer)
     (surface :pointer))
  (declare (ignore surface))
  (log-string :trace "View mapped")
  (let ((view (first (get-listener-owner listener *listener-hash*))))
    (setf (view-mapped view) t)
    (add-view (server-frontend (get-server)) view)))

(defcallback view-unmap :void
    ((listener :pointer)
     (surface :pointer))
  (declare (ignore surface))
  (log-string :trace "View unmapped")
  (let ((view (first (get-listener-owner listener *listener-hash*))))
    (setf (view-mapped view) t)
    (remove-view (server-frontend (get-server)) view)))

(defcallback handle-new-surface :void
    ((listener :pointer)
     (surface :pointer))
  (log-string :trace "New surface added")
  (let ((client-manager (get-listener-owner listener *listener-hash*))
	(destroy-listener (make-listener view-destroy))
	(map-listener (make-listener view-map))
	(unmap-listener (make-listener view-unmap)))
    (with-wlr-accessors ((destroy-signal :event-destroy :pointer t)
			 (map-signal :event-map :pointer t)
			 (unmap-signal :event-unmap :pointer t)
			 (role :role))
	surface (:struct wlr:xdg-surface)
      (unless (eq role :toplevel)
	(log-string :trace "Non-toplevel surface created")
	(return-from handle-new-surface))
      (wl-signal-add map-signal map-listener)
      (wl-signal-add unmap-signal unmap-listener)
      (wl-signal-add destroy-signal destroy-listener))
    (let ((new-view (make-instance 'xdg-view
				   :wlr-surface surface
				   :map-listener map-listener
				   :unmap-listener unmap-listener
				   :destroy-listener destroy-listener)))
      (register-listeners (list new-view client-manager) *listener-hash*
			  map-listener unmap-listener destroy-listener)
      (push new-view (client-manager-surfaces client-manager)))))

(defcallback handle-xdg-shell-destroy :void
    ((listener :pointer)
     (xdg-shell :pointer))
  (declare (ignore listener xdg-shell))
  (log-string :trace "xdg-shell destroyed"))

(defun make-client-manager (wl-display)
  (let ((surface-listener (make-listener handle-new-surface))
	(xdg-shell-destroy (make-listener handle-xdg-shell-destroy))
	(xdg-shell (wlr:xdg-shell-create wl-display)))
    (with-wlr-accessors ((new-surface-event :event-new-surface :pointer t)
			 (destroy-event :event-destroy :pointer t))
	xdg-shell (:struct wlr:xdg-shell)
      (wl-signal-add new-surface-event surface-listener)
      (wl-signal-add destroy-event xdg-shell-destroy))
    (let ((new-client-manager (make-instance 'client-manager
					     :wlr-xdg-shell xdg-shell
					     :new-surface-listener surface-listener
					     :xdg-shell-destroy-listener xdg-shell-destroy)))
      (register-listener xdg-shell-destroy new-client-manager *listener-hash*)
      (register-listener surface-listener new-client-manager *listener-hash*)
      new-client-manager)))

(defun destroy-client-manager (client-manager)
  (with-slots (wlr-xdg-shell surfaces new-surface-listener xdg-shell-destroy-listener)
      client-manager
    (cleanup-listener new-surface-listener *listener-hash*)
    (cleanup-listener xdg-shell-destroy-listener *listener-hash*)
    (log-string :debug "Client Manager Destroyed")))
