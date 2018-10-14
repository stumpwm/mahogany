;; this file contains the seat functions and the cursor functions

(in-package :mahogany/backend)

(defcallback handle-cursor-motion :void
    ((listener :pointer)
     (event (:pointer (:struct wlr:event-pointer-motion))))
  (let ((cursor (get-listener-owner listener *listener-hash*)))
    (with-wlr-accessors ((device :device :pointer t)
			 (delta-x :delta-x)
			 (delta-y :delta-y))
	event (:struct wlr:event-pointer-motion)
      (wlr:cursor-move (cursor-wlr-cursor cursor) device delta-x delta-y))))

(defcallback handle-cursor-absolute-motion :void
    ((listener :pointer)
     (event (:pointer (:struct wlr:event-pointer-motion-absolute))))
  (let ((cursor (get-listener-owner listener *listener-hash*)))
    (with-wlr-accessors ((device :device :pointer t)
			 (new-x :x)
			 (new-y :y))
	event (:struct wlr:event-pointer-motion-absolute)
      (wlr:cursor-warp-absolute (cursor-wlr-cursor cursor) device new-x new-y))))

(defun make-cursor ()
  (let ((wlr-cursor (wlr:cursor-create))
	(xcursor-manager (wlr:xcursor-manager-create "default" 24))
	(motion-listener (make-listener handle-cursor-motion))
	(motion-absolute-listener (make-listener handle-cursor-absolute-motion)))
    ;; don't know if we need to call this on creation or not:
    (wlr:xcursor-manager-set-cursor-image xcursor-manager "left_ptr" wlr-cursor)
    (wlr:cursor-attach-output-layout wlr-cursor (output-layout (get-output-manager (get-server))))
    (with-wlr-accessors ((motion-event :event-motion :pointer t)
			 (m-absolute-event :event-motion-absolute :pointer t))
	wlr-cursor (:struct wlr:cursor)
      (wl-signal-add motion-event motion-listener)
      (wl-signal-add m-absolute-event motion-absolute-listener))
    (let ((new-cursor (make-instance 'cursor
				     :wlr-cursor wlr-cursor
				     :motion-listener motion-listener
				     :motion-absolute-listener motion-absolute-listener
				     :xcursor-manager xcursor-manager)))
      (register-listener motion-listener new-cursor *listener-hash*)
      (register-listener motion-absolute-listener new-cursor *listener-hash*)
      new-cursor)))

(defun destroy-cursor (cursor)
  (with-accessors ((absolute-listener cursor-motion-absolute-listener)
		   (motion-listener cursor-motion-listener)
		   (wlr-cursor cursor-wlr-cursor))
      cursor
    (cleanup-listener absolute-listener *listener-hash*)
    (cleanup-listener motion-listener *listener-hash*)
    (wlr:cursor-destroy wlr-cursor)))

(defun config-cursor-for-output (cursor output)
  (declare (type cursor cursor))
  (let ((xcursor-manager (cursor-xcursor-manager cursor))
	(wlr-cursor (cursor-wlr-cursor cursor)))
    (wlr:xcursor-manager-load xcursor-manager (output-scale output))
    (wlr:xcursor-manager-set-cursor-image xcursor-manager "left_ptr" wlr-cursor)))

(defun seat-config-cursor-for-output (seat output)
  (config-cursor-for-output (seat-cursor seat) output))

(defun make-seat (display name)
  (let* ((wlr-seat (wlr:seat-create display name))
	 (cursor (make-cursor))
         (new-seat (make-instance 'seat
				  :wlr-seat wlr-seat
				  :cursor cursor)))
    ;; make the seat aware of all of the outputs:
    (dolist (output (get-outputs (get-output-manager (get-server))))
      (seat-config-cursor-for-output new-seat output))
    new-seat))

(defun destroy-seat (seat)
  ;; TODO: finish this.
  (destroy-cursor (seat-cursor seat)))

(defun seat-name (seat)
  (foreign-string-to-lisp (foreign-slot-pointer (seat-wlr-seat seat)
						'(:struct wlr:seat)
						:name)))
