;; this file contains the seat functions and the cursor functions

(in-package :mahogany/backend)

(defun make-cursor ()
  (let ((wlr-cursor (wlr:cursor-create))
	(xcursor-manager (wlr:xcursor-manager-create "default" 24)))
    ;; don't know if we need to call this on creation or not:
    (wlr:xcursor-manager-set-cursor-image xcursor-manager "left_ptr" wlr-cursor)
    (wlr:cursor-attach-output-layout wlr-cursor (output-layout (get-output-manager (get-server))))
    (make-instance 'cursor
		   :wlr-cursor wlr-cursor
		   :xcursor-manager xcursor-manager)))

(defun config-cursor-for-output (cursor output)
  (declare (type cursor cursor))
  (let ((xcursor-manager (cursor-xcursor-manager cursor))
	(wlr-cursor (cursor-wlr-cursor cursor)))
    (wlr:xcursor-manager-load xcursor-manager (foreign-slot-value (output-wlr-output output)
								  '(:struct wlr:output)
								  :scale))
    (wlr:xcursor-manager-set-cursor-image xcursor-manager "left_ptr" wlr-cursor)))

(defun seat-config-cursor-for-output (seat output)
  (config-cursor-for-output (seat-cursor seat) output))

(defun make-seat (display name)
  (let* ((wlr-seat (wlr:seat-create display name))
	(cursor (make-cursor)))
    (make-instance 'seat
		   :wlr-seat wlr-seat
		   :cursor cursor)))

(defun seat-name (seat)
  (foreign-string-to-lisp (foreign-slot-pointer (seat-wlr-seat seat)
						'(:struct wlr:seat)
						:name)))
