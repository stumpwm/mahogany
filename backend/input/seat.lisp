(in-package :mahogany/backend)

(defun make-seat (display name)
  (let ((wlr-seat (wlr:seat-create display name)))
    (make-instance 'seat :wlr-seat wlr-seat)))

(defun seat-name (seat)
  (foreign-string-to-lisp (foreign-slot-pointer (seat-wlr-seat seat) '(:struct wlr:seat)
						:name)))
