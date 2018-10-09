(defpackage :mahogany/backend/input/seat
  (:use :cl :cffi :mahogany/backend/util)
  (:import-from :mahogany/backend/input/classes
		#:seat)
  (:import-from :mahogany/log
		#:log-string)
  (:import-from :wayland-server-core
		#:wl-signal-add
		#:wl-list-remove
		#:wl_listener
		#:link))

(in-package :mahogany/backend/input/seat)

(export '(make-seat
	  seat-name))

(defun make-seat (display name)
  (let ((wlr-seat (wlr:seat-create display name)))
    (make-instance 'seat :wlr-seat wlr-seat)))

(defun seat-name (seat)
  (foreign-string-to-lisp (foreign-slot-pointer (seat-wlr-seat seat) '(:struct wlr:seat)
						:name)))
