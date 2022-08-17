(in-package #:mahogany)

(cffi:defcallback cursor-callback :void ((seat (:pointer (:struct hrt-seat))))
  (log-string :debug "cursor callback called"))

(cffi:defcallback output-callback :void ((seat (:pointer (:struct hrt-output))))
  (log-string :debug "output change callback called"))

(cffi:defcallback keyboard-callback :void
    ((seat (:pointer (:struct hrt-seat)))
     (info (:pointer (:struct hrt-keypress-info))))
  (log-string :debug "keyboard callback called"))

(pushnew #p"~/Programs/mahogany/build/lib64/" cffi:*foreign-library-directories* :test #'equal)

(cffi:define-foreign-library libheart
  (:unix "libheart.so"))

(cffi:define-foreign-library libwroots
  (:unix "libwlroots.so"))

(defun run-server ()
  (cffi:use-foreign-library libwroots)
  (cffi:use-foreign-library libheart)
  (log-init :level :trace)
  (cffi:with-foreign-objects ((output-callbacks '(:struct hrt-output-callbacks))
			      (seat-callbacks '(:struct hrt-seat-callbacks))
			      (server '(:struct hrt-server)))
    (cffi:with-foreign-slots ((output-added output-removed)
			      output-callbacks (:struct hrt-output-callbacks))
      (setf output-added (cffi:callback output-callback)
	    output-removed (cffi:callback output-callback)))
    (cffi:with-foreign-slots ((button-event wheel-event keyboard-keypress-event)
			      seat-callbacks (:struct hrt-seat-callbacks))
      (setf button-event (cffi:callback cursor-callback)
	   wheel-event (cffi:callback cursor-callback)
	   keyboard-keypress-event (cffi:callback keyboard-callback)))
    (hrt-server-init server output-callbacks seat-callbacks 3)
    (hrt-server-start server)
    (hrt-server-finish server)))
