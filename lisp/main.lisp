(in-package #:mahogany)

(cffi:defcallback cursor-callback :void ((seat (:pointer (:struct hrt-seat))))
  (declare (ignore seat))
  (log-string :trace "cursor callback called"))

(cffi:defcallback output-callback :void ((seat (:pointer (:struct hrt-output))))
  (declare (ignore seat))
  (log-string :trace "output change callback called"))

(cffi:defcallback keyboard-callback :bool
    ((seat (:pointer (:struct hrt-seat)))
     (info (:pointer (:struct hrt-keypress-info))))
  (cffi:with-foreign-slots ((keysyms modifiers keysyms-len) info (:struct hrt-keypress-info))
    (dotimes (i keysyms-len)
      (let ((key (make-key (cffi:mem-aref keysyms :uint32 i) modifiers)))
	(log-string :trace (lambda (s)
			     (print-key key)
			     (format s ": ~A~%"
				     (xkb:keysym-get-name (mahogany/keyboard::key-keysym key)))))
	(handle-key-event key seat)))))

(defun disable-fpu-exceptions ()
  #+sbcl
  (sb-int:set-floating-point-modes :traps nil)
  #+ccl
  (set-fpu-mode :overflow nil))

(defmacro init-callback-struct (variable type &body sets)
  (let ((vars (mapcar #'car sets)))
    `(cffi:with-foreign-slots (,vars ,variable ,type)
       (setf ,@(loop for pair in sets
		     append (list (car pair) `(cffi:callback ,(cadr pair))))))))

(defun run-server ()
  (disable-fpu-exceptions)
  (hrt:load-foreign-libraries)
  (log-init :level :trace)
  (cffi:with-foreign-objects ((output-callbacks '(:struct hrt-output-callbacks))
			      (seat-callbacks '(:struct hrt-seat-callbacks))
			      (server '(:struct hrt-server)))
    (init-callback-struct output-callbacks (:struct hrt-output-callbacks)
      (output-added output-callback)
      (output-removed  output-callback))
    (init-callback-struct seat-callbacks (:struct hrt-seat-callbacks)
      (button-event cursor-callback)
      (wheel-event cursor-callback)
      (keyboard-keypress-event keyboard-callback))
    (setf (mahogany-state-server *compositor-state*) server)
    (log-string :debug "Initialized mahogany state")
    (hrt-server-init server output-callbacks seat-callbacks 3)
    (log-string :debug "Initialized heart state")
    (unwind-protect
	 (hrt-server-start server)
      (log-string :debug "Cleaning up...")
      (server-stop *compositor-state*)
      (hrt-server-finish server)
      (server-state-reset *compositor-state*)
      (log-string :debug "Shutdown reached."))))
