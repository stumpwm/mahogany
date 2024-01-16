(in-package #:mahogany)

(cffi:defcallback cursor-callback :void ((seat (:pointer (:struct hrt-seat))))
  (declare (ignore seat))
  (log-string :trace "cursor callback called"))

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

(defun init-view-callbacks (view-callbacks)
  (init-callback-struct view-callbacks (:struct hrt-view-callbacks)
    (new-view handle-new-view-event)
    (view-destroyed handle-view-destroyed-event)))

(defun load-config-file (&optional (catch-errors t))
  "Load the user's config file. Returns a values list: whether the file loaded (t if no
rc files exist), the error if it didn't, and the config file that was
loaded. When CATCH-ERRORS is nil, errors are left to be handled
further up. "
  (let* ((xdg-config
           (probe-file (merge-pathnames #p"mahogany/init.lisp" (uiop:xdg-config-home))))
	 (fallback-config
	   (probe-file (merge-pathnames #p".config/mahogany/init.lisp" (user-homedir-pathname))))
	 (config-file (or xdg-config fallback-config)))
    (if config-file
	(progn
	  (log-string :debug "Found config file at ~a" config-file)
          (if catch-errors
	      (handler-case (load config-file)
		(error (c) (values nil (format nil "~a" c) config-file))
		(:no-error (&rest args) (declare (ignore args)) (values t nil config-file)))
              (progn
		(load config-file)
		(values t nil config-file))))
	(progn
	  (log-string :debug "Did not find config file")
	  (values t nil nil)))))

(defun run-server ()
  (disable-fpu-exceptions)
  (hrt:load-foreign-libraries)
  (log-init :level :trace)
  (sb-ext:enable-debugger)
  (load-config-file)
  (cffi:with-foreign-objects ((output-callbacks '(:struct hrt-output-callbacks))
			      (seat-callbacks '(:struct hrt-seat-callbacks))
			      (view-callbacks '(:struct hrt-view-callbacks))
			      (server '(:struct hrt-server)))
    (init-callback-struct output-callbacks (:struct hrt-output-callbacks)
      (output-added handle-new-output)
      (output-removed handle-output-removed)
      (output-mode-changed output-mode-change-callback))
    (init-callback-struct seat-callbacks (:struct hrt-seat-callbacks)
      (button-event cursor-callback)
      (wheel-event cursor-callback)
      (keyboard-keypress-event keyboard-callback))
    (init-view-callbacks view-callbacks)

    (setf (mahogany-state-server *compositor-state*) server)
    (log-string :debug "Initialized mahogany state")
    (hrt-server-init server output-callbacks seat-callbacks view-callbacks 3)
    (log-string :debug "Initialized heart state")
    (unwind-protect
	 (hrt-server-start server)
      (log-string :debug "Cleaning up...")
      (server-stop *compositor-state*)
      (hrt-server-finish server)
      (server-state-reset *compositor-state*)
      (log-string :debug "Shutdown reached."))))
