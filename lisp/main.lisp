(in-package #:mahogany)

(cffi:defcallback cursor-callback :void ((seat (:pointer (:struct hrt:hrt-seat))))
  (declare (ignore seat))
  (log-string :trace "cursor callback called"))

(cffi:defcallback keyboard-callback :bool
    ((seat (:pointer (:struct hrt:hrt-seat)))
     (info (:pointer (:struct hrt:hrt-keypress-info))))
  (cffi:with-foreign-slots ((hrt:keysyms hrt:modifiers hrt:keysyms-len hrt:wl-key-state)
			    info (:struct hrt:hrt-keypress-info))
    ;; I'm not sure why this is an array, but it's what tinywl does:
    (dotimes (i hrt:keysyms-len)
      (let ((key (make-key (cffi:mem-aref hrt:keysyms :uint32 i) hrt:modifiers)))
	(when (handle-key-event *compositor-state* key seat hrt:wl-key-state)
	  (return t))))))

(defmacro init-callback-struct (variable type &body sets)
  (let ((vars (mapcar #'car sets)))
    `(cffi:with-foreign-slots (,vars ,variable ,type)
       (setf ,@(loop for pair in sets
		     append (list (car pair) `(cffi:callback ,(cadr pair))))))))

(defun init-view-callbacks (view-callbacks)
  (init-callback-struct view-callbacks (:struct hrt:hrt-view-callbacks)
    (hrt:new-view handle-new-view-event)
    (hrt:view-destroyed handle-view-destroyed-event)))

(defun load-config-file (&optional (catch-errors nil))
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
  (enable-debugger)
  (load-config-file)
  (cffi:with-foreign-objects ((output-callbacks '(:struct hrt:hrt-output-callbacks))
			      (seat-callbacks '(:struct hrt:hrt-seat-callbacks))
			      (view-callbacks '(:struct hrt:hrt-view-callbacks))
			      (server '(:struct hrt:hrt-server)))
    (init-callback-struct output-callbacks (:struct hrt:hrt-output-callbacks)
      (hrt:output-added handle-new-output)
      (hrt:output-removed handle-output-removed)
      (hrt:output-layout-changed handle-output-layout-change))
    (init-callback-struct seat-callbacks (:struct hrt:hrt-seat-callbacks)
      (hrt:button-event cursor-callback)
      (hrt:wheel-event cursor-callback)
      (hrt:keyboard-keypress-event keyboard-callback))
    (init-view-callbacks view-callbacks)

    (server-state-init *compositor-state* server
                       output-callbacks seat-callbacks view-callbacks
                       :debug-level 3)
    (log-string :debug "Initialized mahogany state")
    (unwind-protect
	 (hrt:hrt-server-start server)
      (log-string :debug "Cleaning up...")
      (server-stop *compositor-state*)
      (server-state-reset *compositor-state*)
      (log-string :debug "Shutdown reached."))))
