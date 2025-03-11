(in-package #:mahogany)

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

(defmacro init-callback-struct (variable type &body sets)
  (let ((vars (mapcar #'car sets)))
	`(cffi:with-foreign-slots (,vars ,variable ,type)
	   (setf ,@(loop for pair in sets
			 append (list (car pair) `(cffi:callback ,(cadr pair))))))))

(defun init-view-callbacks (view-callbacks)
  (init-callback-struct view-callbacks (:struct hrt:hrt-view-callbacks)
	(hrt:new-view handle-new-view-event)
	(hrt:view-destroyed handle-view-destroyed-event)))

(defun run-server (args)
  (disable-fpu-exceptions)
  (hrt:load-foreign-libraries)
  (log-init :level :trace)
  (enable-debugger)
  (if (cl-argparse:get-value "skip-init" args)
      (log-string :debug "Init file loading skipped")
      (load-config-file))
  (cffi:with-foreign-objects ((output-callbacks '(:struct hrt:hrt-output-callbacks))
			      (seat-callbacks '(:struct hrt:hrt-seat-callbacks))
			      (view-callbacks '(:struct hrt:hrt-view-callbacks))
			      (server '(:struct hrt:hrt-server)))
    (init-callback-struct output-callbacks (:struct hrt:hrt-output-callbacks)
      (hrt:output-added handle-new-output)
      (hrt:output-removed handle-output-removed)
      (hrt:output-layout-changed handle-output-layout-change))
    (init-callback-struct seat-callbacks (:struct hrt:hrt-seat-callbacks)
      (hrt:button-event handle-mouse-button-event)
      (hrt:wheel-event handle-mouse-wheel-event)
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

(defun %parse-cmd-line-args (args)
  (let ((parser (cl-argparse:create-main-parser (main-parser "Mahogany is a tiling window manager for Wayland modeled after StumpWM."
                                                             "mahogany")
                  (cl-argparse:add-flag main-parser
                                        :short "q" :long "no-init-file"
                                        :help "Do not load an init file on startup"
                                        :var "skip-init"))))
    (cl-argparse:parse parser args)))

(defun main ()
  (handler-case
      (let ((args (%parse-cmd-line-args (uiop:command-line-arguments))))
        (run-server args))
    (cl-argparse:cancel-parsing-error (e)
      (format t "~a~%" e))))
