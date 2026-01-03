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
          (log-string :info "Found config file at ~a" config-file)
          (if catch-errors
              (handler-case (load config-file)
                (error (c) (values nil (format nil "~a" c) config-file))
                (:no-error (&rest args) (declare (ignore args)) (values t nil config-file)))
              (progn
                (load config-file)
                (values t nil config-file))))
        (progn
          (log-string :info "Did not find config file")
          (values t nil nil)))))

(defmacro init-callback-struct (variable type &body sets)
  (let ((vars (mapcar #'car sets)))
    `(cffi:with-foreign-slots (,vars ,variable ,type)
       (setf ,@(loop for pair in sets
                     append (list (car pair)
                                  (if (cadr pair)
                                      `(cffi:callback ,(cadr pair))
                                      (cffi:null-pointer))))))))

(defun init-view-callbacks (view-callbacks)
  (init-callback-struct view-callbacks (:struct hrt:hrt-view-callbacks)
    (hrt:new-view handle-new-view-event)
    (hrt:request-maximize handle-view-maximize)
    (hrt:request-minimize handle-view-minimize)
    (hrt:view-mapped handle-view-mapped)
    (hrt:view-unmapped handle-view-unmapped)
    (hrt:view-destroyed handle-view-destroyed-event)
    (hrt:request-fullscreen handle-request-fullscreen)))

(defun run-server (args)
  (disable-fpu-exceptions)
  (hrt:load-foreign-libraries)
  (log-init :level (intern (gethash 'loglevel args) 'keyword))
  (enable-debugger)
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
    (if (gethash 'no-init-file args)
        (log-string :info "Init file loading skipped")
        (load-config-file))
    (unwind-protect
         (hrt:hrt-server-start server)
      (log-string :debug "Cleaning up...")
      (server-stop *compositor-state*)
      (server-state-reset *compositor-state*)
      (log-string :debug "Shutdown reached."))))

(alexandria:define-constant +LOGLEVEL-SELECTIONS+ '("TRACE" "DEBUG" "INFO" "WARN" "ERROR" "FATAL" "IGNORE") :test #'equal)

(defun %build-cmd-line-parser ()
  (let ((help-option (adopt:make-option
                      'help
                      :long "help"
                      :short #\h
                      :help "Display help and exit."
                      :reduce (constantly t)))
        (no-init-option (adopt:make-option
                         'no-init-file
                         :long "no-init-file"
                         :short #\q
                         :help "Do not evaulate an init file on startup"
                         :reduce (constantly t)))
        (loglevel (adopt:make-option
                   'loglevel
                   :long "loglevel"
                   :short #\l
                   :parameter "[loglevel]"
                   :initial-value (first +LOGLEVEL-SELECTIONS+)
                   :help (format nil "Specify loglevel ~{~A~^|~}" +LOGLEVEL-SELECTIONS+)
                   :reduce (lambda (prev level) (declare (ignore prev)) (string-upcase level)))))
    (adopt:make-interface
     :name "mahogany"
     :summary "Keyboard driven titling window manager for Wayland"
     :usage "[OPTIONS]"
     :help "Mahogany is a tiling window manager for Wayland modeled after StumpWM."
     :contents (list
                help-option
                no-init-option
                loglevel))))

(defun %parse-cmd-line-args (args)
  (let ((parser (%build-cmd-line-parser)))
    (handler-case
        (multiple-value-bind (unused found) (adopt:parse-options parser args)
          (declare (ignore unused))
          (cond
            ((gethash 'help found)
             (adopt:print-help-and-exit parser))
            ((not (member (gethash 'loglevel found) +LOGLEVEL-SELECTIONS+ :test #'equalp))
             (adopt:print-error-and-exit (format nil "invlid loglevel '~a'. it's must be one of ~{~A~^|~}" (gethash 'loglevel found) +LOGLEVEL-SELECTIONS+)))
            (t
             found)
            ))
      (adopt:unrecognized-option (e)
        (adopt:print-help parser)
        (adopt:print-error-and-exit e)))))

(defun main ()
  (let* ((args (%parse-cmd-line-args (uiop:command-line-arguments))))
    (run-server args)))
