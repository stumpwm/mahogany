(in-package #:mahogany)

(defvar *initializing* nil
  "Use this variable in your config file to run code that should only
be executed once, when mahogany starts up and loads the config file.")

(defun xdg-state-home (&rest more)
    "Returns an absolute pathname for the directory containing user-specific application state files.
MORE may contain specifications for a subpath relative to this directory: a
subpathname specification and keyword arguments as per RESOLVE-LOCATION \(see
also \"Configuration DSL\"\) in the ASDF manual."
    (uiop::resolve-absolute-location
     `(,(or (uiop::getenv-absolute-directory "XDG_STATE_HOME")
            (uiop::os-cond
             ((uiop::os-windows-p) (get-folder-path :local-appdata))
             (t (uiop::subpathname (uiop::user-homedir-pathname) ".local/state/"))))
       ,more)))

(defstruct (loadrc-err
            (:constructor %make-loadrc-err
                (err-msg err-file rc-file backtrace timestamp)))
  (err-msg nil :type string :read-only t)
  (err-file nil :type pathname :read-only t)
  (rc-file nil :type pathname :read-only t)
  (backtrace nil :type string :read-only t)
  (timestamp nil :read-only t))

(defun %make-error-file-path (timestamp type)
  (let ((filename (format nil "~a_~a.txt"
                          (local-time:format-rfc3339-timestring nil timestamp)
                          type)))
    (xdg-state-home "mahogany" "log" "error"
                    filename)))

(defun %write-error-to-file (err-obj)
  (declare (type loadrc-err err-obj))
  (with-slots (err-msg err-file rc-file backtrace timestamp)
      err-obj
    (ensure-directories-exist err-file)
    (with-open-file (file err-file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
      (format file "~A: Loading rc file ~A~%"
              (local-time:format-rfc3339-timestring nil timestamp)
              rc-file)
      (format file "~%~A~%~%" err-msg)
      (format file "Backtrace:~%~%~A" backtrace))))

(defun %handle-init-config-error-timer (timer)
  (declare (type hrt:timer-handle timer))
  (let* ((err-obj (hrt:timer-handle-data timer))
         (err-msg (loadrc-err-err-msg err-obj))
         (err-file (loadrc-err-err-file err-obj))
         (path (make-pathname :directory (pathname-directory err-file)))
         (msg (format nil "Error loading config: ~%~a~%Full error in ~A"
                      ;; TODO: find a smart method to pick the truncation limit:
                      (if (> (length err-msg) 40)
                          (concatenate 'string (subseq err-msg 0 40) "...")
                          err-msg)
                      path)))
    (declare (type loadrc-err err-obj))
    (toast-message *compositor-state* msg
                   :ms-delay 5000
                   :theme *message-error-theme*))
  (hrt:timer-handle-destroy timer))

(defun make-loadrc-err (rc-file condition timestamp)
  (let ((err-msg (format nil "~a" condition))
        (err-file (%make-error-file-path timestamp "loadrc"))
        (backtrace (with-output-to-string (s)
                     (trivial-backtrace:print-backtrace-to-stream s))))
    (%make-loadrc-err err-msg err-file rc-file backtrace timestamp)))

(defun %make-log-loadrc-error-handler (config-file)
  (lambda (c)
    (let* ((timestamp (local-time:now))
           (err-obj (make-loadrc-err config-file c timestamp))
           (timer (hrt:server-make-timer (state-server *compositor-state*)
                                         #'%handle-init-config-error-timer)))
      (%write-error-to-file err-obj)
      (setf (hrt:timer-handle-data timer) err-obj)
      (hrt:timer-handle-update timer 5)
      (invoke-restart 'ignore-loadrc-error))))

(defun load-config-file (&optional (catch-errors nil))
  "Load the user's config file. Returns a values list: whether the file loaded (t if no
rc files exist), and the config file that was loaded. When CATCH-ERRORS is nil,
errors are left to be handled further up. "
  (let* ((config-location (uiop:xdg-config-home "mahogany" "init.lisp"))
         (config-file (probe-file config-location)))
    (if config-file
        (progn
          (log-string :info "Found config file at ~a" config-file)
          (restart-case
              (if catch-errors
                  (handler-bind ((error (%make-log-loadrc-error-handler config-file)))
                    (load config-file)
                    (values t config-file))
                  (progn
                    (load config-file)
                    (values t config-file)))
            (ignore-loadrc-error ()
              (values nil config-file))))
        (progn
          (log-string :info "Did not find config file at ~S" config-location)
          (values t nil nil)))))

(defcommand loadrc ()
  (:documentation "Reload the config file")
  (:method ()
    (multiple-value-bind (success file) (load-config-file t)
      (when success
        (if file
            (toast-message *compositor-state* "Config file loaded successfully.")
            (toast-message *compositor-state* "Config file not found"))))))
