(defpackage #:mahogany/system
  (:documentation "Package for functions interacting with the system that
Mahogany is running under")
  (:use :cl #:mahogany/util #:mahogany/log)
  (:local-nicknames (#:alex #:alexandria))
  (:nicknames #:sys)
  (:export #:find-program
	   #:open-terminal))

(in-package #:mahogany/system)

(defun find-program (name)
  (declare (type string name))
  (handler-case
      (string-trim '(#\Newline #\Space)
		   (with-output-to-string (stream)
		     (uiop:run-program (list "which" name) :output stream)))
    (UIOP/RUN-PROGRAM:SUBPROCESS-ERROR nil)))

(defun open-program (candidates)
  (loop :for name :in candidates
		:do (alex:if-let ((program (find-program name)))
              (progn
                (uiop:launch-program program)
                (return t))
              (log-string :warn "Could not find program ~S in candidates ~S." name candidates)))
  (values nil))

(config-system:defconfig *default-terminals*
  (list "alacritty" "ghostty" "kitty" "xfce4-terminal"
        "konsole" "gnome-terminal" "wezterm" "foot")
  list
  "A list of default terminal programs to use")

(defun open-terminal ()
  (open-program *default-terminals*))
