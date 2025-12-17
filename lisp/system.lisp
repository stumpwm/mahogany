(defpackage #:mahogany/system
  (:documentation "Package for functions interacting with the system that
Mahogany is running under")
  (:use :cl #:mahogany/util)
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

(defvar *default-terminals*
  (vector (uiop:getenv "TERMINAL")
          "alacritty"
          "ghostty"
          "kitty"
          "xfce4-terminal"
          "konsole"
          "gnome-terminal"
          "wezterm"
          "foot")
  "A vector of default terminal programs to use.")

(defun open-terminal ()
  ;; TODO add log messages here for the case where no provided names work.
  (loop :for name :across *default-terminals*
		:do (alex:when-let ((program (find-program name)))
              (uiop:launch-program program)
              (return t)))
  (values nil))
