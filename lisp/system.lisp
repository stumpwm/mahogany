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

(defun open-terminal ()
  (if-let* ((term (uiop:getenv "TERMINAL"))
			(prog-path (find-program term)))
      (uiop:launch-program prog-path)
	(let ((programs #("konsole" "gnome-terminal" "wezterm" "foot")))
      (loop for i across programs
			do (alex:when-let ((program (find-program i)))
				 (uiop:launch-program program)
				 (return t)))
      (values nil))))
