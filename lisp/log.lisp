;; an alternative to this package is vom. However, it doesn't
;; support color, and is unlikely to, so this will stay.
(defpackage #:mahogany/log
  (:use :cl #:cl-ansi-text)
  (:export #:log-level
	   #:log-colored-p
	   #:log-string
	   #:log-stream
	   #:log-init
	   #:with-log-level
	   #:with-log-color-enabled
	   #:with-logging-to-file
	   #:*log-output-file*)
  (:local-nicknames (#:alex #:alexandria)))


(in-package #:mahogany/log)

(deftype debug-specifier ()
  '(member :trace :debug :info :warn :error :fatal :ignore))

(defvar *log-output-file* *standard-output*
  "The file to print log messages")
(declaim (type stream *log-output-file*))

;; log-string is used in this file, so get-print-data needs to
;; be availabe at compile time:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-log-level-data (level)
    (the fixnum (ecase level
		  ;; higher values mean less importance
		  (:trace  (values 6 :white))
		  (:debug  (values 5 :cyan))
		  (:info   (values 4 :blue))
		  (:warn   (values 3 :yellow))
		  (:error  (values 2 :red))
		  (:fatal  (values 1 :red))
		  (:ignore (values 0))))))

;; if you need to add more log levels, you may need to recompile, as
;; the level is translated to a number at read time. See log-string.
(defvar *log-level* (get-log-level-data :info))
(declaim (type (integer 0 6) *log-level*))

(defun readable-log-level (level)
  (ecase level
    (6 (values :trace :white))
    (5  (values :debug :cyan))
    (4  (values :info :blue))
    (3  (values :warn :yellow))
    (2  (values :error :red))
    (1  (values :fatal :red))
    (0 (values :ignore))))

(defun %log-stream (lvl color stream-fn)
  (declare (optimize speed)
	   (type fixnum lvl)
	   (type (function (stream) (values &optional)) stream-fn))
  (when (>= *log-level* lvl)
    (let ((output *log-output-file*))
      (with-color (color :effect :bright :stream output)
	(funcall stream-fn output))
      (finish-output output))))

(defun log-stream (log-lvl stream-fn)
  "Call the given function with *log-output-file* as its argument if the
 log level allows for logging"
  (declare (type debug-specifier log-lvl)
	   (type (function (stream) (values &optional)) stream-fn))
  (unless (eql :ignore log-lvl)
    (multiple-value-bind (lvl color) (get-log-level-data log-lvl)
      (%log-stream lvl color stream-fn))))

(define-compiler-macro log-stream (&whole form log-lvl stream-fn)
  (if (constantp log-lvl)
      (unless (eql :ignore log-lvl)
	(multiple-value-bind (lvl color) (get-log-level-data log-lvl)
	  `(%log-stream ,lvl ,color ,stream-fn)))
      (progn
	(alex:simple-style-warning
	 "Missed optimization in log-stream: the log level is not specificed as a constant")
	form)))

(defmacro log-string (log-lvl string &rest fmt)
  "Log the input to *log-output-file* based on the current value of *log-level*.
The string argument as well as the format args will not be evaluated if the current log
level is not high enough."
  `(log-stream ,log-lvl (lambda (s)
			  (declare (type stream s))
			  (format s ,string ,@fmt) (format s "~%")
			  (values))))

(defun term-colorable-p ()
  (and (interactive-stream-p *standard-input*)
       (member :max-colors (terminfo:capabilities
			    (terminfo:set-terminal (uiop:getenv "TERM"))))))

(defun check-valid-log-level (level)
  ;; TODO: make this something with a use-value restart?
  (check-type level debug-specifier))

(defun log-level ()
  (values (readable-log-level *log-level*)))

(defun (setf log-level) (new-level)
  "The amount of information printed when logging to the output file. The accepted values are:
:ignore Print nothing to stdout
:trace  this should be used for 'tracing' the code, such as when doing deep debugging.
:debug  Information that is diagnostically helpful to people who are not project developers
:info   Useful, general information that is shown by default
:warn   Will signal a warning condition with the supplied text as well as print to
        *log-output-file*. Use if something is wrong, but the app can still continue.
:error  Something went wrong...
:fatal  Bye bye compositor..."
  (check-valid-log-level new-level)
  (setf *log-level* (get-log-level-data new-level)))

(defun log-colored-p ()
  cl-ansi-text:*enabled*)

(defun (setf log-colored) (enablep)
  (setf cl-ansi-text:*enabled* enablep))

(defun log-init (&key (level *log-level*) (output *standard-output*) (color t))
  "Initialize logging. Call this to setup colorized output, ect.
It is not necessary to call this for logging to work properly, but coloring may be messed up.
If *log-output-file* is changed, it is a good idea to call this function again.
  LVL:    see *log-level*
  OUTPUT: see (setf log-level)
  COLOR:  Enable/Disable logging colors. If colors are not supported by the output stream, then
          this argument will be ignored."
  (setf *log-output-file* output)
  (check-valid-log-level level)
  (setf (log-level) level)
  ;; check if we can use pretty colors:
  (if (and (term-colorable-p)
	   color)
      (setf cl-ansi-text:*enabled* t)
      (setf cl-ansi-text:*enabled* nil))
  (log-string :debug "Mahogany Log settings set to:~%~2TColor:~10T~:[FALSE~;TRUE~]~%~2TOutput:~10T~A~%~2TLevel:~10T~S"
	  cl-ansi-text:*enabled* *log-output-file* (log-level)))

(defmacro with-log-level (log-level &body body)
  `(progn
     (check-valid-log-level ,log-level)
     (let ((*log-level* ,(get-log-level-data log-level)))
       ,@body)))

(defmacro with-log-color-enabled (enabledp &body body)
  `(let ((cl-ansi-text:*enabled* ,enabledp))
    ,@body))

(defmacro with-logging-to-file ((file-path log-level &rest options) &body body)
  (let ((file-var (gensym "LOG-FILE")))
    `(with-open-file (,file-var ,file-path ,@options)
       (let ((*log-output-file* ,file-var))
	 (with-log-level ,log-level
	   (with-log-color-enabled nil
	     ,@body))))))
