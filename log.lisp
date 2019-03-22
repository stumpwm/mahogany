;; an alternative to this package is vom. However, it doesn't
;; support color, and is unlikely to, so this will stay.
(defpackage #:mahogany/log
  (:use :cl #:cl-ansi-text)
  (:export #:*log-level*
	   #:log-string
	   #:get-print-data
	   #:log-init
	   #:with-log-level
	   #:with-log-color-enabled
	   #:with-logging-to-file
	   #:*log-output-file*))


(in-package #:mahogany/log)

(defvar *log-output-file* *standard-output*
  "The file to print log messages")

;; if you need to add more log levels, you may need to recompile, as
;; the level is translated to a number at read time. See log-string.
(defvar *log-level* :info
  "The amount of information printed to the *log-output-file*. The accepted values are:
:ignore Print nothing to stdout
:trace  this should be used for 'tracing' the code, such as when doing deep debugging.
:debug  Information that is diagnostically helpful to people who are not project developers
:info   Useful, general information that is shown by default
:warn   Will signal a warning condition with the supplied text as well as print to
        *log-output-file*. Use if something is wrong, but the app can still continue.
:error  Something went wrong...
:fatal  Better call the insurance company...")

;; log-string is used in this file, so get-print-data needs to
;; be availabe at compile time:
(eval-when (:compile-toplevel :load-toplevel)
  (defun get-print-data (level)
    ;; would probably be better to use a hashtable or plist
    (ccase level
      ;; higher values mean less importance
      (:trace  (values 6 :white))
      (:debug  (values 5 :cyan))
      (:info   (values 4 :blue))
      (:warn   (values 3 :yellow))
      (:error  (values 2 :red))
      (:fatal  (values 1 :red))
      (:ignore (values 0)))))

(defmacro log-string (log-lvl string &rest fmt)
  "Log the input to *log-output-file* based on the current value of *log-level*"
  (assert (keywordp log-lvl))
  (unless (eql :ignore log-lvl)
    (multiple-value-bind (lvl color)
	(get-print-data log-lvl)
      `(when (>= (get-print-data *log-level*) ,lvl)
	 (with-color (,color :effect :bright)
	   (format *log-output-file* ,string ,@fmt)
	   (format *log-output-file* "~%"))
	 (finish-output *log-output-file*)
	 ,(when (= lvl 3)
	    `(warn (format nil ,string ,@fmt)))))))

(defun term-colorable-p ()
  (and (interactive-stream-p *standard-input*)
       (member :max-colors (terminfo:capabilities
			    (terminfo:set-terminal (uiop:getenv "TERM"))))))

(defun check-valid-log-level (level)
  ;; TODO: make this something with a use-value restart?
  (assert (member level '(:trace :debug :info :warn :error :fatal))))

(defun log-debug-level ()
  *log-level*)

(defun (setf log-debug-level) (new-level)
  (check-valid-log-level new-level)
  (setf *log-level* new-level))

(defun log-colored ()
  cl-ansi-text:*enabled*)

(defun (setf log-colored) (enablep)
  (setf cl-ansi-text:*enabled* enablep))

(defun log-init (&key (level *log-level*) (output *standard-output*) (color t))
  "Initialize logging. Call this to setup colorized output, ect.
It is not necessary to call this for logging to work properly, but coloring may be messed up.
If *log-output-file* is changed, it is a good idea to call this function again.
  LVL:    see *log-level*
  OUTPUT: see *log-output-file*
  COLOR:  Enable/Disable logging colors. If colors are not supported by the output stream, then
          this argument will be ignored."
  (setf *log-output-file* output)
  (check-valid-log-level level)
  (setf *log-level* level)
  ;; check if we can use pretty colors:
  (if (and (term-colorable-p)
	   color)
      (setf cl-ansi-text:*enabled* t)
      (setf cl-ansi-text:*enabled* nil))
  (log-string :debug "Mahogany Log settings set to:~%~2TColor:~10T~:[FALSE~;TRUE~]~%~2TOutput:~10T~A~%~2TLevel:~10T~S"
	  cl-ansi-text:*enabled* *log-output-file* *log-level*))

(defmacro with-log-level (log-level &body body)
  `(progn
     (check-valid-log-level ,log-level)
     (let ((*log-level* ,log-level))
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
