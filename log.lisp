(defpackage #:mh-log
  (:use :cl :cl-ansi-text))

(in-package #:mh-log)

(export '(*log-level*
	  mh-log
	  mh-log-init))

(defvar *log-output-file* *standard-output*)

;; if you need to add more log levels, you may need to recompile, as
;; the level is translated to a number at read time. See mh-log:mh-log
(defvar *log-level* :info
  "The amount of information printed to the *log-output-file*. The accepted values are:
:ignore Print nothing to stdout
:trace  this should be used for 'tracing' the code, such as when doing deep debugging.
:debug  Information that is diagnostically helpful to people more than just the developers
:info   Useful, general information that is shown by default
:warn   Will signal a warning condition with the supplied text as well as print to
        *log-output-file*. Use if something is wrong, but the app can still continue.
:error  Something went wrong...
:fatal  Better call the insurance company...")

;; mh-log is used in this file, so get-print-data needs to
;; be availabe at compile time:
(eval-when (:compile-toplevel)
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

(defmacro mh-log (log-lvl string &rest fmt)
  "Log the input to *log-output-file* based on the current value of *log-level*"
  (unless (eql :ignore log-lvl)
    (multiple-value-bind (lvl color)
      (get-print-data log-lvl)
      `(progn
	 (when (>= (get-print-data *log-level*) ,lvl)
	   (with-color (,color :effect :bright)
	     (format *log-output-file* ,string ,@fmt)
	     (format *log-output-file* "~%"))
	   (finish-output *log-output-file*))
	 ,(if (= lvl 3)
	      `(warn (format nil ,string ,@fmt)))))))


;; as of August 2018, the terminfo package doesn't support 32 bit terminfo files,
;; so we must try to use the old versions:
;; TODO: update for new a new version of terminfo
(defun fix-term-name (name)
  (cl-ppcre:register-groups-bind (fixed) ("([a-z]*)-*" name)
    (or fixed name)))

(defun term-colorable-p ()
  (and (interactive-stream-p *standard-input*)
       (member :max-colors (terminfo:capabilities
			    (terminfo:set-terminal (fix-term-name (uiop:getenv "TERM")))))))

(defun mh-log-init (&key (level :info) (output *standard-output*) (color t))
  "Initialize logging. Call this to setup colorized output, ect.
It is not necessary to call this for logging to work properly, but coloring may be messed up.
  LVL:    see *log-level*
  OUTPUT: see *log-output-file*
  COLOR:  Enable/Disable logging colors"
  (setf *log-output-file* output)
  ;; TODO: make this something with a use-value restart
  (assert (member level '(:trace :debug :info :warn :error :crit)))
  (setf *log-level* level)
  ;; check if we can use pretty colors:
  (if (and (term-colorable-p)
	   color)
      (setf cl-ansi-text:*enabled* t)
      (setf cl-ansi-text:*enabled* nil))
  (mh-log :info "Log settings set to:~%~2TColor:~10T~:[FALSE~;TRUE~]~%~2TOutput:~10T~A~%~2TLevel:~10T~S"
	  cl-ansi-text:*enabled* *log-output-file* *log-level*))
