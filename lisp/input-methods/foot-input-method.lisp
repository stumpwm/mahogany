(in-package #:mahogany)

(defclass foot-input-method (cl-interactive:input-method) ())

(defmethod cl-interactive:prepare-completions-for-input-method
    ((im foot-input-method) (completions cl-interactive:database))
  (cl-interactive:database-strings completions))

(defmethod cl-interactive:prepare-completions-for-input-method
    ((im foot-input-method) (completions list))
  (if (every #'stringp completions)
      completions
      (error 'cl-interactive:unknown-completions-error
	     :input-method im
	     :completions completions)))

(defmethod cl-interactive:input-method-read ((im foot-input-method)
					     (prompt string)
					     &key completions require-match
					       initial-input history)
  (declare (ignore initial-input history))
  (multiple-value-bind (read-fd write-fd)
      (sb-unix:unix-pipe)
    (unwind-protect
	 (let ((curpid (sb-unix:unix-getpid))
	       (read (sb-sys:make-fd-stream read-fd :input t))
	       (write (sb-sys:make-fd-stream write-fd :output t)))
	   (flet ((fzf-foot ()
		    (multiple-value-bind (subrfd subwfd)
			(sb-unix:unix-pipe)
		      (let ((write (sb-sys:make-fd-stream subwfd :output t)))
			(loop for comp in completions
			      do (write-line comp write)
				 (write-line comp))
			(force-output write)
			(sb-unix:unix-close subwfd)
			(format t "Launching ~A" (list "foot" "--" "sh" "-c"
      (format nil "fzf --print-query --prompt=~S < /proc/~D/fd/~D | tail -1 > /proc/~D/fd/~D"
	      prompt curpid subrfd curpid write-fd)))
			(uiop:launch-program
			 (list "foot" "--" "sh" "-c"
			       (format nil "fzf --print-query --prompt=~S < /proc/~D/fd/~D | tail -1 > /proc/~D/fd/~D"
				       prompt curpid subrfd curpid write-fd))))
		      (unwind-protect (read-line read)
			(sb-unix:unix-close subrfd)))))
	     (let ((pset nil)
		   (res (fzf-foot)))
	       (tagbody
		start
		  (if require-match
		      (if (find res completions :test #'string=)
			  (return-from cl-interactive:input-method-read res)
			  (progn
			    (psetf pset t
				   prompt (if pset
					      prompt
					      (concatenate 'string
							   "[Invalid Entry] "
							   prompt)))
			    (setf res (fzf-foot))
			    (go start)))
		      (return-from cl-interactive:input-method-read res))))))
      (sb-unix:unix-close write-fd)
      (sb-unix:unix-close read-fd))))
