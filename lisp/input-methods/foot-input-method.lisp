(in-package #:mahogany)

(defclass foot-input-method (cl-interactive:input-method) ()
  (:documentation "Input method that uses foot and fzf for interactive input"))

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

(defmacro with-unix-pipe ((read-spec write-spec) &body body)
  (let (read-stream
        write-stream
        read-var
        write-var)
    (macrolet ((set-vars (spec stream-var var-symb)
                 `(if (listp ,spec)
                      (destructuring-bind (var &optional type)
                          ,spec
                        (ecase type
                          (:stream
                           (setf ,var-symb (gensym (symbol-name var))
                                 ,stream-var var))
                          ((or :fd nil)
                           (setf ,var-symb var))))
                      (setf ,var-symb ,spec))))
      (set-vars read-spec read-stream read-var)
      (set-vars write-spec write-stream write-var))
    `(multiple-value-bind (,read-var ,write-var)
         (sb-unix:unix-pipe)
       (let (,@(when read-stream
                 (list `(,read-stream (sb-sys:make-fd-stream ,read-var :input t))))
             ,@(when write-stream
                 (list `(,write-stream (sb-sys:make-fd-stream ,write-var :output t)))))
       (unwind-protect
            (progn
              ,@body)
         ,@(if read-stream
               (list `(close ,read-stream))
               (list `(sb-unix:unix-close ,read-var)))
         ,@(if write-stream
               (list `(close ,write-stream))
               (list `(sb-unix:unix-close ,write-var)
         )))))))

(defmethod cl-interactive:input-method-read ((im foot-input-method)
					     (prompt string)
					     &key completions require-match
					       initial-input history)
  (declare (ignore initial-input history))
  (flet ((fzf-foot ()
           (let ((curpid (sb-unix:unix-getpid)))
             (with-unix-pipe ((read :stream) write-fd)
		       (with-unix-pipe (subrfd (write :stream))
			     (loop for comp in completions
			           do (write-line comp write)
				          (write-line comp))
			     (force-output write)
			     (close write)
                 (let ((cmd (list "foot" "--" "sh" "-c"
			                      (format nil "fzf --print-query --prompt=~S < /proc/~D/fd/~D | tail -1 > /proc/~D/fd/~D"
				                          prompt curpid subrfd curpid write-fd))))
			       (log-string :debug "Launching ~A" cmd)
			       (uiop:launch-program cmd))
		         (read-line read))))))
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
