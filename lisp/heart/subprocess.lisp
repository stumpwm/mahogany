(in-package #:hrt)

(defstruct (collect-output-process
			(:constructor make-collect-output-process (process pid stdout-result stderr-result callback))
			(:copier nil))
  ;; the process-info type is documented, but it's not external.
  ;; should we really be using it?
  (process nil :type uiop/launch-program::process-info :read-only t)
  (pid 0 :type (unsigned-byte 64) :read-only t)
  (stdout-result nil :type string-stream :read-only t)
  (stderr-result nil :type string-stream :read-only t)
  (callback nil :type (function (collect-output-result) null) :read-only t))

(defun collect-app-output-begin (args callback stdin)
  (let* ((process (uiop:launch-program args
                                       :output :stream
                                       :error-output :stream
                                       :input (when stdin
                                                  :stream)))
		 (pid (uiop:process-info-pid process))
		 (stdout-result (make-string-output-stream))
         (stderr-result (make-string-output-stream)))
    (when stdin
      (let ((stdin-stream (uiop:process-info-input process)))
        (write-string stdin stdin-stream)
        (close stdin-stream)))
	(make-collect-output-process process pid stdout-result
                                 stderr-result
                                 callback)))

(defun %collect-app-output (output-stream result-stream)
  ;; FIXME: Should there be a limit of the number of bytes read at one time
  ;; so a noisy client doesn't block the server?
  (declare (optimize (speed 3) (safety 0)))
  (loop
	;; There doesn't seem to be a good way to do bulk
    ;; non-blocking input portably, so just read character-by-character
    ;; Hopefully it's buffered somewhere so we aren't wasting too many
    ;; CPU cycles:
	:for char = (read-char-no-hang output-stream nil)
	;; Loop continues as long as some bytes are read
	:while char
	:do
	   (write-char char result-stream)))

(defun collect-app-slurp-stdout (info)
  (declare (type collect-output-process info))
  (with-accessors ((process collect-output-process-process)
				   (stdout-result collect-output-process-stdout-result)
				   (stderr-result collect-output-process-stderr-result))
	  info
	(%collect-app-output (uiop:process-info-output process) stdout-result)))

(defun collect-app-slurp-stderr (info)
  (declare (type collect-output-process info))
  (with-accessors ((process collect-output-process-process)
				   (stderr-result collect-output-process-stderr-result))
	  info
	(%collect-app-output (uiop:process-info-error-output process)
						 stderr-result)))

(defstruct collect-output-result
  (stdout nil :type string)
  (stderr nil :type string)
  ;; The exit code may not always be available:
  ;; because we are just looking at stdout, we stop looking when stdout is closed,
  ;; not when the process finishes. If the process hasn't
  ;; finished by the time the cleanup code runs, we don't
  ;; get an exit code
  (exit-code nil :type (or null number)))

(defun collect-app-output-end (info)
  (declare (type collect-output-process info))
  (with-accessors ((stdout collect-output-process-stdout-result)
				   (stderr collect-output-process-stderr-result)
				   (process collect-output-process-process))
	  info
	(let ((stdout-result (get-output-stream-string stdout))
		  (stderr-result  (get-output-stream-string stderr))
		  (exit-code nil))
	  ;; If we wanted to follow the UIOP launch-program interface to the
	  ;; letter, we would unconditionally call uiop:wait-process somewhere
	  ;; to ensure we get an exit code and make sure the process doesn't
	  ;; leave behind resources from the OS. However, it only exposes
	  ;; a blocking interface, which we can't use. Thankfully, uiop just
	  ;; calls the implementation's version, and SBCL does the
	  ;; appropriate cleanup by listening to the SIGCHLD signal. That's
	  ;; also why I'm hesitant to add our own SIGCHLD handler, as it would
	  ;; most likely overwrite whatever the implementation does.
	  ;; I can't find what CCL does, but it's not leaving any orphaned
	  ;; processes behind, so it's probably not an issue?
	  (when (not (uiop:process-alive-p process))
		(setf exit-code (uiop:wait-process process)))
	  (make-collect-output-result :stdout stdout-result
								  :stderr stderr-result
								  :exit-code exit-code))))

(defun collect-app-output-cleanup (info)
  (with-accessors ((stdout collect-output-process-stdout-result)
				   (stderr collect-output-process-stderr-result)
				   (process collect-output-process-process))
	  info
	(uiop:close-streams process)
	(close stdout)
	(close stderr)))

(mahogany/util:defglobal *subprocesses* (make-hash-table))

(cffi:defcstruct app-output-data
  ;; Note: PIDs are usually ints, but use a long to give us some headroom:
  (pid :long)
  (stdout-source :pointer)
  (stderr-source :pointer))

(cffi:defcallback handle-app-stderr :int
    ((fd :int)
     (mask :uint32)
     (data :pointer))
  (declare (ignore fd))
  (cffi:with-foreign-slots ((pid stderr-source) data
                            (:struct app-output-data))
    (alexandria:when-let ((info (gethash pid *subprocesses*)))
      (when (plusp (logand mask +hrt-event-readable+))
        (collect-app-slurp-stderr info)))
    ;; Even if the info object isn't here anymore, we still need to
    ;; do some cleanup:
    (when (or (plusp (logand mask +hrt-event-hangup+))
              (plusp (logand mask +hrt-event-error+)))
      ;; let the stdout handler deal with cleanup, stderr
      ;; being done shouldn't stop stdin from being read.
      ;; Wait until that is done to do the cleanup for stderr too.
      (hrt-event-loop-remove stderr-source)))
  0)

(cffi:defcallback handle-app-stdout :int
    ((fd :int)
     (mask :uint32)
     (data :pointer))
  (declare (ignore fd))
  (cffi:with-foreign-slots ((pid stdout-source) data
                            (:struct app-output-data))
    (let ((info (gethash pid *subprocesses*)))
      (when (plusp (logand mask +hrt-event-readable+))
        (collect-app-slurp-stdout info))
      (when (or (plusp (logand mask +hrt-event-hangup+))
                (plusp (logand mask +hrt-event-error+)))
        (hrt-event-loop-remove stdout-source)
        (remhash pid *subprocesses*)
        (unwind-protect
             (let ((result (collect-app-output-end info)))
               (funcall (collect-output-process-callback info) result))
          (collect-app-output-cleanup info)
          (cffi:foreign-free data)))))
  0)

(declaim (inline extract-fd-from-stream))
(defun extract-fd-from-stream (stream)
  #+sbcl
  (sb-posix:file-descriptor stream)
  #+ccl
  (ccl:stream-device stream :output)
  #-(or sbcl ccl)
  (error "Not Implemented"))

(declaim (inline extract-stdout-fd))
(defun extract-stdout-fd (info)
  (declare (type collect-output-process info))
  (extract-fd-from-stream
   (uiop:process-info-output
    (collect-output-process-process info))))

(declaim (inline extract-stderr-fd))
(defun extract-stderr-fd (info)
  (declare (type collect-output-process info))
  (extract-fd-from-stream
   (uiop:process-info-error-output
    (collect-output-process-process info))))

(defun subprocess-collect (server args callback &optional stdin)
  "Start a subprocess with args ARGS and call CALLBACK with when it has finished
writing to stdout.

SERVER: a hrt-server object
ARGS:   A list of program arguments or the name of program; \"ls\"
        or (list \"ls\" \"/\"
CALLBACK: A function that takes a COLLECT-OUTPUT-RESULT object as an argument
STDIN: A string to pass to the standard input of the command"
  (declare (type (or null string) stdin))
  (let ((info (collect-app-output-begin args callback stdin))
        data)
    (handler-case
        (let ((stdout-fd (extract-stdout-fd info))
              (stderr-fd (extract-stderr-fd info)))
          (setf data (foreign-struct-create ((:struct app-output-data))
                                            (pid (collect-output-process-pid info))))
          (setf (gethash (collect-output-process-pid info) *subprocesses*)
                info)
          (cffi:with-foreign-slots ((stdout-source stderr-source) data (:struct app-output-data))
            (setf stdout-source (hrt-event-loop-add-fd
                                 server stdout-fd
                                 +hrt-event-readable+
                                 (cffi:callback handle-app-stdout)
                                 data)
                  stderr-source (hrt-event-loop-add-fd
                                 server stderr-fd
                                 +hrt-event-readable+
                                 (cffi:callback handle-app-stderr)
                                 data))))

      (t ()
        ;; I'm not entirely sure how to cleanup here. We certainly need to
        ;; free the data we allocated and close the streams, but do we really
        ;; need to terminate the process?
        ;; This gets handled in normal exectuion by handle-app-output:
        (uiop:terminate-process (collect-output-process-process info))
        (when data
          (cffi:foreign-free data))
        (remhash (collect-output-process-pid info) *subprocesses*)
        (collect-app-output-cleanup info)))))
