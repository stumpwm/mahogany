(in-package #:hrt)

(defstruct (collect-output-process
			(:constructor make-collect-output-process (process pid stdout-result stderr-result callback))
			(:copier nil))
  ;; the process-info type is documented, but it's not external.
  ;; should we really be using it?
  (process nil :type uiop/launch-program::process-info :read-only t)
  (pid 0 :type (unsigned-byte 64) :read-only t)
  (stdout-result nil :type string-stream :read-only t)
  (stderr-result nil :type (or string-stream null))
  (callback nil :type (function (collect-output-result) null)))

(defun collect-app-output-begin (args callback &optional (stderr nil))
  (let* ((process (if stderr
					  (uiop:launch-program args :output :stream
												:error-output :stream)
					  (uiop:launch-program args :output :stream)))
		 (pid (uiop:process-info-pid process))
		 (stdout-result (make-string-output-stream)))
	(make-collect-output-process process pid stdout-result
								 (when stderr
								   (make-string-output-stream))
                                 callback)))

(defconstant +buffer-size+ 4096)

(defun %collect-app-output (output-stream result-stream)
  (let* ((buffer (make-array +buffer-size+ :element-type 'character)))
	(loop
	  ;; Read bytes into the buffer
	  :for bytes-read = (read-sequence buffer output-stream)
	  ;; Loop continues as long as some bytes are read
	  :while (plusp bytes-read)
	  :do
		 ;; Process or write the read bytes
		 (write-sequence buffer result-stream :end bytes-read))))

(defun collect-app-output-slurp (info)
  (declare (type collect-output-process info))
  (with-accessors ((process collect-output-process-process)
				   (stdout-result collect-output-process-stdout-result)
				   (stderr-result collect-output-process-stderr-result))
	  info
	(%collect-app-output (uiop:process-info-output process) stdout-result)
	(when stderr-result
	  (%collect-app-output (uiop:process-info-error-output process)
						   stderr-result))))

(defstruct collect-output-result
  (stdout nil :type string)
  (stderr nil :type (or null string))
  ;; Will be non-nil when we can get it. Because we are just
  ;; looking at stdout, we stop looking when stdout is closed,
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
		  (stderr-result (when stderr
						   (get-output-stream-string stderr)))
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
	(when stderr
	  (close stderr))))

(mahogany/util:defglobal *subprocesses* (make-hash-table))

(cffi:defcstruct app-output-data
  ;; Note: PIDs are usually ints, but use a long to give us some headroom:
  (pid :long)
  (event-source :pointer))

(cffi:defcallback handle-app-output :int
    ((fd :int)
     (mask :uint32)
     (data :pointer))
  (declare (ignore fd))
  (cffi:with-foreign-slots ((pid event-source) data
                            (:struct app-output-data))
    (let ((info (gethash pid *subprocesses*)))
      (when (plusp (logand mask +hrt-event-readable+))
        (collect-app-output-slurp info))
      (when (or (plusp (logand mask +hrt-event-hangup+))
                (plusp (logand mask +hrt-event-error+)))
        (hrt-event-loop-remove event-source)
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

(defun subprocess-collect (server args callback)
  "Start a subprocess with args ARGS and call CALLBACK with when it has finished
writing to stdout.

SERVER: a hrt-server object
ARGS:   A list of program arguments or the name of program; \"ls\"
        or (list \"ls\" \"/\"
CALLBACK: A function that takes a COLLECT-OUTPUT-RESULT object as an argument"
  (let ((info (collect-app-output-begin args callback))
        data)
    (handler-case
        (let ((stdout-fd (extract-stdout-fd info)))
          (setf data (foreign-struct-create ((:struct app-output-data))
                                            (pid (collect-output-process-pid info))))
          (setf (gethash (collect-output-process-pid info) *subprocesses*)
                info)
          (cffi:with-foreign-slots ((event-source) data (:struct app-output-data))
            (setf event-source (hrt-event-loop-add-fd
                                server stdout-fd
                                +hrt-event-readable+
                                (cffi:callback handle-app-output)
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
