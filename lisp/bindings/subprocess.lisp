(in-package #:hrt)

(mahogany/util:defglobal *subprocesses* (make-hash-table))

;; FIXME: this assumes that pid_t is an int, which is usually true:
(cffi:defctype pid :int)

(cffi:defcallback %handle-process-finish :void
    ((output :pointer)
     (size :size)
     (result hrt-collect-result)
     (pid pid)
     (data :pointer))
  ;; (declare (inline cffi:foreign-string-to-lisp))
  (unwind-protect
       (let* ((output (cffi:foreign-string-to-lisp output :max-chars size))
              (callback (gethash pid *subprocesses*)))
         (funcall callback output result data))
    (mahogany/log:log-string
     :trace "Recieved callback for subprocess with PID ~S"
     pid)
    (cffi:foreign-free output)
    (remhash pid *subprocesses*)))

(defun %start-process (server argv data)
  (let ((argv-len (length argv))
        ;; Reserve a separate list of pointers to clean up to
        ;; make this a bit easier instead of iterating through
        ;; the C array we create:
        (pointers (list)))
    (declare (dynamic-extent pointers))
    ;; Reserve space for a nullptr on the end, as it's
    ;; needed for the C code to deal with variable-length arrays:
    (cffi:with-foreign-object (c-args :pointer (+ argv-len 1))
      (loop :for s :in argv
            :for i :from 0
            :do
               (let ((ptr (cffi:foreign-string-alloc s)))
                 (push ptr pointers)
                 (setf (cffi:mem-aref c-args '(:pointer :char) i) ptr)))
      (setf (cffi:mem-aref c-args '(:pointer :char) argv-len)
            (cffi:null-pointer))
      (unwind-protect
           (with-return-by-value ((pid :int))
             (let ((result (hrt-output-from-subprocess server
                                         c-args
                                         (cffi:callback %handle-process-finish)
                                         data
                                         pid)))
               (when (not (eql result :hrt-exec-success))
                 (error "Could not start subprocess"))))
        (dolist (ptr pointers)
          (cffi:foreign-free ptr))))))

(defun subprocess-collect (server argv callback &optional
                                                  (data (cffi:null-pointer)))
  ;; FIXME:? In this case, using a type assertion is probably better,
  ;;  as DATA must be a foreign pointer.
  (declare (type cffi:foreign-pointer server data))
  "Collect the stdout of a subprocess started with arg list ARGV,
and call the callback CALLBACK when no more output is recieved.

ARGV: A list of strings denoting the program and its arguments
CALLBACK: A function taking the output string, the execution result, and the
  DATA pointer passed to this function."
  (let ((pid (%start-process server argv data)))
    (mahogany/log:log-string :trace "Started subprocess with PID ~S"
                             pid)
    (setf (gethash pid *subprocesses*) callback)))
