(in-package #:hrt)

(mahogany/util:defglobal *hrt-server* nil
    "Reference to current hrt-server object used in macros and other
constructs that need a reference to it.")

(declaim (type fset:seq *work-queue*))
(mahogany/util::defglobal *work-queue* (fset:seq)
  "Queue holding the functions to be executed on the main thread")

(declaim (type (or null cffi:foreign-pointer) *workqueue-semaphore*))
(defparameter *workqueue-semaphore* nil)

#-ATOMICS-CAS-SPECIAL-VAR
(error "Lisp implementation does not have support for required CAS operation")

(defmacro cas-deque (seq)
  (let ((old (gensym "OLD"))
        (b (gensym "CAS-DEQUE")))
    `(block ,b
       (tagbody
        loop
          (let ((,old ,seq))
            (multiple-value-bind (val success) (fset:first ,seq)
              (unless (atomics:cas ,seq ,old (fset:less-first ,old))
                (go loop))
              (return-from ,b (values val success))))))))

(defmacro cas-enque (seq val)
  (let ((var (gensym "VAL")))
    `(let ((,var ,val))
       (atomics:atomic-update ,seq
                              (lambda (q)
                                (fset:with-first q ,var))))))

(define-hrt-callback work-queue-callback :int
    ((fd :int)
     (mask :uint32)
     (data :pointer))
    ()
  (declare (ignore fd data))
  (flet ((run-with-restarts (func)
           (restart-case (funcall func)
             (continue ()
               :report "Continue executing, ignoring the error"))))
    (when (or (plusp (logand mask +hrt-event-hangup+))
	      (plusp (logand mask +hrt-event-error+)))
      (mahogany/log:log-string :error
                               "Error while waiting for work-queue semaphore (hangup: ~S) (error: ~S)."
                               (logand mask +hrt-event-hangup+)
                               (logand mask +hrt-event-error+))
      ;; FIXME: Things are hoplessly borked if we get an error here. I haven't been able to
      ;;  find a way to recover from this or even why we would get an error. Since
      ;;  this is vital to how the compositor works, initiate termination via a signal.
      (error 'mahogany/util:mahogany-panic :text "Fatal error when reading work-queue semaphore"))
    (hrt-event-loop-semaphore-decrement *workqueue-semaphore*)
    ;; Purposefully only execute one callback at a time to not hog resources:
    (alexandria:if-let ((func (cas-deque *work-queue*)))
      ;; func is non-null, don't need to check success val:
      (progn
        (mahogany/log:log-string :trace "Running ~S on main thread" func)
        (run-with-restarts func))
      (warn "work queue callback called with no value in the queue"))
    0))

(defun run-in-main-thread (func)
  "Run the given function in the main thread. The function must have no arguments.

This allows actions originating in external threads to manipulate compositor data safely.
The order of execution is not guaranteed if multiple lambdas are added at the same time."
  (declare (type function func))
  (check-type func function)
  ;; Increment the semaphore after enqueing to ensure the event loop doesn't
  ;; get woken up too early:
  (cas-enque *work-queue* func)
  (hrt-event-loop-semaphore-increment *workqueue-semaphore* 1))

(declaim (inline %hrt-server))
(defun %hrt-server ()
  "Get the global hrt-server object"
  #+hrt-debug
  (if *hrt-server*
      *hrt-server*
      (error 'mahogany/util:mahogany-panic
	     "hrt server object has not been registered. SERVER-INIT must be called first."))
  #-hrt-debug
  *hrt-server*)

(defun server-init (server output-callbacks seat-callbacks view-callbacks
                    debug-level)
  (let ((initialized (hrt-server-init server
                       output-callbacks seat-callbacks view-callbacks
                       debug-level)))
    (when initialized
      (setf *hrt-server* server)
      (setf *workqueue-semaphore*
            (hrt-event-loop-semaphore-add server 0
                                          (cffi:callback work-queue-callback))))
    initialized))

(defun server-finish (server)
  (hrt-event-loop-semaphore-close *workqueue-semaphore*)
  (setf *workqueue-semaphore* nil)
  (setf *hrt-server* nil)
  (hrt-server-finish server))
