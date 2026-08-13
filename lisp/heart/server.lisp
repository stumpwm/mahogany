(in-package #:hrt)

(mahogany/util:defglobal *hrt-server* nil
    "Reference to current hrt-server object used in macros and other
constructs that need a reference to it.")

(declaim (type fset:seq *work-queue*))
(mahogany/util::defglobal *work-queue* (fset:seq)
  "Queue holding the functions to be executed on the main thread")

(declaim (type (or null cffi:foreign-pointer) *workqueue-semaphore*))
(defparameter *workqueue-semaphore* nil)

(declaim (type hash-table *timer-table*))
(mahogany/util::defglobal *timer-table* (make-hash-table)
    "Table holding the pending timers")

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
           (declare (type function func))
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

(defstruct (timer-handle
            (:constructor make-timer-handle (handle callback data)))
  (handle nil :type cffi:foreign-pointer :read-only t)
  (callback nil :type (function (timer-handle) t) :read-only t)
  (data nil))

(define-hrt-callback timer-callback :int
    ((data :pointer))
    ()
  (declare (inline gethash))
  (let ((handle (gethash (cffi:pointer-address data) *timer-table*)))
    (declare (type timer-handle handle))
    (mahogany/log:log-string :trace "timer callback called: ~S" handle)
    (funcall (timer-handle-callback handle) handle)))

(defun server-make-timer (server callback &optional data)
  (let* ((timer
           (hrt-event-loop-timer-add server (cffi:callback timer-callback)))
         (handle (make-timer-handle timer callback data)))
    (setf (gethash (cffi:pointer-address timer) *timer-table*)
          handle)
    handle))

(defun timer-handle-update (handle msec-delay)
  (declare (type timer-handle handle))
  (let ((result
          (hrt-event-loop-timer-update (timer-handle-handle handle)
                                       msec-delay)))
    (if (< 0 result)
        t
        nil)))

(defun timer-handle-destroy (handle)
  (declare (type timer-handle handle))
  (let ((timer-handle (timer-handle-handle handle)))
    (remhash (cffi:pointer-address timer-handle) *timer-table*)
    (hrt-event-loop-timer-destroy timer-handle)))

(declaim (inline %hrt-server))
(defun %hrt-server ()
  "Get the global hrt-server object"
  #+hrt-debug
  (if *hrt-server*
      *hrt-server*
      (error 'mahogany/util:mahogany-panic
             :text
	     "hrt server object has not been registered. SERVER-INIT must be called first."))
  #-hrt-debug
  *hrt-server*)

(defun server-init (server output-callbacks seat-callbacks view-callbacks
                    layer-shell-callbacks
                    debug-level)
  (let ((initialized (hrt-server-init
                      server
                      output-callbacks seat-callbacks view-callbacks
                      layer-shell-callbacks
                      debug-level)))
    (when initialized
      (setf *hrt-server* server)
      (setf *workqueue-semaphore*
            (hrt-event-loop-semaphore-add server 0
                                          (cffi:callback work-queue-callback))))
    initialized))

(defun server-start (server)
  (float-features:with-float-traps-masked (:overflow :divide-by-zero)
    (hrt-server-start server)))

(defun server-finish (server)
  (hrt-event-loop-semaphore-close *workqueue-semaphore*)
  (setf *workqueue-semaphore* nil)
  (hrt-server-finish server)
  (setf *hrt-server* nil))

(defun server-group-create (server)
  (declare (type cffi:foreign-pointer server))
  (let ((result (hrt-server-group-create server)))
    (if (not (cffi:null-pointer-p result))
        result
        (error 'mahogany/util:mahogany-panic
               "Could not make hrt-server-group"))))
