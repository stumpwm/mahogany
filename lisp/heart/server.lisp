(in-package #:hrt)

(declaim (type cl-freelock:queue *work-queue*))
(mahogany/util::defglobal *work-queue* (cl-freelock:make-queue)
  "Queue holding the functions to be executed on the main thread")

(declaim (type (or null cffi:foreign-pointer) *workqueue-semaphore*))
(defparameter *workqueue-semaphore** nil)

(cffi:defcallback work-queue-callback :int
    ((fd :int)
     (mask :uint32)
     (data :pointer))
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
      ;;  find a way to recover from this or even why we would get an error. As I see it,
      ;;  there's two options; either remove the callback from the event loop, or terminate
      ;;  the compositor because vital functionality won't be available.
      ;;  Termination is way easier to implement, so do that.
      (error 'mahogany/util:mahogany-panic :text "Fatal error when reading work-queue semaphore")
      (return-from work-queue-callback 0))
    (hrt-event-loop-semaphore-decrement *workqueue-semaphore*)
    ;; Purposefully only execute one callback at a time to not hog resources:
    (alexandria:when-let ((func (cl-freelock:queue-pop *work-queue*)))
      (mahogany/log:log-string :trace "Running ~S on main thread" func)
      (run-with-restarts func))
    0))

(defun run-in-main-thread (func)
  "Run the given function in the main thread. The function must have no arguments.

This allows actions originating in external threads to manipulate compositor data safely."
  (declare (type function func))
  (check-type func function)
  (cl-freelock:queue-push *work-queue* func)
  (hrt-event-loop-semaphore-increment *workqueue-semaphore* 1))

(defun server-init (server output-callbacks seat-callbacks view-callbacks
                    debug-level)
  (let ((initialized (hrt-server-init server
                       output-callbacks seat-callbacks view-callbacks
                       debug-level)))
    (when initialized
      (setf *workqueue-semaphore*
            (hrt-event-loop-semaphore-add server 0
                                          (cffi:callback work-queue-callback))))
    initialized))

(defun server-finish (server)
  (hrt-event-loop-semaphore-close *workqueue-semaphore*)
  (setf *workqueue-semaphore* nil)
  (hrt-server-finish server))
