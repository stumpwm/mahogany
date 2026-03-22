(in-package #:hrt)

(defstruct view-transaction-state
  (dirty nil :type boolean))

(declaim (type (or null view-transaction-state) *view-manipulation-state*))
(defvar *view-manipulation-state* nil
  "State of the current view mainpulation transaction")

(defun %commit-view-transaction ()
  (mahogany/log:log-string :trace "Commiting view transaction")
  (hrt-seat-reset-view-under
   (hrt-server-seat (%hrt-server))))

(defun dirty-view-transaction ()
  (if *view-manipulation-state*
      (setf (view-transaction-state-dirty *view-manipulation-state*) t)
      (%commit-view-transaction)))

(defmacro with-view-transaction (() &body body)
  (let ((body-fn-name (gensym "body-fn")))
    `(flet ((,body-fn-name ()
              ,@body))
       (if (not *view-manipulation-state*)
           (let ((*view-manipulation-state* (make-view-transaction-state)))
             (,body-fn-name)
             (when (view-transaction-state-dirty *view-manipulation-state*)
               (%commit-view-transaction)))
           (,body-fn-name)))))
