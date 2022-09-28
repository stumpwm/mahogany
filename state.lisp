(in-package #:mahogany)

(defclass mahogany-state ()
  ((hrt-server :type hrt-server
	       :initarg server
	       :accessor mahogany-state-server)
   (key-state :type key-state
	      :accessor mahogany-state-key-state)
   (prefix-key :type key
	       :accessor mahogany-state-prefix-key)))

(defun server-state-reset (state)
  (declare (type state mahogany-state))
  (setf (mahogany-state-server state) nil))

(defun server-stop (state)
  (declare (type state mahogany-state))
  (hrt:hrt-server-stop (mahogany-state-server state)))
