(in-package #:mahogany)

(declaim (type mahogany-state *compositor-state*))
(defglobal *compositor-state* (make-instance 'mahogany-state))


(defun handle-server-stop (sequence seat)
  (declare (ignore sequence seat))
  (server-stop *compositor-state*))

(defun open-terminal (sequence seat)
  (declare (ignore sequence seat))
  (sys:open-terminal))

(setf (mahogany-state-keybindings *compositor-state*)
      (list (define-kmap
	      (kbd "C-t") (define-kmap
			    (kbd "q") #'handle-server-stop
			    (kbd "c") #'open-terminal))))
