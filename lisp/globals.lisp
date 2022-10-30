(in-package #:mahogany)

(declaim (type mahogany-state *compositor-state*))
(defglobal *compositor-state* (make-instance 'mahogany-state))


(defun handle-server-stop (sequence seat)
  (server-stop *compositor-state*))

(setf (mahogany-state-keybindings *compositor-state*)
      (list (define-kmap
	      (kbd "C-t") (define-kmap
			    (kbd "q") #'handle-server-stop))))
