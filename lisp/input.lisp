(in-package #:mahogany)

(defun execute-command (function key-sequence seat)
  (funcall function key-sequence seat))

(defun check-and-run-keybinding (key seat)
  (declare (type key key) (optimize speed))
  (when (not (key-modifier-key-p key))
    (let* ((key-state (mahogany-state-key-state *compositor-state*))
	   (handling-keybinding (key-state-active-p key-state)))
      (log-string :trace "Already handling keybinding: ~A" handling-keybinding)
      (flet ((reset-state ()
	       (log-string :trace "Reseting keyboard state")
	       (server-keystate-reset *compositor-state*)))
	(prog1
	    (multiple-value-bind (matched result) (key-state-advance key key-state)
	      (cond
		(;; A known keybinding was pressed:
		 matched
		 (when result
		   (execute-command result (key-state-sequence key-state) seat)
		   (reset-state))
		 t)
		(;; No keybinding was pressed but we were expecting one.
		 ;; Since this is canceling the keybinding, we still behave like we found somthing
		 handling-keybinding
		 (reset-state)
		 t)
		;; No action was taken, return nil
		(t  nil)))
	  (log-string :trace "Keyboard state: ~A" (mahogany-state-key-state *compositor-state*)))))))

(defun handle-key-event (key seat)
  (declare (type key key))
  (cond
    ((check-and-run-keybinding key seat) t)
    (t
     ;; TODO: add the keysym constants (or compute them at compile time)
     ;; in xkb library
     (when (eql 65307 (key-keysym key))
       (server-stop *compositor-state*)
       t))))
