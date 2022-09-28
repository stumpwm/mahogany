(in-package #:mahogany)



(defun handle-key-event (key seat)
  (declare (type key key)
	   (ignore seat))
  ;; TODO: add the keysym constants (or compute them at compile time)
  ;; in xkb library
  (when (eql 65307 (key-keysym key))
    (server-stop *compositor-state*)
    t))
