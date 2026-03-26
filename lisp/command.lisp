(in-package #:mahogany)

(setf cl-interactive:*default-input-method* (make-instance 'foot-input-method))

(defun interactively-read-string (com im arg prompt)
  (declare (ignore com arg))
  (cl-interactive:input-method-read im prompt :require-match nil))

(defmacro defcommand (name (&key sequence seat) &body body)
  (let ((seat-var (or seat (gensym "seat")))
        (sequence-var (or sequence (gensym "sequence"))))
  `(defun ,name (,sequence-var ,seat-var)
     ,@(when (or (not sequence) (not seat))
         `((declare ,@(when (not sequence)
                       `((ignore ,sequence-var)))
                   ,@(when (not seat)
                       `((ignore ,seat-var))))))
     ,@body)))
