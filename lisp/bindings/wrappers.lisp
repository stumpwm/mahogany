(in-package #:hrt)

(defmacro with-return-by-value (variables &body body)
  `(cffi:with-foreign-objects ,variables
     ,@body
     (values ,@(loop for pair in variables
		     collect `(cffi:mem-ref ,(first pair) ,(second pair))))))

(defun output-resolution (output)
  (declare (type cffi:foreign-pointer output))
  (with-return-by-value ((width :int) (height :int))
    (hrt-output-resolution output width height)))

(defun output-position (output)
  (declare (type cffi:foreign-pointer output))
  (with-return-by-value ((x :int) (y :int))
    (hrt-output-position output x y)))
