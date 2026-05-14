(in-package #:hrt)

(defmacro with-return-by-value (variables &body body)
  `(cffi:with-foreign-objects ,variables
     ,@body
     (values ,@(loop for pair in variables
		     collect `(cffi:mem-ref ,(first pair) ,(second pair))))))

(defmacro foreign-struct-create ((type) &rest slots)
  (let ((slot-names (mapcar #'first slots))
	(var-name (gensym "init")))
    `(let ((,var-name (cffi:foreign-alloc (quote ,type))))
       (cffi:with-foreign-slots (,slot-names ,var-name ,type)
	 (setf ,@(loop for pair in slots
		       append pair)))
       ,var-name)))

(defmacro with-foreign-struct-init ((var type) slots &body body)
  (let ((slot-names (mapcar #'first slots)))
  `(cffi:with-foreign-object (,var (quote ,type))
     (cffi:with-foreign-slots (,slot-names ,var ,type)
       (setf ,@(loop for pair in slots
		     append pair)))
       ,@body)))

(declaim (inline write-color-array))
(defun write-color-array (ptr color)
  (declare (type cl-colors2:rgb color)
           (type cffi:foreign-pointer ptr))
  (setf (cffi:mem-aref ptr :float 0) (coerce (colors:rgb-red color) 'single-float)
        (cffi:mem-aref ptr :float 1) (coerce (colors:rgb-green color) 'single-float)
        (cffi:mem-aref ptr :float 2) (coerce (colors:rgb-blue color) 'single-float)
        (cffi:mem-aref ptr :float 3) 1.0))
