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
