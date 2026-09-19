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
  (macrolet ((convert (accessor)
               `(silence-notes
                  (coerce (,accessor color) 'single-float))))
    (setf (cffi:mem-aref ptr :float 0) (convert colors:rgb-red)
          (cffi:mem-aref ptr :float 1) (convert colors:rgb-green)
          (cffi:mem-aref ptr :float 2) (convert colors:rgb-blue)
          (cffi:mem-aref ptr :float 3) 1.0)))

(declaim (inline clear-object))
(defun clear-foreign-object (ptr type)
  (let ((type-size (cffi:foreign-type-size type)))
    (loop :for i
          :from 0 :below type-size
          :do (setf (cffi:mem-ref ptr :unsigned-char i) 0))))
