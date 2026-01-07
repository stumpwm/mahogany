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

(defun output-usable-area (output)
  (declare (type cffi:foreign-pointer output))
  (let ((box (cffi:foreign-slot-pointer output '(:struct hrt-output)
                                      'usable-area)))
    (cffi:with-foreign-slots ((x y width height) box (:struct wlr-box))
      (values x y width height))))
