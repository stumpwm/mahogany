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

(defstruct (view (:constructor %make-view (hrt-view)))
  (hrt-view (cffi:null-pointer) :type cffi:foreign-pointer :read-only t))

(defun view-init (hrt-view scene-tree)
  (let ((view (%make-view hrt-view)))
    (hrt-view-init hrt-view scene-tree)
    (the view view)))

(defmethod mh/interface:set-dimensions ((view view) width height)
  (hrt-view-set-size (view-hrt-view view) width height))

(defmethod mh/interface:set-position ((view view) x y)
  (hrt-view-set-relative (view-hrt-view view) x y))
