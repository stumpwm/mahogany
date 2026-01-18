(in-package #:hrt)

(defmacro with-return-by-value (variables &body body)
  `(cffi:with-foreign-objects ,variables
     ,@body
     (values ,@(loop for pair in variables
                     collect `(cffi:mem-ref ,(first pair) ,(second pair))))))

(defmacro with-foreign-struct-init ((var type) slots &body body)
  (let ((slot-names (mapcar #'first slots)))
  `(cffi:with-foreign-object (,var (quote ,type))
     (cffi:with-foreign-slots (,slot-names ,var ,type)
       (setf ,@(loop for pair in slots
                     append pair)))
       ,@body)))

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

(deftype toast-message-gravity ()
  `(member ,@(cffi:foreign-enum-keyword-list 'window-gravity)))

(declaim (inline write-color-array))
(defun write-color-array (ptr color)
  (declare (type cl-colors2:rgb color)
           (type cffi:foreign-pointer ptr))
  (setf (cffi:mem-aref ptr :float 0) (coerce (colors:rgb-red color) 'single-float)
        (cffi:mem-aref ptr :float 1) (coerce (colors:rgb-green color) 'single-float)
        (cffi:mem-aref ptr :float 2) (coerce (colors:rgb-blue color) 'single-float)
        (cffi:mem-aref ptr :float 3) 1.0))

(defun toast-message (server output text
                      gravity
                      theme
                      ms-delay)
  (declare (type cffi:foreign-pointer server output)
           (type toast-message-gravity gravity)
           (type integer ms-delay)
           (type mh/theme:theme theme)
           (type string text)
           (optimize (speed 3) (safety 1) (debug 0)))
  (cffi:with-foreign-strings ((m text) (c-font (mh/theme:theme-font theme)))
    (with-foreign-struct-init (message-theme (:struct hrt-message-theme))
        ((font c-font)
         (message-padding 5)
         (message-border-width 1)
         (margin-x 15)
         (margin-y 15))
      (write-color-array (cffi:foreign-slot-pointer message-theme
					            '(:struct hrt-message-theme)
					            'font-color)
		         (mh/theme:theme-font-color theme))
      (write-color-array (cffi:foreign-slot-pointer message-theme
					            '(:struct hrt-message-theme)
					            'background-color)
		         (mh/theme:theme-background-color theme))
      (write-color-array (cffi:foreign-slot-pointer message-theme
                                                    '(:struct hrt-message-theme)
                                                    'border-color)
                         (mh/theme:theme-border-color theme))
      (hrt-toast-message server output m gravity message-theme ms-delay))))
