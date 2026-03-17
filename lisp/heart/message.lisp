(in-package #:hrt)

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
