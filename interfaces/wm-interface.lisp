(in-package #:mahogany/wm-interface)

(defclass output ()
  ((x :accessor output-x
      :type fixnum
      :initform 0)
  (y :accessor output-y
     :type fixnum
     :initform 0)
  (width :accessor output-width
   	 :type fixnum)
  (height :accessor output-height
   	  :type fixnum)))


(defgeneric set-backend (wm backend)
  (:documentation "Set the backend of the wm"))

(defgeneric get-visible-views (wm)
  (:documentation "Get the views to display."))

(defgeneric add-view (wm view)
  (:documentation "Add the view to the wm"))

(defgeneric remove-view (wm view)
  (:documentation "Remove the view from the wm"))

(defgeneric view-at (wm x y)
  (:documentation "Get the view at the specified output coordinates"))
