(in-package #:mahogany/wm-interface)

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

(defgeneric configure-output (output x y width height)
  (:documentation "Set the width, height, x, y of the output in layout coordinates."))

(defgeneric add-output (wm output)
  (:documentation "Add the output to the wm"))

;; These functions are expected to be implemented by the backend:

(defgeneric output-width (output)
  (:documentation "Get the width of the output"))

(defgeneric output-height (output)
  (:documentation "Get the width of the output"))

;; class definitions:

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
   	   :type fixnum)
   (tree :accessor output-tree)
   (floating-windows :accessor output-floating-windows
		     :type list
		     :initform nil)))
