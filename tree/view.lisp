(defpackage :mahogany/tree/view
  (:use :cl))

(defclass view-frame (frame)
  ((view :initarg :view
	 :accessor frame-view
	 :documentation "The client of the frame")
   (modes :initarg :modes
	  :accessor frame-modes)))
