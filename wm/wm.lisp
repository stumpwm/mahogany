(in-package #:mahogany/wm)

(defclass window-manager ()
  ((groups :reader wm-groups
	   :initform (make-array 1 :fill-pointer t)
	   :type vector)
   (views :accessor wm-views
	  :type list)))

(defmethod add-view ((wm window-manager) view)
  (push view (wm-views wm)))

(defmethod remove-view ((wm window-manager) view)
  (removef (wm-views wm) view :test #'equal))

(defmethod get-visible-views ((wm window-manager))
  (wm-views wm))
