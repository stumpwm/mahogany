(in-package #:mahogany/wm)

(defclass window-manager ()
  ((backend :accessor window-manager-backend)
   (groups :reader wm-groups
	   :initform (make-array 1 :fill-pointer t)
	   :type vector)
   (views :accessor wm-views
	  :initform nil
	  :type list)))

(defmethod set-backend ((wm window-manager) backend)
  (setf (window-manager-backend wm) backend))

(defmethod add-view ((wm window-manager) view)
  (push view (wm-views wm)))

(defmethod remove-view ((wm window-manager) view)
  (removef (wm-views wm) view :test #'equal))

(defmethod get-visible-views ((wm window-manager))
  (wm-views wm))
