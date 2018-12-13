(in-package #:mahogany/wm)

(defclass window-manager ()
  ((backend :accessor window-manager-backend)
   ;; (groups :reader wm-groups
   ;; 	   :initform (make-array 1 :fill-pointer t)
   ;; 	   :type vector)
   (views :accessor wm-views
	  :initform nil
	  :type list)
   (tree :reader wm-tree
	 :type tree-container
	 :initform (let ((container (make-instance 'tree-container))
			 (frame (make-instance 'view-frame :x -25 :y -25 :width 1918 :height 1059)))
		     (setf (frame-parent frame) container)
		     (setf (root-tree container) frame)
		     container))))

(defmethod set-backend ((wm window-manager) backend)
  (setf (window-manager-backend wm) backend))

(defmethod add-view ((wm window-manager) view)
  (unless (find-empty-frame (wm-tree wm))
    (split-frame-v (root-tree (wm-tree wm))))
  (let ((empty-frame (find-empty-frame (wm-tree wm))))
    (if empty-frame
	(progn (setf (frame-view empty-frame) view)
	       (fit-view-into-frame view empty-frame))
	(push view (wm-views wm))))
  (print (root-tree (wm-tree wm))))

;; (defmethod add-output ((wm window-manager) output)
;;   ;; set the

(defmethod remove-view ((wm window-manager) view)
  (removef (wm-views wm) view :test #'equal))

(defmethod get-visible-views ((wm window-manager))
  (mapcar #'frame-view (remove-if  (lambda (x)
					   (not (frame-view x)))
				   (snakes:generator->list (leafs-in (root-tree (wm-tree wm)))))))
