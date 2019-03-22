(in-package #:mahogany/wm)

(defclass window-manager ()
  ((backend :accessor window-manager-backend)
   ;; (groups :reader wm-groups
   ;; 	   :initform (make-array 1 :fill-pointer t)
   ;; 	   :type vector)
   (views :accessor wm-views
	  :initform nil
	  :type list)
   (visible-views :accessor wm-visible-views
		  :initform nil
		  :type list)
   (outputs :accessor wm-outputs
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

(defun generate-visible-views (frame)
  (declare (optimize (speed 2)))
  (mapcar #'frame-view
	  (remove-if  (lambda (x)
			(not (frame-view x)))
		      (snakes:generator->list (leafs-in frame)))))

(defmethod add-view ((wm window-manager) view)
  (let ((frame (root-tree (output-tree (first (wm-outputs wm))))))
    (unless (find-empty-frame frame)
      (split-frame-v frame))
    (let ((empty-frame (find-empty-frame frame)))
      (if empty-frame
	  (progn (setf (frame-view empty-frame) view)
		 (fit-view-into-frame view empty-frame))
	  (push view (wm-views wm))))
    (setf (slot-value wm 'visible-views) (generate-visible-views frame))))

;; (defmethod add-output ((wm window-manager) output)
;;   ;; set the

(defmethod remove-view ((wm window-manager) view)
  (let ((leafs (get-populated-frames (root-tree (output-tree (first (wm-outputs wm)))))))
    (when-let ((frame (find view leafs :key #'frame-view
  			    :test #'equalp)))
      (log-string :trace "View removed from tree")
      (setf (frame-view frame) nil)))
  (removef (wm-visible-views wm) view :test #'equal)
  (removef (wm-views wm) view :test #'equal))

(defmethod add-output ((wm window-manager) output)
  (push output (wm-outputs wm)))

(defmethod initialize-instance :after ((output output) &key &allow-other-keys)
  (with-accessors ((tree output-tree)
		   (x output-x)
		   (y output-y)
		   (width output-width)
		   (height output-height))
      output
    (setf tree (let ((container (make-instance 'tree-container))
		     (frame (make-instance 'view-frame :x 0 :y 0 :width 0 :height 0)))
		 (setf (frame-parent frame) container)
		 (setf (root-tree container) frame)
		 container))))

(defmethod configure-output :after ((output output) x y width height)
  (LOG-STRING :TRACE "Configuring output in wm")
  (let ((tree (root-tree (output-tree output))))
    (set-dimensions tree
		    (output-width output) (output-height output))
    (setf (frame-x tree) (output-x output)
	  (frame-y tree) (output-y output))
    (log-string :trace "~A" tree)))


(defmethod get-visible-views ((wm window-manager))
  (with-accessors ((visible-views wm-visible-views)) wm
    (if (not visible-views)
	(setf visible-views (generate-visible-views (root-tree (output-tree (first (wm-outputs wm))))))
	visible-views)))

(defmethod view-at ((wm window-manager) x y)
  (if-let ((frame (frame-at (root-tree (output-tree (first (wm-outputs wm)))) x y)))
    (frame-view frame)
    ;; at this stage in the program's progress, it is an error if we don't find a frame:
    (error "Could not find frame")))
