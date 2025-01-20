(in-package #:mahogany)

(defun group-focus-frame (group frame seat)
  (with-accessors ((current-frame mahogany-group-current-frame)) group
    (when current-frame
      (group-unfocus-frame group current-frame seat))
    (tree:mark-frame-focused frame seat)
    (setf current-frame frame)))

(defun group-unfocus-frame (group frame seat)
  (with-accessors ((current-frame mahogany-group-current-frame)) group
    (tree:unmark-frame-focused frame seat)
    (setf current-frame nil)))

(defun group-add-output (group output seat)
  (declare (type mahogany-output output)
	   (type mahogany-group group))
  (with-accessors ((output-map mahogany-group-output-map)
		   (tree-container mahogany-group-tree-container)
		   (current-frame mahogany-group-current-frame))
      group
    (multiple-value-bind (x y) (hrt:output-position (mahogany-output-hrt-output output))
      (multiple-value-bind (width height) (hrt:output-resolution (mahogany-output-hrt-output output))
	(let ((new-tree (tree:tree-container-add tree-container
					    :x x :y y :width width :height height)))
	  (setf (gethash (mahogany-output-full-name output) output-map) new-tree)
	  (when (not current-frame)
	    (group-focus-frame group (tree:find-first-leaf new-tree) seat)))))
    (log-string :trace "Group map: ~S" output-map)))

(defun group-reconfigure-outputs (group outputs)
  "Re-examine where the outputs are and adjust the trees that are associated with them
to match."
  (with-accessors ((output-map mahogany-group-output-map)) group
    (loop for mh-output across outputs
	  do (with-accessors ((full-name mahogany-output-full-name)
			      (hrt-output mahogany-output-hrt-output))
		 mh-output
	       (alexandria:when-let ((tree (gethash full-name output-map)))
		 (multiple-value-bind (x y) (hrt:output-position hrt-output)
		   (set-position tree x y))
		 (multiple-value-bind (width height) (hrt:output-resolution hrt-output)
		   (set-dimensions tree width height)))))))

(defun %first-hash-table-value (table)
  (declare (type hash-table table)
	   (optimize (speed 3) (safety 0)))
  (with-hash-table-iterator (iter table)
    (multiple-value-bind (found key value) (iter)
      (declare (ignore found key))
      value)))

(defun group-remove-output (group output seat)
  (declare (type mahogany-output output)
	   (type mahogany-group group))
  (with-accessors ((output-map mahogany-group-output-map)) group
    (let* ((output-name (mahogany-output-full-name output))
	   (tree (gethash output-name output-map)))
      (remhash output-name output-map)
      (when (equalp tree (tree:find-root-frame (mahogany-group-current-frame group)))
	(group-unfocus-frame group (mahogany-group-current-frame group) seat)
	(alexandria:when-let ((other-tree (%first-hash-table-value output-map)))
	  (group-focus-frame group (tree:find-first-leaf other-tree) seat)))
      (when (and (mahogany-group-current-frame group) (= 0 (hash-table-count output-map)))
	(group-unfocus-frame group (mahogany-group-current-frame group) seat))
      (tree:remove-frame tree))))

(defun %add-hidden (hidden-list view)
  (ring-list:add-item hidden-list view)
  (hrt:view-set-hidden view t))

(defun %swap-next-hidden (hidden-list view)
  (let ((swapped (ring-list:swap-next hidden-list view)))
    (hrt:view-set-hidden view t)
    (hrt:view-set-hidden swapped nil)
    swapped))

(defun %swap-prev-hidden (hidden-list view)
  (let ((swapped (ring-list:swap-previous hidden-list view)))
    (hrt:view-set-hidden view t)
    (hrt:view-set-hidden swapped nil)
    swapped))

(defun %pop-hidden-item (hidden-list)
  (alexandria:when-let ((popped (ring-list:pop-item hidden-list)))
    (hrt:view-set-hidden popped nil)
    popped))

(defun group-add-view (group view)
  (declare (type mahogany-group group)
	   (type hrt:view view))
  (with-accessors ((views mahogany-group-views)
		   (outputs mahogany-group-output-map)
		   (hidden mahogany-group-hidden-views))
      group
    (push view (mahogany-group-views group))
    (alexandria:when-let ((current-frame (mahogany-group-current-frame group)))
      (alexandria:when-let ((view (tree:frame-view current-frame)))
	(%add-hidden hidden view))
      (setf (tree:frame-view current-frame) view))))

(defun group-remove-view (group view)
  (declare (type mahogany-group group))
  (with-accessors ((view-list mahogany-group-views)
		   (output-map mahogany-group-output-map)
		   (hidden mahogany-group-hidden-views))
      group
    (dolist (tree (tree:tree-children (mahogany-group-tree-container group)))
      (dolist (f (mahogany/tree:get-populated-frames tree))
	(when (equalp (tree:frame-view f) view)
	  (setf (tree:frame-view f) nil)
	  (alexandria:when-let ((new-view (%pop-hidden-item hidden)))
	    (setf (tree:frame-view f) new-view)))))
    (ring-list:remove-item hidden view)
    (setf view-list (remove view view-list :test #'equalp))))

(defmethod tree:find-empty-frame ((group mahogany-group))
  (with-hash-table-iterator (iter (mahogany-group-output-map group))
    (tagbody
     :top (multiple-value-bind (found name frame) (iter)
	    (declare (ignore name))
	    (when found
	      (alexandria:if-let ((view-frame (tree:find-empty-frame frame)))
		(return-from tree:find-empty-frame view-frame)
		(go :top)))))))

(defun group-maximize-current-frame (group)
  "Remove all of the splits in the current window tree and replae it with the
currently focused frame"
  (declare (type mahogany-group group))
  (let* ((current-frame (mahogany-group-current-frame group))
	 (tree-root (mahogany/tree:find-root-frame current-frame)))
    (flet ((hide-and-disable (view-frame)
	     (alexandria:when-let ((view (tree:frame-view view-frame)))
	       (%add-hidden (mahogany-group-hidden-views group) view))))
      (tree:replace-frame tree-root current-frame #'hide-and-disable))))

(defun group-next-hidden (group)
  (declare (type mahogany-group group))
  (let ((current-frame (mahogany-group-current-frame group))
	(hidden-views (mahogany-group-hidden-views group))
	(next-view))
    (when (> (ring-list:ring-list-size hidden-views) 0)
      (alexandria:if-let ((view (tree:frame-view current-frame)))
	(setf next-view (%swap-next-hidden hidden-views view))
	(setf next-view (%pop-hidden-item hidden-views)))
      (setf (tree:frame-view current-frame) next-view))))

(defun group-previous-hidden (group)
  (declare (type mahogany-group group))
  (let ((current-frame (mahogany-group-current-frame group))
	(hidden-views (mahogany-group-hidden-views group))
	(next-view))
    (when (> (ring-list:ring-list-size hidden-views) 0)
      (alexandria:if-let ((view (tree:frame-view current-frame)))
	(setf next-view (%swap-prev-hidden hidden-views view))
	(setf next-view (%pop-hidden-item hidden-views)))
      (setf (tree:frame-view current-frame) next-view))))

(defun group-next-frame (group seat)
  (declare (type mahogany-group group))
  (let* ((current-frame (mahogany-group-current-frame group))
	 (next-frame (tree:frame-next current-frame)))
    (group-focus-frame group next-frame seat)))

(defun group-prev-frame (group seat)
  (declare (type mahogany-group group))
  (let* ((current-frame (mahogany-group-current-frame group))
	 (prev-frame (tree:frame-prev current-frame)))
    (group-focus-frame group prev-frame seat)))
