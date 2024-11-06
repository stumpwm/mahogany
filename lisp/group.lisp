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
		   (current-frame mahogany-group-current-frame))
      group
    (multiple-value-bind (x y) (hrt:output-position (mahogany-output-hrt-output output))
      (multiple-value-bind (width height) (hrt:output-resolution (mahogany-output-hrt-output output))
	(let ((new-tree (tree:make-basic-tree :x x :y y :width width :height height)))
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
		   (set-position (tree:root-tree tree) x y))
		 (multiple-value-bind (width height) (hrt:output-resolution hrt-output)
		   (set-dimensions (tree:root-tree tree) width height)))))))

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
	   (tree-container (gethash output-name output-map)))
      (remhash output-name output-map)
      (when (equalp tree-container (tree:find-frame-container (mahogany-group-current-frame group)))
	(group-unfocus-frame group (mahogany-group-current-frame group) seat)
	(alexandria:when-let ((other-container (%first-hash-table-value output-map)))
	  (group-focus-frame group (tree:find-first-leaf other-container) seat)))
      (when (and (mahogany-group-current-frame group) (= 0 (hash-table-count output-map)))
	(group-unfocus-frame group (mahogany-group-current-frame group) seat)))))

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
	(ring-list:add-item hidden view))
      (setf (tree:frame-view current-frame) view))))

(defun group-remove-view (group view)
  (declare (type mahogany-group group))
  (with-accessors ((view-list mahogany-group-views)
		   (output-map mahogany-group-output-map)
		   (hidden mahogany-group-hidden-views))
      group
    (maphash (lambda (key container)
	       (declare (ignore key))
	       ;; OPTIMIZE ME: get-pouplated frames builds a list, we could use an iterator instead.
	       (dolist (f (mahogany/tree:get-populated-frames (mahogany/tree:root-tree container)))
		 (when (equalp (tree:frame-view f) view)
		   (setf (tree:frame-view f) nil)
		   (alexandria:when-let ((new-view (ring-list:pop-item hidden)))
		     (setf (tree:frame-view f) new-view)))))
	     output-map)
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
  (declare (type mahogany-group group))
  (let* ((current-frame (mahogany-group-current-frame group))
	 (container (mahogany/tree:find-frame-container current-frame))
	 (tree-root (tree:root-tree container)))
    (flet ((hide-and-disable (view-frame)
	       (alexandria:when-let (view (tree:frame-view view-frame))
		 (ring-list:add-item (mahogany-group-hidden-views group) view))))
      (tree:replace-frame tree-root current-frame #'hide-and-disable))))
