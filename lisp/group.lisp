(in-package #:mahogany)



(defun group-add-output (group output)
  (declare (type mahogany-output output)
	   (type mahogany-group group))
  (with-accessors ((output-map mahogany-group-output-map)) group
    (multiple-value-bind (x y) (hrt:output-position (mahogany-output-hrt-output output))
      (multiple-value-bind (width height) (hrt:output-resolution (mahogany-output-hrt-output output))
	(setf (gethash (mahogany-output-full-name output) output-map)
	      (make-basic-tree :x x :y y :width width :height height))
	(log-string :trace "Group map: ~S" output-map)))))

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
		   (set-position (root-tree tree) x y))
		 (multiple-value-bind (width height) (hrt:output-resolution hrt-output)
		   (set-dimensions (root-tree tree) width height)))))))


(defun group-remove-output (group output)
  (declare (type mahogany-output output)
	   (type mahogany-group group))
  (remhash (mahogany-output-full-name output) (mahogany-group-output-map group)))

(defun group-add-view (group view)
  (declare (type mahogany-group group)
	   (type hrt:view view))
  (with-accessors ((views mahogany-group-views)
		   (outputs mahogany-group-output-map))
      group
    (push view (mahogany-group-views group))
    ;; (with-hash-table-iterator (iter outputs)
    (loop for tree being the hash-values of outputs
	  do (when-let ((empty (find-empty-frame tree)))
	       (log-string :trace "Found frame for view")
	       (put-view-in-frame view empty)
	       (log-string :trace "Current tree: ~S" (root-tree tree))
	       (return-from group-add-view)))
    ;; TODO: get algorithm to place new views so they can be seen:
    (log-string :error "Could not find frame for new view")))

(defun group-remove-view (group view)
  (declare (type mahogany-group group))
  (with-accessors ((view-list mahogany-group-views)) group
    (setf view-list (remove view view-list :test #'equalp))))
