(in-package #:mahogany)

(defun server-state-init (state server output-callbacks seat-callbacks view-callbacks
						  &key (debug-level 3))
  (with-accessors ((groups mahogany-state-groups)
				   (current-group mahogany-current-group))
	  state
	(setf (mahogany-state-server state) server)
	(hrt:hrt-server-init server
						 output-callbacks seat-callbacks view-callbacks
						 debug-level)
	(let* ((scene-tree (hrt:hrt-server-scene-tree server))
		   (default-group (make-mahogany-group "DEFAULT" 1 scene-tree)))
      (setf current-group default-group)
      (vector-push-extend default-group groups))))

(defun server-state-reset (state)
  (declare (type mahogany-state state))
  (with-accessors ((groups mahogany-state-groups)
				   (server mahogany-state-server))
	  state
	(let ((scene-tree (hrt:hrt-server-scene-tree server)))
	  (loop for g across groups
			:do (destroy-mahogany-group g scene-tree)))
	(hrt:hrt-server-finish server)
	(setf server nil)))

(defun server-stop (state)
  (declare (type mahogany-state state))
  (hrt:hrt-server-stop (mahogany-state-server state)))

(declaim (inline server-seat))
(defun server-seat (state)
  (hrt:hrt-server-seat (mahogany-state-server state)))

(defun server-keystate-reset (state)
  (setf (mahogany-state-key-state state)
	(make-key-state (mahogany-state-keybindings state))))

(defun (setf mahogany-state-keybindings) (kmaps state)
  (declare (type list kmaps)
	   (type mahogany-state state))
  (setf (slot-value state 'keybindings) kmaps)
  (unless (key-state-active-p (mahogany-state-key-state state))
    (server-keystate-reset state)))

(defun mahogany-state-output-add (state mh-output)
  (declare (type mahogany-state state)
	   (type mahogany-output mh-output))
  (with-accessors ((outputs mahogany-state-outputs)
		   (groups mahogany-state-groups))
      state
    (log-string :trace "New output added ~S" (mahogany-output-full-name mh-output))
    (vector-push-extend mh-output outputs)
    (loop for g across groups
	  do (group-add-output g mh-output (server-seat state)))))

(defun mahogany-state-output-remove (state hrt-output)
  (with-accessors ((outputs mahogany-state-outputs)
		   (groups mahogany-state-groups))
      state
    (let ((mh-output (find hrt-output outputs
			   :key #'mahogany-output-hrt-output
			   :test #'cffi:pointer-eq)))
      (log-string :trace "Output removed ~S" (mahogany-output-full-name mh-output))
      (loop for g across groups
	    do (group-remove-output g mh-output (server-seat state)))
      ;; TODO: Is there a better way to remove an item from a vector when we could know the index?
      (setf outputs (delete mh-output outputs :test #'equalp)))))

(defun mahogany-state-output-reconfigure (state)
  (log-string :trace "Output layout changed!")
  (with-accessors ((groups mahogany-state-groups)) state
    (loop for g across groups
	  do (group-reconfigure-outputs g (mahogany-state-outputs state)))))

(defun mahogany-state-view-add (state view-ptr)
  (declare (type mahogany-state state)
	   (type cffi:foreign-pointer view-ptr))
  (with-accessors ((view-tbl mahogany-state-views)
		   (current-group mahogany-current-group)
		   (server mahogany-state-server))
      state
    (let ((new-view (group-add-initialize-view current-group view-ptr)))
      (setf (gethash (cffi:pointer-address view-ptr) view-tbl) new-view))))

(defun mahogany-state-view-remove (state view-ptr)
  (declare (type mahogany-state state)
	   (type cffi:foreign-pointer view-ptr))
  (with-slots (views) state
    (alexandria:if-let ((view (gethash (cffi:pointer-address view-ptr) views)))
      (progn
	(group-remove-view (mahogany-current-group state) view)
	(remhash (cffi:pointer-address view-ptr) views))
      (log-string :error "Could not find mahogany view associated with pointer ~S" view-ptr))))

(defun mahogany-current-frame (state)
  (mahogany-group-current-frame (mahogany-current-group state)))
