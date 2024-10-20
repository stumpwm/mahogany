(in-package #:mahogany)

(defmethod initialize-instance :after ((object mahogany-state) &key &allow-other-keys)
  (let ((default-group (make-mahogany-group "DEFAULT" 1)))
    (setf (slot-value object 'current-group) default-group)
    (vector-push-extend default-group (mahogany-state-groups object))))

(defun server-state-reset (state)
  (declare (type mahogany-state state))
  (setf (mahogany-state-server state) nil))

(defun server-stop (state)
  (declare (type mahogany-state state))
  (hrt:hrt-server-stop (mahogany-state-server state)))

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
	  do (group-add-output g mh-output))))

(defun mahogany-state-output-remove (state hrt-output)
  (with-accessors ((outputs mahogany-state-outputs)
		   (groups mahogany-state-groups))
      state
    (let ((mh-output (find hrt-output outputs
			   :key #'mahogany-output-hrt-output
			   :test #'cffi:pointer-eq)))
      (log-string :trace "Output removed ~S" (mahogany-output-full-name mh-output))
      ;; TODO: Is there a better way to remove an item from a vector when we know the index?
      (loop for g across groups
	    do (group-remove-output g mh-output))
      (setf outputs (delete mh-output outputs)))))

(defun mahogany-state-output-reconfigure (state)
  (log-string :trace "Output layout changed!")
  (with-accessors ((groups mahogany-state-groups)) state
    (loop for g across groups
	  do (group-reconfigure-outputs g (mahogany-state-outputs state)))))

(defun mahogany-state-view-add (state view)
  (declare (type mahogany-state state))
  (push view (slot-value state 'views))
  (group-add-view (mahogany-current-group state) view)
  (log-string :trace "Views: ~S" (slot-value state 'views)))

(defun mahogany-state-view-remove (state view)
  (declare (type mahogany-state state))
  (with-slots (views) state
    (group-remove-view (mahogany-current-group state) view)
    (setf views (remove view views :test #'cffi:pointer-eq))
    (log-string :trace "Views: ~S" views)))
