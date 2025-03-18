(in-package #:mahogany)

(defvar *default-group-name* "DEFAULT")

(defun %add-group (state name index)
  (declare (type state mahogany-state)
           (type name string)
           (type index fixnum))
  (with-accessors ((groups mahogany-state-groups)
                   (current-group mahogany-current-group)
                   (server mahogany-state-server))
      state
    (let* ((scene-tree (hrt:hrt-server-scene-tree server))
           (default-group (make-mahogany-group name index scene-tree)))
      (vector-push-extend default-group groups)
      default-group)))

(defun server-state-init (state server output-callbacks seat-callbacks view-callbacks
                          &key (debug-level 3))
  (setf (mahogany-state-server state) server)
  (hrt:hrt-server-init server
                       output-callbacks seat-callbacks view-callbacks
                       debug-level)
  (let ((default-group (%add-group state *default-group-name* 1)))
    (setf (mahogany-current-group state) default-group)))

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

(defmethod (setf mahogany-current-group) :around (group state)
  (with-accessors ((hidden-groups mahogany-state-hidden-groups)
                   (server mahogany-state-server))
      state
    (when (not (find group (mahogany-state-groups state) :test #'equalp))
      (error (format nil "Group ~S is not part of this state" group)))
    (when (slot-boundp state 'current-group)
      (group-suspend (mahogany-current-group state) (hrt:hrt-server-seat server)))
    (call-next-method)
    (group-wakeup group (hrt:hrt-server-seat server))))

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
    (log-string :debug "New output added ~S" (mahogany-output-full-name mh-output))
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
      (log-string :debug "Output removed ~S" (mahogany-output-full-name mh-output))
      (loop for g across groups
            do (group-remove-output g mh-output (server-seat state)))
      ;; TODO: Is there a better way to remove an item from a vector when we could know the index?
      (setf outputs (delete mh-output outputs :test #'equalp)))))

(defun mahogany-state-group-add (state &key group-name (make-current t))
  (let ((index (length (mahogany-state-groups state))))
    (unless group-name
      (setf group-name (concatenate 'string "DEFAULT" "-" (write-to-string index))))
    (let ((new-group (%add-group state group-name index)))
      (with-accessors ((current-group mahogany-current-group)
                       (hidden-groups mahogany-state-hidden-groups)
                       (state-outputs mahogany-state-outputs))
          state
        (loop for o across state-outputs
              do (group-add-output new-group o (server-seat state)))
        (cond
          (make-current
           (ring-list:add-item hidden-groups current-group)
           (setf current-group new-group))
          (t
           (ring-list:add-item hidden-groups current-group)))
        (log-string :trace "Hidden groups: ~S" hidden-groups))
      new-group)))

(defun mahogany-state-group-remove (state group)
  (with-accessors ((groups mahogany-state-groups)
                   (hidden-groups mahogany-state-hidden-groups)
                   (current-group mahogany-current-group))
      state
    (when (= (length groups) 1)
      (log-string :warn "Cannot remove the only group")
      (return-from mahogany-state-group-remove))
    (if (find group groups :test #'equalp)
        (progn
          (cond
            ((equal group current-group)
             (setf current-group (ring-list:pop-item hidden-groups)))
            (t
             (ring-list:remove-item hidden-groups group)))
          (setf groups (delete group groups
                               :test #'equalp))
          (group-transfer-views current-group group)
          (let* ((server (mahogany-state-server state))
                 (scene-tree (hrt:hrt-server-scene-tree server)))
            (destroy-mahogany-group group scene-tree))
          (log-string :trace "Hidden groups: ~S" hidden-groups))
        (log-string :error "could not find group to delete"))))

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

(defun state-next-hidden-group (state)
  (declare (type mahogany-state state))
  (let ((current-group (mahogany-current-group state))
		(hidden-groups (mahogany-state-hidden-groups state)))
	(when (> (ring-list:ring-list-size hidden-groups) 0)
      (setf (mahogany-current-group state) (ring-list:swap-next hidden-groups current-group))
	  (log-string :trace "Hidden groups: ~S" hidden-groups))))

(defun state-prev-hidden-group (state)
  (declare (type mahogany-state state))
  (let ((current-group (mahogany-current-group state))
        (hidden-groups (mahogany-state-hidden-groups state)))
    (when (> (ring-list:ring-list-size hidden-groups) 0)
      (setf (mahogany-current-group state) (ring-list:swap-previous hidden-groups current-group)))))

(defun mahogany-current-frame (state)
  (mahogany-group-current-frame (mahogany-current-group state)))
