(in-package #:mahogany)

(defun make-mahogany-group (name number hrt-scene)
  (let ((hrt-group (hrt:hrt-scene-group-create hrt-scene)))
    (hrt:hrt-scene-group-set-enabled hrt-group nil)
    (log-string :debug "Created group ~A" name)
    (%make-mahogany-group name number hrt-group)))

(defun destroy-mahogany-group (group scene-tree seat)
  (group-suspend group seat)
  (alexandria:when-let ((views (mahogany-group-views group)))
    (log-string :error "The following views are associated with a group that is being deleted. They will be orphaned:~%~4T ~S" views)
    (dolist (v views)
      (hrt:view-reparent v scene-tree)))
  (hrt:hrt-scene-group-destroy (mahogany-group-hrt-group group))
  (log-string :debug "Destroyed group ~A" (mahogany-group-name group)))

(defun %add-hidden (hidden-list view)
  (log-string :trace "Hiding view ~S" view)
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

(defun group-suspend (group seat)
  (declare (type mahogany-group group))
  (with-accessors ((focused-frame mahogany-group-current-frame)
                   (hrt-group mahogany-group-hrt-group))
      group
    (log-string :debug "Suspending group ~A" (mahogany-group-name group))
    (when focused-frame
      (tree:unmark-frame-focused focused-frame seat))
    (hrt:hrt-scene-group-set-enabled hrt-group nil)))

(defun group-wakeup (group seat)
  (declare (type mahogany-group group))
  (with-accessors ((focused-frame mahogany-group-current-frame)
                   (hrt-group mahogany-group-hrt-group))
      group
    (log-string :debug "Waking up group ~A" (mahogany-group-name group))
    (when focused-frame
      (tree:mark-frame-focused focused-frame seat))
    (hrt:hrt-scene-group-set-enabled hrt-group t)))

(defun group-transfer-views (group to-transfer)
  "Transfer the all views from to-transfer to group"
  (declare (type mahogany-group group to-transfer))
  (let ((hrt-group (mahogany-group-hrt-group group))
        (to-transfer-group (mahogany-group-hrt-group to-transfer))
        (hidden-list (mahogany-group-hidden-views group)))
    (hrt:hrt-scene-group-transfer to-transfer-group hrt-group)
    (dolist (other-view (mahogany-group-views to-transfer))
      (group-remove-view to-transfer other-view)
      (push other-view (mahogany-group-views group))
      (when (hrt:view-mapped-p other-view)
        (%add-hidden hidden-list other-view)))))

(defun group-focus-frame (group frame seat)
  (with-accessors ((current-frame mahogany-group-current-frame)) group
    (unless (eql current-frame frame)
      (when current-frame
        (group-unfocus-frame group current-frame seat))
      (tree:mark-frame-focused frame seat)
      (setf current-frame frame))))

(defun group-unfocus-frame (group frame seat)
  (with-accessors ((current-frame mahogany-group-current-frame)) group
    (tree:unmark-frame-focused frame seat)
    (setf current-frame nil)))

(defun group-add-output (group output seat)
  (declare (type mahogany-output output)
           (type mahogany-group group))
  (with-accessors ((output-map mahogany-group-output-map)
                   (tree-container mahogany-group-tree-container)
                   (current-frame mahogany-group-current-frame)
                   (hidden-views mahogany-group-hidden-views))
      group
    (multiple-value-bind (x y)
        (hrt:output-position (mahogany-output-hrt-output output))
      (multiple-value-bind (width height)
          (hrt:output-resolution (mahogany-output-hrt-output output))
        (let ((new-tree (tree:tree-container-add tree-container output
                                                 :x x :y y
                                                 :width width :height height)))
          (setf (gethash (mahogany-output-full-name output) output-map) new-tree)
          (when (not current-frame)
            (let ((first-leaf (tree:find-first-leaf new-tree)))
              (group-focus-frame group first-leaf seat)
              (alexandria:when-let ((spare (%pop-hidden-item hidden-views)))
                (setf (tree:frame-view first-leaf) spare)))))))
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
                 (tree:reconfigure-node tree hrt-output))))))

(defun %first-hash-table-value (table)
  (declare (type hash-table table)
           (optimize (speed 3) (safety 0)))
  (with-hash-table-iterator (iter table)
    (multiple-value-bind (found key value) (iter)
      (declare (ignore found key))
      value)))

(defun group-current-output (group)
  "Return the output that contains the group's currently focused frame"
  (let* ((cur-frame (mahogany-group-current-frame group))
         (output-node (tree:frame-parent (tree:find-root-frame cur-frame))))
    (tree:output-node-output output-node)))

(defun group-remove-output (group output seat)
  (declare (type mahogany-output output)
           (type mahogany-group group))
  (with-accessors ((output-map mahogany-group-output-map)
                   (hidden-views mahogany-group-hidden-views))
      group
    (let* ((output-name (mahogany-output-full-name output))
           (tree (gethash output-name output-map)))
      (remhash output-name output-map)
      (when (equalp output (group-current-output group))
        (group-unfocus-frame group (mahogany-group-current-frame group) seat)
        (alexandria:when-let ((other-tree (%first-hash-table-value output-map)))
          (group-focus-frame group (tree:find-first-leaf other-tree) seat)))
      (when (and (mahogany-group-current-frame group) (= 0 (hash-table-count output-map)))
        (group-unfocus-frame group (mahogany-group-current-frame group) seat))
      (tree:remove-frame tree (lambda (x) (alexandria:when-let ((v (tree:frame-view x)))
                                            (%add-hidden hidden-views v)))))))

(defun %group-add-view (group view)
  (declare (type mahogany-group group)
           (type hrt:view view))
  (with-accessors ((views mahogany-group-views)
                   (outputs mahogany-group-output-map)
                   (hidden mahogany-group-hidden-views))
      group
    (alexandria:when-let ((current-frame (mahogany-group-current-frame group)))
      (alexandria:when-let ((view (tree:frame-view current-frame)))
        (%add-hidden hidden view))
      (setf (tree:frame-view current-frame) view)
      (dirty-view-transaction))))

(defun group-add-initialize-view (group view-ptr)
  (declare (type mahogany-group group)
           (type cffi:foreign-pointer view-ptr))
  (let* ((hrt-group (mahogany-group-hrt-group group))
         (view (hrt:scene-init-view hrt-group view-ptr)))
    (push view (mahogany-group-views group))
    ;; We need to send a configure event, so we might as well
    ;; guess the size of the window:
    (with-accessors ((focused-frame mahogany-group-current-frame))
        group
      (if focused-frame
          (set-dimensions view (tree:frame-width focused-frame) (tree:frame-height focused-frame))
          (set-dimensions view 0 0)))
    view))

(defun group-map-view (group view)
  (%group-add-view group view))

(declaim (inline %find-view-frame))
(defun %find-view-frame (group view fn)
  (dolist (tree (tree:tree-children (mahogany-group-tree-container group)))
    (dolist (f (mahogany/tree:get-populated-frames tree))
      (when (equalp (tree:frame-view f) view)
        (funcall fn f)))))

(defun %group-remove-view (group view)
  (declare (type mahogany-group group))
  (with-accessors ((view-list mahogany-group-views)
                   (output-map mahogany-group-output-map)
                   (hidden mahogany-group-hidden-views))
      group
    (flet ((remove-and-populate (f)
             (setf (tree:frame-view f) nil)
             (alexandria:when-let ((new-view (%pop-hidden-item hidden)))
               (setf (tree:frame-view f) new-view))))
      (%find-view-frame group view #'remove-and-populate))
    (ring-list:remove-item hidden view)
    (dirty-view-transaction)))

(defun group-unmap-view (group view)
  (%group-remove-view group view))

(defun group-remove-view (group view)
  (declare (type mahogany-group group))
  (with-accessors ((view-list mahogany-group-views)
                   (output-map mahogany-group-output-map)
                   (hidden mahogany-group-hidden-views))
      group
    (%group-remove-view group view)
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

(defun %maximize-frame (group frame)
  (declare (type mahogany-group group))
  (let ((tree-root (mahogany/tree:find-root-frame frame)))
    (flet ((hide-and-disable (view-frame)
             (alexandria:when-let ((view (tree:frame-view view-frame)))
               (%add-hidden (mahogany-group-hidden-views group) view))))
      (tree:replace-frame tree-root frame #'hide-and-disable)))
  (dirty-view-transaction))

(defun group-maximize-current-frame (group)
  "Remove all of the splits in the current window tree and replae it with the
currently focused frame"
  (declare (type mahogany-group group))
  (let ((current-frame (mahogany-group-current-frame group)))
    (%maximize-frame group current-frame)))

(defun group-set-fullscreen (group view output set-fullscreen)
  ;; TODO: implement me!
  nil ; return nil to show no action was taken
  )

(defun group-next-hidden (group)
  (declare (type mahogany-group group))
  (let ((current-frame (mahogany-group-current-frame group))
        (hidden-views (mahogany-group-hidden-views group))
        (next-view))
    (when (> (ring-list:ring-list-size hidden-views) 0)
      (alexandria:if-let ((view (tree:frame-view current-frame)))
        (setf next-view (%swap-next-hidden hidden-views view))
        (setf next-view (%pop-hidden-item hidden-views)))
      (setf (tree:frame-view current-frame) next-view)
      (dirty-view-transaction))))

(defun group-previous-hidden (group)
  (declare (type mahogany-group group))
  (let ((current-frame (mahogany-group-current-frame group))
        (hidden-views (mahogany-group-hidden-views group))
        (next-view))
    (when (> (ring-list:ring-list-size hidden-views) 0)
      (alexandria:if-let ((view (tree:frame-view current-frame)))
        (setf next-view (%swap-prev-hidden hidden-views view))
        (setf next-view (%pop-hidden-item hidden-views)))
      (setf (tree:frame-view current-frame) next-view)
      (dirty-view-transaction))))

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

(defun group-maximize-view (group view)
  ;; attempt to stop abuse by only listening when the
  ;; frame requesting this info is focused:
  (alexandria:if-let ((frame (tree:find-view-frame
                              (mahogany-group-current-frame group)
                              view)))
    (progn
      (log-string :trace "maximizing view ~S" view)
      (%maximize-frame group frame))
    (hrt:view-configure view)))

(defun group-minimize-view (group view)
  ;; attempt to stop abuse by only doing something
  ;; if the view is focused:
  (let* ((cur-frame (mahogany-group-current-frame group))
         (cur-view (tree:frame-view cur-frame)))
    (if (equal cur-view view)
        (progn
          (log-string :trace "\"minimizing\" view ~S" view)
          (group-next-hidden group))
        (hrt:view-configure view))))
