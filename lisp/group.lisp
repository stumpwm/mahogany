(in-package #:mahogany)

(defun make-mahogany-group (name number hrt-server)
  (let* ((hrt-group (hrt:hrt-server-group-create hrt-server))
        (tiled-layer (tree:make-layer-container hrt-group)))
    (hrt:hrt-scene-group-set-enabled hrt-group nil)
    (log-string :debug "Created group ~A" name)
    (%make-mahogany-group name number hrt-group tiled-layer)))

(defun destroy-mahogany-group (group scene-tree seat)
  (group-suspend group seat)
  (with-accessors ((views mahogany-group-views)
                   (hrt-group mahogany-group-hrt-group)
                   (tiled-layer mahogany-group-tiled-container)
                   (group-name mahogany-group-name))
      group
    (when views
      (log-string :error "The following views are associated with a group that is being deleted. They will be orphaned:~%~4T ~S" views)
      (dolist (v views)
        (hrt:view-reparent v scene-tree)))
    (tree:destroy-layer-container tiled-layer)
    (hrt:hrt-scene-group-destroy hrt-group)
    (log-string :debug "Destroyed group ~A" group-name)))

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
    (setf (mahogany-group-active-p group) nil)
    (when focused-frame
      (tree:unmark-frame-focused focused-frame seat))
    (hrt:hrt-scene-group-set-enabled hrt-group nil)))

(defun group-wakeup (group seat)
  (declare (type mahogany-group group))
  (with-accessors ((focused-frame mahogany-group-current-frame)
                   (hrt-group mahogany-group-hrt-group))
      group
    (log-string :debug "Waking up group ~A" (mahogany-group-name group))
    (setf (mahogany-group-active-p group) nil)
    (when focused-frame
      (tree:mark-frame-focused focused-frame seat))
    (hrt:hrt-scene-group-set-enabled hrt-group t)))

(defun group-transfer-views (group to-transfer)
  "Transfer the all views from to-transfer to group"
  (declare (type mahogany-group group to-transfer))
  (let ((group-tile-layer (mahogany-group-tiled-container group))
        (to-transfer-layer (mahogany-group-tiled-container to-transfer))
        (hidden-list (mahogany-group-hidden-views group)))
    (tree:layer-container-transfer to-transfer-layer group-tile-layer)
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
  (declare (type hrt:output output)
           (type mahogany-group group))
  (with-accessors ((output-map mahogany-group-output-map)
                   (tiled-container mahogany-group-tiled-container)
                   (current-frame mahogany-group-current-frame)
                   (hidden-views mahogany-group-hidden-views))
      group
    (multiple-value-bind (x y)
        (hrt:output-position output)
      (multiple-value-bind (width height)
          (hrt:output-resolution output)
        (let ((new-tree (tree:tree-output-add tiled-container output
                                              :x x :y y
                                              :width width :height height)))
          (setf (gethash (hrt:output-full-name output) output-map) new-tree)
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
    (loop for output across outputs
          do (with-accessors ((full-name hrt:output-full-name))
                 output
               (alexandria:when-let ((tree (gethash full-name output-map)))
                 (tree:reconfigure-node tree output))))))

(defun %first-hash-table-value (table)
  (declare (type hash-table table)
           (optimize (speed 3) (safety 0)))
  (with-hash-table-iterator (iter table)
    (multiple-value-bind (found key value) (iter)
      (declare (ignore found key))
      value)))

(defun %group-current-output-node (group)
  (let* ((cur-frame (mahogany-group-current-frame group))
         (output-node (tree:find-root-frame cur-frame)))
    output-node))

(defun group-current-output (group)
  "Return the output that contains the group's currently focused frame"
  (tree:output-node-output (%group-current-output-node group)))

(defun group-remove-output (group output seat)
  (declare (type hrt:output output)
           (type mahogany-group group))
  (with-accessors ((output-map mahogany-group-output-map)
                   (hidden-views mahogany-group-hidden-views))
      group
    (let* ((output-name (hrt:output-full-name output))
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

(defun group-add-initialize-view (group view-ptr)
  (declare (type mahogany-group group)
           (type cffi:foreign-pointer view-ptr))
  (let* ((tiled-layer (mahogany-group-tiled-container group))
         (hrt-tiled-layer (tree:layer-container-layer tiled-layer))
         (view (hrt:view-init view-ptr)))
    (hrt:scene-layer-add-view hrt-tiled-layer view)
    (push view (mahogany-group-views group))
    ;; We need to send a configure event, so we might as well
    ;; guess the size of the window:
    (let ((focused-frame (mahogany-group-current-frame group)))
      ;; TODO: Try to guess when there is a full screen view visible:
      (if (and focused-frame (not (typep focused-frame 'tree:output-node)))
          (set-dimensions view (tree:frame-width focused-frame) (tree:frame-height focused-frame))
          (set-dimensions view 0 0)))
    view))

(defun group-map-view (group view)
  (declare (type mahogany-group group)
	   (type hrt:view view))
  (with-accessors ((views mahogany-group-views)
		   (outputs mahogany-group-output-map)
		   (hidden mahogany-group-hidden-views))
      group
    (alexandria:when-let ((current-frame (mahogany-group-current-frame group)))
      ;; By default, add new views to the tiled layer:
      (let* ((layer (mahogany-group-tiled-container group))
             (hrt-layer (tree:layer-container-layer layer)))
        (hrt:scene-layer-add-view hrt-layer view))
      (alexandria:when-let ((to-hide (tree:frame-view current-frame)))
        (%add-hidden hidden to-hide))
      (%swap-view-into-frame group current-frame view))))

(declaim (inline %find-view-frame))
(defun %find-view-frame (group view fn)
  (dolist (tree (tree:tree-children (mahogany-group-tiled-container group)))
    ;; TODO: use foreach-leaf here:
    (dolist (f (mahogany/tree:get-populated-frames tree))
      (when (equalp (tree:frame-view f) view)
        (funcall fn f)))))

(defun group-unmap-view (group view)
  (declare (type mahogany-group group))
  (with-accessors ((view-list mahogany-group-views)
                   (output-map mahogany-group-output-map)
                   (hidden mahogany-group-hidden-views))
      group
    (log-string :trace "unmapping view ~S" view)
    (alexandria:if-let ((f (tree:find-view-frame (mahogany-group-tiled-container group) view)))
      (hrt:with-view-transaction ()
        (etypecase f
          (tree:output-node
           (log-string :trace "unmapping fullscreen view ~S" view)
           (let ((to-replace (ring-list:peek-item hidden)))
             (cond
               ((and to-replace (hrt:view-fullscreen-p to-replace))
                (tree:set-fullscreen f (%pop-hidden-item hidden)))
               (t
                (%clear-fullscreen-state group f)
                ;; reset the current frame:
                (let ((to-focus (tree:find-focused-frame f)))
                  ;; Don't pull from the hidden list unless we land on an empty frame:
                  (when (and to-replace
                             (not (tree:frame-view to-focus)))
                    (setf (tree:frame-view to-focus) (%pop-hidden-item hidden)))
                  (setf (mahogany-group-current-frame group) to-focus)
                  (tree:mark-frame-focused to-focus (server-seat *compositor-state*)))))))
          (tree:view-frame
           (setf (tree:frame-view f) nil)
           (when (> (ring-list:ring-list-size hidden) 0)
             (%swap-view-into-frame group f (%pop-hidden-item hidden)))))
        (hrt:dirty-view-transaction))
      (ring-list:remove-item hidden view))))

(defun group-remove-view (group view)
  (declare (type mahogany-group group))
  (with-accessors ((view-list mahogany-group-views)
                   (output-map mahogany-group-output-map)
                   (hidden mahogany-group-hidden-views))
      group
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
  (let ((topmost-frame (mahogany/tree:find-topmost-frame frame)))
    (flet ((hide-and-disable (view-frame)
             (alexandria:when-let ((view (tree:frame-view view-frame)))
               (%add-hidden (mahogany-group-hidden-views group) view))))
      (tree:replace-frame topmost-frame frame #'hide-and-disable)))
  (hrt:dirty-view-transaction))

(defun group-maximize-current-frame (group)
  "Remove all of the splits in the current window tree and replae it with the
currently focused frame"
  (declare (type mahogany-group group))
  (let ((current-frame (mahogany-group-current-frame group)))
    (%maximize-frame group current-frame)))

(defstruct (%hidden-view-info (:constructor %make-hidden-view-info (output frame)))
  (output nil :type tree:output-node :read-only t)
  (frame nil :type tree:view-frame :read-only t))

(defun %hide-views-under-fullscreen (group output-node &optional (add-fun #'ring-list:add-item-prev))
  (declare (optimize (speed 3))
           (type mahogany-group group)
           (type tree:output-node output-node)
           (type (function (ring-list:ring-list T) (values fixnum)) add-fun))
  (let ((hidden-views (mahogany-group-hidden-views group)))
    (dolist (f (tree:get-populated-frames output-node))
      (let ((v (tree:frame-view f)))
        (unless (ring-list:contains-item hidden-views v)
          (funcall add-fun hidden-views v)
          (hrt:view-set-hidden v t))
        (setf (gethash v (mahogany-group-hidden-view-map group))
              (%make-hidden-view-info output-node f))))))

(defun %group-make-fullscreen (group view output)
  (declare (type (or null hrt:output) output)
           (type hrt:view view)
           (type mahogany-group group))
  (hrt:view-set-fullscreen view t)
  (alexandria:if-let ((frame (tree:find-view-frame
			      (mahogany-group-tiled-container group)
			      view)))
    ;; The frame is visible, immediately fullscreen it:
    (let ((output-node (if output
			   (gethash (hrt:output-full-name output)
                                    (mahogany-group-output-map group))
			   (%group-current-output-node group))))
      (unless output-node
        (mahogany/log:log-string :warn "Could not find output when making view fullscreen")
        (return-from %group-make-fullscreen nil))
      (setf (tree:frame-view frame) nil)
      (let ((prev-fullscreen (tree:set-fullscreen output-node view)))
        (cond
          (prev-fullscreen
           (%add-hidden (mahogany-group-hidden-views group) prev-fullscreen))
          (t
           ;; Since there wasn't a fullscreen node here previously, mark all the views
           ;; that it hides as hidden:
           (%hide-views-under-fullscreen group output-node))))
      ;; When a frame on the current output is focused, move the focus to the fullscreen
      ;; view:
      (when (eq (%group-current-output-node group)
                output-node)
        (tree:mark-frame-focused output-node (server-seat *compositor-state*))
        (setf (mahogany-group-current-frame group) output-node)))
    ;; Frame isn't visible; allow the view to be fullscreened and we will deal with it
    ;; when it becomes visible:
    (progn
      (log-string :trace "Allowing hidden view to become fullscreen: ~S" view)))
  ;; We always fulfull the request:
  t)

(defun %clear-fullscreen-state (group frame)
  "Unmark the output node frame and make the views that are under it visible.
After this function is ran, the current frame needs to be set and focused."
  (declare (type tree:output-node frame)
           (type mahogany-group group))
  (let ((hidden-map (mahogany-group-hidden-view-map group)))
    (loop :for hidden-view :being :the :hash-key
            :of hidden-map
              :using (hash-value data)
          :when (eql frame (%hidden-view-info-output data))
            :do (remhash hidden-view hidden-map)
                (hrt:view-set-hidden hidden-view nil)
                (ring-list:remove-item (mahogany-group-hidden-views group)
                                       hidden-view
                                       :test #'eql)))
  (tree:unmark-frame-focused frame (server-seat *compositor-state*))
  (tree:clear-fullscreen frame))

(defun %group-unfullscreen (group view)
  (declare (type mahogany-group group)
           (type hrt:view view))
  (hrt:view-set-fullscreen view nil)
  (alexandria:if-let ((frame (tree:find-view-frame
                              (mahogany-group-tiled-container group)
                              view)))
    ;; The frame is visible, we need to do some cleanup:
    (cond
      ((typep frame 'tree:output-node)
       (%clear-fullscreen-state group frame)
       (let ((to-focus (tree:find-focused-frame frame)))
         (setf (mahogany-group-current-frame group) to-focus)
         (alexandria:when-let ((prev (tree:frame-view to-focus)))
           (%add-hidden (mahogany-group-hidden-views group) prev))
         (setf (tree:frame-view to-focus) view)))
      (t
       ;; We aren't doing anything; communicate that to the calling code
       nil))
    (progn
      (log-string :trace "Make hidden view not fullscreen: ~S" view))))

(defun group-set-fullscreen (group view output set-fullscreen)
  (if set-fullscreen
      (%group-make-fullscreen group view output)
      (%group-unfullscreen group view)))

(defun %swap-view-into-frame (group current-frame view)
  (declare (type mahogany-group group)
           (type (or null hrt:view) view))
  (cond
    ((hrt:view-fullscreen-p view)
     (let* ((cur-output (%group-current-output-node group))
            (prev-fullscreen (tree:set-fullscreen cur-output view)))
       (unless prev-fullscreen
         (%hide-views-under-fullscreen group cur-output #'ring-list:add-item))
       (tree:mark-frame-focused cur-output (server-seat *compositor-state*))
       (setf (mahogany-group-current-frame group) cur-output)))
    (t
     (let ((hidden-data (gethash view (mahogany-group-hidden-view-map group))))
       (declare (type (or null %hidden-view-info) hidden-data))
       (etypecase current-frame
         (tree:output-node
          (%clear-fullscreen-state group current-frame)
          (cond
            ((and hidden-data
                  (eql (%hidden-view-info-output hidden-data) current-frame))
             (log-string :trace "Hidden view is visible under fullscreen view")
             ;; The view should be visible, focus its frame:
             (let ((to-focus (%hidden-view-info-frame hidden-data)))
               (setf (mahogany-group-current-frame group) to-focus)
               (tree:mark-frame-focused to-focus (server-seat *compositor-state*))))
            (t
             (let ((to-focus (tree:find-focused-frame current-frame)))
               (setf (mahogany-group-current-frame group) to-focus)
               (alexandria:when-let ((prev (tree:frame-view to-focus)))
                 (%add-hidden (mahogany-group-hidden-views group) prev))
               (setf (tree:frame-view to-focus) view)))))
         (tree:view-frame
          (when hidden-data
            (setf (tree:frame-view (%hidden-view-info-frame hidden-data))
                  nil)
            (remhash view (mahogany-group-hidden-view-map group)))
          (setf (tree:frame-view current-frame) view))))))
  (hrt:dirty-view-transaction))

(defun group-next-hidden (group)
  (declare (type mahogany-group group))
  (let ((current-frame (mahogany-group-current-frame group))
        (hidden-views (mahogany-group-hidden-views group))
        (next-view))
    (when (> (ring-list:ring-list-size hidden-views) 0)
      (alexandria:if-let ((view (tree:frame-view current-frame)))
        (setf next-view (%swap-next-hidden hidden-views view))
        (setf next-view (%pop-hidden-item hidden-views)))
      (%swap-view-into-frame group current-frame next-view))))

(defun group-previous-hidden (group)
  (declare (type mahogany-group group))
  (let ((current-frame (mahogany-group-current-frame group))
        (hidden-views (mahogany-group-hidden-views group))
        (next-view))
    (when (> (ring-list:ring-list-size hidden-views) 0)
      (alexandria:if-let ((view (tree:frame-view current-frame)))
        (setf next-view (%swap-prev-hidden hidden-views view))
        (setf next-view (%pop-hidden-item hidden-views)))
      (%swap-view-into-frame group current-frame next-view))))

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
  (declare (type mahogany-group group)
           (type hrt:view view))
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
  (declare (type mahogany-group group)
           (type hrt:view view))
  ;; attempt to stop abuse by only doing something
  ;; if the view is focused:
  (let* ((cur-frame (mahogany-group-current-frame group))
         (cur-view (tree:frame-view cur-frame)))
    (if (equal cur-view view)
        (progn
          (log-string :trace "\"minimizing\" view ~S" view)
          (group-next-hidden group))
        (hrt:view-configure view))))
