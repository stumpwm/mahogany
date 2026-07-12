(in-package #:mahogany)

(defvar *default-group-name* "DEFAULT")

;; If there turns out to be more choices than
;; minimize and maximize, make this a bitmask or separate them out:
(config-system:defconfig *follow-config-events* :all
  (member :none :all :minimize :maximize)
  "The client window management events that the compositor listends to.")

(defun %add-group (state name index)
  (declare (type mahogany-state state)
           (type string name)
           (type fixnum index))
  (with-accessors ((groups state-groups)
                   (current-group state-current-group)
                   (server state-server))
      state
    (let* ((default-group (make-mahogany-group name index server)))
      (setf (mahogany-group-active-p default-group) t)
      (vector-push-extend default-group groups)
      default-group)))

(defun server-state-init (state server output-callbacks seat-callbacks view-callbacks
			  layer-shell-callbacks
                          &key (debug-level 3))
  (setf (state-server state) server)
  (hrt:server-init server
                       output-callbacks seat-callbacks view-callbacks
		       layer-shell-callbacks
                       debug-level)
  (let ((default-group (%add-group state *default-group-name* 1)))
    (setf (state-current-group state) default-group)))

(defun server-state-reset (state)
  (declare (type mahogany-state state))
  (with-accessors ((groups state-groups)
                   (server state-server))
      state
    ;; Clear the current frame so that subsequent cleanup code
    ;; doesn't try to switch focus to an invalid frame:
    (setf (state-%current-frame state) nil)
    (let ((scene-tree (hrt:hrt-server-scene-tree server))
          (seat (hrt:hrt-server-seat server)))
      (loop for g across groups
            :do (destroy-mahogany-group g scene-tree seat)))
    (setf groups (adjust-array groups 0 :fill-pointer 0))
    (hrt:server-finish server)
    ;; The actual scene object is freed during hrt-server-finish:
    (setf server nil)))

(defun server-stop (state)
  (declare (type mahogany-state state))
  (hrt:hrt-server-stop (state-server state)))

(defun (setf state-current-group) (group state)
  (declare (type mahogany-state state)
           (type mahogany-group group))
  (with-accessors ((hidden-groups state-hidden-groups)
                   (server state-server))
      state
    (when (not (find group (state-groups state) :test #'equalp))
      (error (format nil "Group ~S is not part of this state" group)))
    (when (state-%current-group state)
      (group-suspend (state-current-group state) (hrt:hrt-server-seat server)))
    (setf (state-%current-group state) group)
    (group-wakeup group (hrt:hrt-server-seat server))
    (alexandria:when-let ((group-frame (mahogany-group-current-frame group)))
      (state-focus-frame state group-frame (server-seat state)))
    (hrt:dirty-view-transaction)))

(defun group-frame-p (frame)
  ;; TODO: It might be better to start storing the
  ;; group that a frame is in, so we can use that
  ;; instead of just relying on the type. It would
  ;; also make it possible to pass arbitrary frames
  ;; to state-focus-frame
  (not (typep frame 'tree::layer-shell-frame)))

(defun state-focus-frame (state new-frame seat)
  "Focus the given frame. The frame must either be a member of
the current group or a layer shell frame"
  (with-accessors ((cur-frame state-%current-frame)
                   (cur-group state-current-group))
      state
    (unless (eql cur-frame new-frame)
      ;; Determine if the frame is in a group:
      (let ((cur-in-group (group-frame-p cur-frame))
            (next-in-group (group-frame-p new-frame)))
        (if next-in-group
            (group-focus-frame cur-group new-frame seat)
            (progn
              (when cur-in-group
                (group-unfocus cur-group seat))
              (tree:mark-frame-focused new-frame seat))))
      (setf cur-frame new-frame))))

(defun server-keystate-reset (state)
  (setf (state-key-state state)
        (make-key-state (state-keybindings state))))

(defun (setf state-keybindings) (kmaps state)
  (declare (type list kmaps)
           (type mahogany-state state))
  (setf (state-%keybindings state) kmaps)
  (unless (key-state-active-p (state-key-state state))
    (server-keystate-reset state)))

(defun mahogany-state-output-add (state hrt-output)
  (declare (type mahogany-state state)
           (type cffi:foreign-pointer hrt-output))
  (with-accessors ((outputs state-outputs)
                   (groups state-groups)
                   (scene mahogany-state-scene))
      state
    (let* ((mh-output (hrt:make-output hrt-output))
           (output-container (tree::make-output-container mh-output)))
      (log-string :debug "New output added ~S" (hrt:output-full-name mh-output))
      ;; (mahogany/output-config::
      (hrt:output-init mh-output
                       (let ((output-match-data (find-output-config mh-output)))
                         (if output-match-data
                             (mahogany/output-config::output-match-data-config output-match-data)
                             nil)))
      (vector-push-extend output-container outputs)
      (loop for g across groups
            do (group-add-output g output-container))
      (unless (state-%current-frame state)
        (let ((cur-group (state-current-group state)))
          (group-focus cur-group (server-seat state))
        (setf (state-%current-frame state) (mahogany-group-current-frame cur-group)))))))

(declaim (inline %find-output))
(defun %find-output (hrt-output state)
  (declare (type mahogany-state state))
  (find hrt-output (state-outputs state)
        :key #'tree::output-container-output-ptr
        :test #'cffi:pointer-eq))

(defun mahogany-state-output-remove (state hrt-output)
  (with-accessors ((outputs state-outputs)
                   (groups state-groups)
                   (cur-frame state-%current-frame))
      state
    (let* ((output-container (%find-output hrt-output state))
          (mh-output (tree::output-container-output output-container)))
      (log-string :debug "Output removed ~S" (hrt:output-full-name mh-output))
      (loop for g across groups
            do (group-remove-output g output-container (server-seat state)))
      ;; TODO: Is there a better way to remove an item from a vector when we could know the index?
      (setf outputs (delete output-container outputs :test #'equalp))
      (hrt:destroy-output mh-output))
    (setf cur-frame (mahogany-group-current-frame
                     (state-current-group state)))
    (when (and cur-frame (> (length outputs) 0))
      (tree:mark-frame-focused cur-frame (server-seat state)))))

(defun mahogany-state-group-add (state &key group-name (make-current t))
  (let ((index (+ 1 (length (state-groups state)))))
    (unless group-name
      (setf group-name (concatenate 'string "DEFAULT" "-" (write-to-string index))))
    (let ((new-group (%add-group state group-name index)))
      (with-accessors ((current-group state-current-group)
                       (hidden-groups state-hidden-groups)
                       (state-outputs state-outputs))
          state
        (loop for o across state-outputs
              do (group-add-output new-group o))
        (cond
          (make-current
           (ring-list:add-item hidden-groups current-group)
           (setf current-group new-group))
          (t
           (ring-list:add-item hidden-groups current-group)))
        (log-string :trace "Hidden groups: ~S" hidden-groups))
      new-group)))

(defun mahogany-state-group-remove (state group)
  (with-accessors ((groups state-groups)
                   (hidden-groups state-hidden-groups)
                   (current-group state-current-group))
      state
    (when (= (length groups) 1)
      (log-string :warn "Cannot remove the only group")
      (return-from mahogany-state-group-remove))
    (if (find group groups :test #'equalp)
        (let* ((server (state-server state))
               (scene-tree (hrt:hrt-server-scene-tree server))
               (seat (hrt:hrt-server-seat server)))
          (cond
            ((equal group current-group)
             (let ((new-current (ring-list:pop-item hidden-groups)))
               (setf current-group new-current)
               (group-wakeup new-current seat)))
            (t
             (ring-list:remove-item hidden-groups group)))
          (setf groups (delete group groups
                               :test #'equalp))
          (group-transfer-views current-group group)
          (destroy-mahogany-group group scene-tree seat)
          (log-string :trace "Hidden groups: ~S" hidden-groups))
        (log-string :error "could not find group to delete"))))

(defun mahogany-state-output-reconfigure (state)
  (hrt:with-view-transaction ()
    (with-accessors ((groups state-groups)) state
      (loop for g across groups
            do (group-reconfigure-outputs g (state-outputs state))))))

(defun mahogany-state-layers-arrange (state output)
  (declare (type mahogany-state state))
  (let ((hrt-output (tree:output-container-output output)))
    (log-string :debug "layer shell layers re-arranged on output ~S"
              (hrt:output-full-name hrt-output))
    (hrt:with-view-transaction ()
      (with-accessors ((groups state-groups)) state
        (loop for g across groups
              do (group-rearrange-output g hrt-output))))))

(defun mahogany-state-view-add (state view-ptr)
  (declare (type mahogany-state state)
           (type cffi:foreign-pointer view-ptr))
  (with-accessors ((view-tbl state-views)
                   (current-group state-current-group)
                   (server state-server))
      state
    (let ((new-view (group-add-initialize-view current-group view-ptr)))
      (setf (gethash (cffi:pointer-address view-ptr) view-tbl) new-view))))

(defun %find-view-in-groups (view groups)
  (find-if (lambda (g) (member view (mahogany-group-views g))) groups))

(defmacro %with-found-group (state (group-var view) &body body)
  `(alexandria:if-let ((,group-var (%find-view-in-groups ,view (state-groups ,state))))
     (progn
       ,@body)
     (log-string :error "Could not find mahogany view in any groups!")))

(defun mahogany-state-view-remove (state view-ptr)
  (declare (type mahogany-state state)
           (type cffi:foreign-pointer view-ptr))
  (with-slots (views groups) state
    (alexandria:if-let ((view (gethash (cffi:pointer-address view-ptr) views)))
      (progn
        (%with-found-group state (group view)
          (group-remove-view group view))
        (remhash (cffi:pointer-address view-ptr) views))
      (log-string :error "Could not find mahogany view associated with pointer ~S" view-ptr))))

(defun %cur-frame-set-from-group (state group)
  (declare (type mahogany-state state)
           (type mahogany-group group))
  (let ((cur-frame (mahogany-group-current-frame group)))
    (setf (state-%current-frame state) cur-frame)
    (tree:mark-frame-focused cur-frame (server-seat state))))

(defun mahogany-state-view-fullscreen (state view output set-fullscreen)
  (declare (type mahogany-state state)
	       (type hrt:view view))
  (%with-found-group state (group view)
    (log-string :debug
                "~@<Fullscreen requested (~:[no~;yes~]):~I ~:_view ~S ~:_on output ~S~:>"
                set-fullscreen view output)
	(group-set-fullscreen group view output set-fullscreen)
    (%cur-frame-set-from-group state group)))

(defun mahogany-state-view-map (state view)
  (declare (type mahogany-state state)
           (type hrt:view view))
  (%with-found-group state (group view)
    (group-map-view group view)
    ;; Mapping the view can cause focus to change:
    (%cur-frame-set-from-group state group)))

(defun mahogany-state-view-unmap (state view)
  (declare (type mahogany-state state)
           (type hrt:view view))
  (%with-found-group state (group view)
    (group-unmap-view group view)
    ;; Unmapping the view can cause focus to change:
    (%cur-frame-set-from-group state group)))

(defun mahogany-state-view-size-changed (state view)
  (declare (type mahogany-state state)
	       (type hrt:view view)
           (ignore state view))
  ;; For now, just check to see if the view is now under the cursor:
  (hrt:dirty-view-transaction)
  ;; TODO: Center the view in its frame if the dimensions do not match. May
  ;;  also need to do something if it's now bigger than the frame:
  )

(defun mahogany-state-view-maximize (state view)
  (declare (type mahogany-state state)
           (type hrt:view view))
  (%with-found-group state (group view)
    (if (member *follow-config-events* '(:all :maximize))
        (group-maximize-view group view)
        (hrt:view-configure view))))

(defun mahogany-state-view-minimize (state view)
  (declare (type mahogany-state state)
           (type hrt:view view))
  (%with-found-group state (group view)
    (if (member *follow-config-events* '(:all :minimize))
        (group-minimize-view group view)
        (hrt:view-configure view))))

(defun state-next-hidden-frame (state)
  (let ((group (state-current-group *compositor-state*)))
    (group-next-hidden group)
    (%cur-frame-set-from-group state group)))

(defun state-prev-hidden-frame (state)
  (let ((group (state-current-group *compositor-state*)))
    (group-previous-hidden group)
    (%cur-frame-set-from-group state group)))

(defun state-next-hidden-group (state)
  (declare (type mahogany-state state))
  (let ((current-group (state-current-group state))
        (hidden-groups (state-hidden-groups state)))
    (when (> (ring-list:ring-list-size hidden-groups) 0)
      (setf (state-current-group state) (ring-list:swap-next hidden-groups current-group))
      (log-string :trace "Hidden groups: ~S" hidden-groups))))

(defun state-prev-hidden-group (state)
  (declare (type mahogany-state state))
  (let ((current-group (state-current-group state))
        (hidden-groups (state-hidden-groups state)))
    (when (> (ring-list:ring-list-size hidden-groups) 0)
      (setf (state-current-group state) (ring-list:swap-previous hidden-groups current-group)))))

(defun state-grab-seat (state)
  (declare (type mahogany-state state))
  (let ((seat (server-seat state)))
    (unless (hrt:seat-grabbed-p seat)
      (tree:unmark-frame-focused (state-%current-frame state)
                                 seat)
      (hrt:seat-grab seat "help"))))

(defun state-ungrab-seat (state)
  (declare (type mahogany-state state))
  (let ((seat (server-seat state)))
    (hrt:hrt-seat-ungrab seat)
    (tree:mark-frame-focused (state-%current-frame state)
                               seat)))

(defun mahogany-set-keymap (state &key (rules (cffi:null-pointer)) (keymap-flags :no-flags))
  "Set the xkb keymap using the provided rules and flags. Signals a
KEYMAP-CREATION-ERROR if the rules are invalid or malformed."
  (let* ((seat (hrt:hrt-server-seat (state-server state)))
	 (success (hrt:hrt-seat-set-keymap seat rules keymap-flags)))
    (unless success
      ;; TODO: register with xkb's logging handling to get
      ;; some sort of usable error message?
      ;; Either that, or just turn this into a warning or return a boolean?
      (error 'xkb:keymap-creation-error))))

(declaim (ftype (function (mahogany-state cffi:foreign-pointer) hrt:output)))
(defun %get-or-autoassign-output (state hrt-layer-shell)
  (declare (type mahogany-state state))
  (alexandria:if-let ((hrt-output (hrt:hrt-layer-surface-output hrt-layer-shell)))
    (tree:output-container-output (%find-output hrt-output state))
    (let ((current-output (group-current-output (state-current-group state))))
      ;; TODO: try to use the fallback output:
      (unless current-output
	    (log-string :error "Could not auto-assign output to layer surface")
	    (return-from %get-or-autoassign-output nil))
      (hrt:hrt-layer-shell-surface-set-output hrt-layer-shell
                                              (hrt:output-hrt-output current-output))
      current-output)))

(defun mahogany-state-layer-shell-handle (state hrt-layer-shell)
  (declare (type mahogany-state state))
  (alexandria:if-let ((output (%get-or-autoassign-output state hrt-layer-shell)))
    (progn
      (hrt:layer-shell-surface-place hrt-layer-shell
                                     output)
      (hrt:hrt-layer-shell-finish-init hrt-layer-shell))
    (hrt:hrt-layer-shell-surface-abort hrt-layer-shell)))

(defun state-layer-surface-add (state hrt-layer-surface)
  (declare (type mahogany-state state))
  (let* ((surfaces (state-layer-surfaces state))
         (new-surface (hrt:make-layer-surface hrt-layer-surface))
         (output-container (%find-output (hrt:layer-surface-output new-surface)
                                         state))
         (interactivity (hrt:layer-surface-keyboard-interactivity new-surface)))
    (setf (gethash (cffi:pointer-address hrt-layer-surface) surfaces) new-surface)
    (log-string
     :info
     "New Layer surface on output ~S~%, layer ~S with keyboard ~S keyboard"
     (tree::output-container-output output-container)
     (hrt::layer-surface-layer new-surface)
     interactivity)
    ;; If the surface can't be focused, don't do anything to place it.
    ;; Otherwise, add it to the tree data structure so we can navigate
    ;; to it by keyboard. The only difference between exclusive and
    ;; "on demand" surfaces is that we focus exclusive surfaces when
    ;; they appear.
    ;; We need to do something different
    ;; in the future for bottom or background surfaces.
    (unless (eq :layer-shell-keyboard-none interactivity)
      (let ((add-node (tree::output-container-add-layer-shell output-container new-surface)))
        (when (eq :layer-shell-keyboard-exclusive interactivity)
          (state-focus-frame state add-node (server-seat state)))))))

(defun state-layer-surface-remove (state hrt-layer-surface)
  (declare (type mahogany-state state))
  (let* ((surfaces (state-layer-surfaces state))
         (surface-ptr (cffi:pointer-address hrt-layer-surface))
         (surface (gethash surface-ptr surfaces))
         (output-container (%find-output (hrt:layer-surface-output surface)
                                         state))
         (removed-frame (tree::output-container-remove-layer-shell
                         output-container surface)))
    (when (eq (state-current-frame state) removed-frame)
      (state-focus-frame state (mahogany-group-current-frame
                                (state-current-group state))
                         (server-seat state)))
    (remhash surface-ptr surfaces)))
