(in-package #:mahogany)

(defcommand run-shell-command
    ((shell-command (:function interactively-read-string :data "Exec")))
  (:method ((exec string))
    (uiop:launch-program exec)))

(defcommand handle-server-stop ()
  (:method ()
    (server-stop *compositor-state*)))

(defcommand shutdown-gracefully ()
  (:method ()
    (server-shutdown-gracefully *compositor-state*)))

(defcommand open-terminal ()
  (:method ()
    (mh-sys:open-terminal)))

(defcommand split-frame-h ()
  (:documentation "Split the current frame horizontally")
  (:method ()
    (let ((frame (state-current-frame *compositor-state*)))
      (when frame
        (tree:split-frame-h frame :direction :right)))))

(defcommand split-frame-v ()
  (:documentation "Split the current frame vertically")
  (:method ()
    (let ((frame (state-current-frame *compositor-state*)))
      (when frame
        (tree:split-frame-v frame :direction :bottom)))))

(defcommand maximize-current-frame ()
  (:method ()
    (let ((group (state-current-group *compositor-state*)))
      (group-maximize-current-frame group))))

(defcommand close-current-view ()
  (:method ()
    (alexandria:when-let*
        ((frame (state-current-frame *compositor-state*))
         (surface (tree:frame-surface frame)))
      (typecase surface
        (hrt:view
         (hrt:view-request-close surface))
        (hrt:layer-surface
         (hrt:layer-surface-close surface))))))

(defcommand next-view ()
  (:documentation "Raise the next hidden view in the current group")
  (:method ()
    (state-next-hidden-frame *compositor-state*)))

(defcommand previous-view ()
  (:documentation "Raise the previous hidden view in the current group")
  (:method ()
    (state-prev-hidden-frame *compositor-state*)))

(defcommand next-frame (seat)
  (:documentation
   "Set the current frame to the next one in the frame graph")
  (:method (seat)
    (let ((cur-frame (state-current-frame *compositor-state*)))
      (state-focus-frame *compositor-state* (tree:frame-next cur-frame) seat))))

(defcommand prev-frame (seat)
  (:documentation
   "Set the current frame to the previous one in the frame graph")
  (:method (seat)
    (let ((cur-frame (state-current-frame *compositor-state*)))
      (state-focus-frame *compositor-state* (tree:frame-prev cur-frame) seat))))

(defun interactively-read-new-group-name (com im arg prompt)
  (declare (ignore com arg))
  (let* ((next-name (state-next-group-name *compositor-state*))
         (full-prompt (concatenate 'string prompt " (default: " next-name ")")))
    (cl-interactive:input-method-read im full-prompt :require-match nil)))

(defcommand gnew
    ((name (:function interactively-read-new-group-name :data "Name")))
  (:method ((name string))
    (mahogany-state-group-add
     *compositor-state*
     :group-name (if (string= name "") nil name))))

(defcommand gnewbg
    ((name (:function interactively-read-new-group-name :data "Name")))
  (:method ((name string))
    (mahogany-state-group-add
     *compositor-state*
     :group-name (if (string= name "") nil name)
     :make-current nil)))

(defun interactively-read-group (com im arg prompt)
  (declare (ignore com arg))
  (let* ((groups (state-groups *compositor-state*))
         ;; It would be good to sort the list so that the current
         ;; group is on the bottom, mark the current group with a `*`,
         ;; or something similar to help user.
         (group-name (cl-interactive:completing-read
                      im prompt
                      :completions (map 'list #'mahogany-group-name groups)
                      :require-match t)))
    (find group-name groups :key #'mahogany-group-name :test #'string=)))

(defcommand grouplist
    ((group (:function interactively-read-group :data "Group?")))
  (:method (group)
    (setf (state-current-group *compositor-state*) group)))

(defcommand gkill ()
  (:method ()
    (let ((current-group (state-current-group *compositor-state*)))
      (mahogany-state-group-remove *compositor-state* current-group))))

(defcommand gnext ()
  (:method ()
    (state-next-hidden-group *compositor-state*)))

(defcommand gprev ()
  (:method ()
    (state-prev-hidden-group *compositor-state*)))

(defcommand grename
    ((new-name (:function interactively-read-string :data "New Name")))
  (:method (new-name)
    (let ((current-group (state-current-group *compositor-state*)))
      (setf (mahogany-group-name current-group) new-name))))

(defun %prep-move-cur-view ()
  (alexandria:when-let*
      ((current-frame (state-current-frame *compositor-state*))
       (surface (tree:frame-surface current-frame)))
    (when (typep surface 'hrt:layer-surface)
      (error 'mahogany/util:invalid-operation
             :text "Cannot move layer shell surfaces"))
    (let ((current-group (state-current-group *compositor-state*)))
      (unless (eq (mahogany-group-current-frame current-group)
                  current-frame)
        (error 'mahogany/util:mahogany-panic
               :text "Current tiled frame does not match current group"))
      (return-from %prep-move-cur-view (values current-group surface))))
  (error 'mahogany/util:invalid-operation
         :text "No view to move."))

(defun %move-current-surface (destination)
  (multiple-value-bind (cur-group surface)
      (%prep-move-cur-view)
    (group-move-view cur-group destination surface)))

(defcommand gmerge
    ((from (:function interactively-read-group :data "From Group")))
  (:documentation "Merge FROM into the current group. FROM is not deleted")
  (:method (from)
    (group-transfer-views (state-current-group *compositor-state*)
                          from)))

(defcommand gmove
    ((group (:function interactively-read-group :data "Group?")))
  (:documentation "Move the currently focused view to the specified group")
  (:method (destination)
    (%move-current-surface destination)))

(defcommand gmove-and-follow
    ((group (:function interactively-read-group :data "Group?")))
  (:documentation
   "Move the currently focused view to the specified group and switch to it")
  (:method (destination)
    (%move-current-surface destination)
    (setf (state-current-group *compositor-state*) destination)))

(defcommand gnext-with-window
    ()
  (:documentation
   "Cycle to the next group in the group list, taking the current window along.")
  (:method ()
    (multiple-value-bind (initial-group surface)
        (%prep-move-cur-view)
      (state-next-hidden-group *compositor-state*)
      (group-move-view
       initial-group
       (state-current-group *compositor-state*)
       surface))))

(defcommand gprev-with-window
    ()
  (:documentation
   "Cycle to the previous group in the group list, taking the current window along.")
  (:method ()
    (multiple-value-bind (initial-group surface)
        (%prep-move-cur-view)
      (state-prev-hidden-group *compositor-state*)
      (group-move-view
       initial-group
       (state-current-group *compositor-state*)
       surface))))

#+:hrt-debug
(defcommand add-output ()
  (:method ()
    (if (hrt:hrt-add-output (state-server *compositor-state*))
        (log-string :info "Output not added")
        (log-string :info "Output added"))))

#+:hrt-debug
(defvar *debug-map*
    (define-kmap
      (kbd "a") #'add-output))

(defvar *session-root-map*
  (define-kmap
    (kbd "q") (define-kmap
                (kbd "q") #'shutdown-gracefully
                (kbd "k") #'handle-server-stop)))

(define-kmap-mode session-cmd-mode
  :documentation "keybindings for quitting mahogany"
  :prefix-binding *session-root-map*)

(defvar *group-map*
  (define-kmap
    (kbd "c") #'gnew
    (kbd "k") #'gkill
    (kbd "n") #'gnext
    (kbd "N") #'gnext-with-window
    (kbd "l") #'grouplist
    (kbd "p") #'gprev
    (kbd "P") #'gprev-with-window
    (kbd "m") #'gmove
    (kbd "M") #'gmove-and-follow))

(defvar *root-map*
  (define-kmap
    (kbd "!") #'run-shell-command
    (kbd ";") #'colon
    (kbd "o") #'next-frame
    (kbd "O") #'prev-frame
    (kbd "k") #'close-current-view
    (kbd "c") #'open-terminal
    (kbd "s") #'split-frame-v
    (kbd "S") #'split-frame-h
    (kbd "Q") #'maximize-current-frame
    (kbd "n") #'next-view
    (kbd "p") #'previous-view
    (kbd "g") *group-map*))

(defvar *top-map* (define-kmap))

#+:hrt-debug
(progn
  (define-key *root-map* (kbd "d") *debug-map*))

;; Instead of using the macro, maybe we should define this manually
;; so users can't deactivate it?
(define-kmap-mode base-mode
  :documentation "Base mode for mahogany that contains the default keybindings"
  :top-binding *top-map*
  :prefix-binding *root-map*)

(define-kmap-mode prefix-passthrough-mode
  :documentation "Kmap mode that makes pressing the prefix key twice in a row
send the prefix key to the focused client."
  :prefix-binding *prefix-passthrough-kmap*)

(base-mode t)
(prefix-passthrough-mode t)
(session-cmd-mode t)
