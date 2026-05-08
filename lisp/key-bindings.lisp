(in-package #:mahogany)

(cl-interactive:define-command run-shell-command
    ((shell-command (:function interactively-read-string "Exec: ")))
  (:method ((exec string))
    (uiop:launch-program exec)))

(cl-interactive:define-command handle-server-stop ()
  (:method ()
    (server-stop *compositor-state*)))

(cl-interactive:define-command open-terminal ()
  (:method ()
    (mh-sys:open-terminal)))

(cl-interactive:define-command split-frame-h ()
  (:method ()
    (let ((frame (mahogany-current-frame *compositor-state*)))
      (when frame
        (tree:split-frame-h frame :direction :right)))))

(cl-interactive:define-command split-frame-v ()
  (:method ()
    (let ((frame (mahogany-current-frame *compositor-state*)))
      (when frame
        (tree:split-frame-v frame :direction :bottom)))))

(cl-interactive:define-command maximize-current-frame ()
  (:method ()
    (let ((group (mahogany-current-group *compositor-state*)))
      (group-maximize-current-frame group))))

(cl-interactive:define-command close-current-view ()
  (:method ()
    (let ((frame (mahogany-current-frame *compositor-state*)))
      (alexandria:when-let ((view (mahogany/tree:frame-view frame)))
        (hrt:view-request-close view)))))

(cl-interactive:define-command next-view ()
  (:method ()
    (let ((group (mahogany-current-group *compositor-state*)))
      (group-next-hidden group)))
  (:documentation "Raise the next hidden view in the current group"))

(cl-interactive:define-command previous-view ()
  (:method ()
    (let ((group (mahogany-current-group *compositor-state*)))
      (group-previous-hidden group)))
  (:documentation "Raise the next hidden view in the current group"))

(cl-interactive:define-command next-frame ()
  (:method ()
    (let ((group (mahogany-current-group *compositor-state*)))
      (group-next-frame group *current-seat*))))

(cl-interactive:define-command prev-frame ()
  (:method ()
    (let ((group (mahogany-current-group *compositor-state*)))
      (group-prev-frame group *current-seat*))))

(cl-interactive:define-command gnew ()
  (:method ()
    (mahogany-state-group-add *compositor-state*)))

(cl-interactive:define-command gkill ()
  (:method ()
    (let ((current-group (mahogany-current-group *compositor-state*)))
      (mahogany-state-group-remove *compositor-state* current-group))))

(cl-interactive:define-command gnext ()
  (:method ()
    (state-next-hidden-group *compositor-state*)))

(cl-interactive:define-command gprev ()
  (:method ()
    (state-next-hidden-group *compositor-state*)))

#+:hrt-debug
(cl-interactive:define-command add-output ()
  (:method ()
    (if (hrt:hrt-add-output (mahogany-state-server *compositor-state*))
        (log-string :info "Output not added")
        (log-string :info "Output added"))))

#+:hrt-debug
(defvar *debug-map*
    (define-kmap
      (kbd "a") #'add-output))

(defvar *group-map*
  (define-kmap
    (kbd "c") #'gnew
    (kbd "k") #'gkill
    (kbd "n") #'gnext
    (kbd "p") #'gprev))

(defvar *root-map*
  (define-kmap
    (kbd "!") #'run-shell-command
    (kbd "o") #'next-frame
    (kbd "O") #'prev-frame
    (kbd "q") #'handle-server-stop
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
