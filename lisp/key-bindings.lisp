(in-package #:mahogany)

(defcommand run-shell-command
    ((shell-command (:function interactively-read-string "Exec: ")))
  (:method ((exec string))
    (uiop:launch-program exec)))

(defcommand handle-server-stop ()
  (:method ()
    (server-stop *compositor-state*)))

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
  (:documentation "Split the current frame horizontally")
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
    (let ((frame (state-current-frame *compositor-state*)))
      (alexandria:when-let ((view (mahogany/tree:frame-view frame)))
        (hrt:view-request-close view)))))

(defcommand next-view ()
  (:documentation "Raise the next hidden view in the current group")
  (:method ()
    (state-next-hidden-frame *compositor-state*)))

(defcommand previous-view ()
  (:documentation "Raise the next hidden view in the current group")
  (:method ()
    (state-next-hidden-frame *compositor-state*)))

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

(defcommand gnew ()
  (:method ()
    (mahogany-state-group-add *compositor-state*)))

(defcommand gkill ()
  (:method ()
    (let ((current-group (state-current-group *compositor-state*)))
      (mahogany-state-group-remove *compositor-state* current-group))))

(defcommand gnext ()
  (:method ()
    (state-next-hidden-group *compositor-state*)))

(defcommand gprev ()
  (:method ()
    (state-next-hidden-group *compositor-state*)))

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

(defvar *group-map*
  (define-kmap
    (kbd "c") #'gnew
    (kbd "k") #'gkill
    (kbd "n") #'gnext
    (kbd "p") #'gprev))

(defvar *root-map*
  (define-kmap
    (kbd "!") #'run-shell-command
    (kbd ";") #'colon
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
