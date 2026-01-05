(in-package #:mahogany)

(defcommand handle-server-stop ()
  (server-stop *compositor-state*))

(defcommand open-terminal ()
  (mh-sys:open-terminal))

(defcommand split-frame-h ()
  (let ((frame (mahogany-current-frame *compositor-state*)))
    (when frame
      (tree:split-frame-h frame :direction :right))))

(defcommand split-frame-v ()
  (let ((frame (mahogany-current-frame *compositor-state*)))
    (when frame
      (tree:split-frame-v frame :direction :bottom))))

(defcommand maximize-current-frame ()
  (let ((group (mahogany-current-group *compositor-state*)))
    (group-maximize-current-frame group)))

(defcommand close-current-view ()
  (let ((frame (mahogany-current-frame *compositor-state*)))
    (alexandria:when-let ((view (mahogany/tree:frame-view frame)))
      (hrt:view-request-close view))))

(defcommand next-view ()
  "Raise the next hidden view in the current group"
  (let ((group (mahogany-current-group *compositor-state*)))
    (group-next-hidden group)))

(defcommand previous-view ()
  "Raise the next hidden view in the current group"
  (let ((group (mahogany-current-group *compositor-state*)))
    (group-previous-hidden group)))

(defcommand next-frame (:seat seat)
  (let ((group (mahogany-current-group *compositor-state*)))
    (group-next-frame group seat)))

(defcommand prev-frame (:seat seat)
  (let ((group (mahogany-current-group *compositor-state*)))
    (group-prev-frame group seat)))

(defcommand gnew ()
  (mahogany-state-group-add *compositor-state*))

(defcommand gkill ()
  (let ((current-group (mahogany-current-group *compositor-state*)))
    (mahogany-state-group-remove *compositor-state* current-group)))

(defcommand gnext ()
  (state-next-hidden-group *compositor-state*))

(defcommand gprev ()
  (state-next-hidden-group *compositor-state*))

#+:hrt-debug
(defcommand add-output ()
  (if (hrt:hrt-add-output (mahogany-state-server *compositor-state*))
      (log-string :info "Output not added")
      (log-string :info "Output added")))

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
