(in-package #:mahogany)

(defun handle-server-stop (sequence seat)
  (declare (ignore sequence seat))
  (server-stop *compositor-state*))

(defun open-terminal (sequence seat)
  (declare (ignore sequence seat))
  (sys:open-terminal))

(defun open-kcalc (sequence seat)
  (declare (ignore sequence seat))
  (uiop:launch-program (sys:find-program "kcalc")))

(defun split-frame-h (sequence seat)
  (declare (ignore sequence seat))
  (let ((frame (mahogany-current-frame *compositor-state*)))
    (when frame
      (tree:split-frame-h frame :direction :right))))

(defun split-frame-v (sequence seat)
  (declare (ignore sequence seat))
  (let ((frame (mahogany-current-frame *compositor-state*)))
    (when frame
      (tree:split-frame-v frame :direction :bottom))))

(defun maximize-current-frame (sequence seat)
  (declare (ignore sequence seat))
  (let ((group (mahogany-current-group *compositor-state*)))
    (group-maximize-current-frame group)))

(defun close-current-view (sequence seat)
  (declare (ignore sequence seat))
  (let ((frame (mahogany-current-frame *compositor-state*)))
	(alexandria:when-let ((view (mahogany/tree:frame-view frame)))
	  (hrt:view-request-close view))))

(defun next-view (sequence seat)
  "Raise the next hidden view in the current group"
  (declare (ignore sequence seat))
  (let ((group (mahogany-current-group *compositor-state*)))
    (group-next-hidden group)))

(defun previous-view (sequence seat)
  "Raise the next hidden view in the current group"
  (declare (ignore sequence seat))
  (let ((group (mahogany-current-group *compositor-state*)))
    (group-previous-hidden group)))

(defun next-frame (sequence seat)
  (declare (ignore sequence))
  (let ((group (mahogany-current-group *compositor-state*)))
    (group-next-frame group seat)))

(defun prev-frame (sequence seat)
  (declare (ignore sequence))
  (let ((group (mahogany-current-group *compositor-state*)))
    (group-prev-frame group seat)))

(defun gnew (sequence seat)
  (declare (ignore sequence seat))
  (mahogany-state-group-add *compositor-state*))

(defun gkill (sequence seat)
  (declare (ignore sequence seat))
  (let ((current-group (mahogany-current-group *compositor-state*)))
    (mahogany-state-group-remove *compositor-state* current-group)))

(defun gnext (sequence seat)
  (declare (ignore sequence seat))
  (state-next-hidden-group *compositor-state*))

(defun gprev (sequence seat)
  (declare (ignore sequence seat))
  (state-next-hidden-group *compositor-state*))

#+:hrt-debug
(defun add-output (sequence seat)
  (declare (ignore sequence seat))
  (if (hrt:hrt-add-output (mahogany-state-server *compositor-state*))
      (log-string :info "Output not added")
      (log-string :info "Output added")))

#+:hrt-debug
(defvar *debug-map* (define-kmap
		      (kbd "a") #'add-output))

(defvar *group-map* (define-kmap
		      (kbd "c") #'gnew
		      (kbd "k") #'gkill
		      (kbd "n") #'gnext
		      (kbd "p") #'gprev))

(defvar *root-map* (define-kmap
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
		     (kbd "+") #'open-kcalc
		     (kbd "g") *group-map*))
#+:hrt-debug
(progn
  (define-key *root-map* (kbd "d") *debug-map*))

(setf (mahogany-state-keybindings *compositor-state*)
      (list (define-kmap
              (kbd "C-t") *root-map*)))
