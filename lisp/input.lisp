(in-package #:mahogany)

(config-system:defconfig *keyboard-focus-type* :click
  (member :click-and-wheel :click :ignore :sloppy)
  "How keyboard focus is controlled by the mouse")

(config:defconfig *gimme-key-enable* t
  boolean
  "Should we show a popup with key hints?")

(config:defconfig *gimme-key-theme*
    (mh/theme:make-theme
     :font "monospace 17"
     :background-color (colors:rgb 0.1 0.1 0.1)
     :border-color (colors:rgb 0.3 0.3 0.3))
  mh/theme:theme
  "Theme data for displaying gimme-key messages")

(defun execute-command (function key-sequence seat)
  (funcall function key-sequence seat))

(defun %unkown-keybinding-message (key-state last)
  (declare (optimize (speed 3) (safety 0))
           (type key-state key-state)
           (type key last))
  (with-output-to-string (s)
    (format s "Keybinding \"")
    (dolist (k (reverse (key-state-sequence key-state)))
      (pprint-key k s)
      (format s " "))
    (pprint-key last s)
    (format s "\"~%is undefined.")))

(defun check-and-run-keybinding (key seat key-state)
  (declare (type key key) (optimize (speed 3)))
  (when (not (key-modifier-key-p key))
    (let* ((handling-keybinding (key-state-active-p key-state)))
      (log-string :trace "Already handling keybinding: ~A" handling-keybinding)
      (flet ((reset-state ()
               (log-string :trace "Reseting keyboard state")
               (server-keystate-reset *compositor-state*)))
        (multiple-value-bind (matched result)
            (key-state-advance key key-state)
          (cond
            (;; A known keybinding was pressed:
             matched
             (when result
               (reset-state)
               (when *gimme-key-enable*
                 ;; NOTE kills the toast, cause i don't know how to do this cleanly
                 (toast-message *compositor-state* "" :ms-delay 1))
               ;; Only consume the pressed key if the command doesn't
               ;; return `:pass-through`
               (not (eql (execute-command result
                                          (key-state-sequence key-state) seat)
                         :pass-through)))
             (when (and *gimme-key-enable* (not result))
               (toast-message *compositor-state*
                              (mahogany/keyboard:gimme-key-format key-state)
                              :gravity :gravity-bottom
                              :ms-delay 0
                              :theme *gimme-key-theme*))
             ;; Consume the pressed key
             t)
            (;; No keybinding was pressed but we were expecting one.
             handling-keybinding
             (toast-message *compositor-state* (%unkown-keybinding-message key-state key))
             (reset-state)
             ;; Since we still took an action (canceling the keybinding),
             ;; still consume the pressed key
             t)
            ;; No action was taken, don't comsume the pressed key:
            (t  nil)))))))

(defun handle-key-event (state key seat event-state)
  (declare (type key key)
           (type bit event-state)
           (optimize(speed 3)))
  (let ((key-state (mahogany-state-key-state state)))
    (declare (type key-state key-state))
    (if (= event-state 1)
        (check-and-run-keybinding key seat key-state)
        (key-state-active-p key-state))))

(defun %focus-frame-under-cursor (seat)
  (let* ((group (mahogany-current-group *compositor-state*))
         (found (tree:frame-at (mahogany-group-tree-container group)
                               (hrt:hrt-seat-cursor-lx seat)
                               (hrt:hrt-seat-cursor-ly seat))))
    (group-focus-frame group found seat)))

(cffi:defcallback handle-mouse-wheel-event :void
    ((seat (:pointer (:struct hrt:hrt-seat)))
     (event :pointer))
  (when (eq *keyboard-focus-type* :click-and-wheel)
    (%focus-frame-under-cursor seat))
  (hrt:hrt-seat-notify-axis seat event))

(cffi:defcallback handle-mouse-button-event :void
    ((seat (:pointer (:struct hrt:hrt-seat)))
     (event :pointer))
  (when (or (eq *keyboard-focus-type* :click)
            (eq *keyboard-focus-type* :click-and-wheel))
    (%focus-frame-under-cursor seat))
  (hrt:hrt-seat-notify-button seat event))

(cffi:defcallback keyboard-callback :bool
    ((seat (:pointer (:struct hrt:hrt-seat)))
     (info (:pointer (:struct hrt:hrt-keypress-info))))
  (cffi:with-foreign-slots ((hrt:keysyms hrt:modifiers hrt:keysyms-len hrt:wl-key-state)
                            info (:struct hrt:hrt-keypress-info))
    ;; I'm not sure why this is an array, but it's what tinywl does:
    (dotimes (i hrt:keysyms-len)
      (let ((key (make-key (cffi:mem-aref hrt:keysyms :uint32 i) hrt:modifiers)))
        (when (handle-key-event *compositor-state* key seat hrt:wl-key-state)
          (return t))))))
