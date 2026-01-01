(in-package #:mahogany)

(config-system:defconfig *keyboard-focus-type* :click
  (member :click-and-wheel :click :ignore :sloppy)
  "How keyboard focus is controlled by the mouse")

(defun execute-command (function key-sequence seat)
  (funcall function key-sequence seat))

(defun check-and-run-keybinding (key seat key-state)
  (declare (type key key) (optimize (speed 3)))
  (when (not (key-modifier-key-p key))
    (let* ((handling-keybinding (key-state-active-p key-state)))
      (log-string :trace "Already handling keybinding: ~A" handling-keybinding)
      (flet ((reset-state ()
               (log-string :trace "Reseting keyboard state")
               (server-keystate-reset *compositor-state*)))
        (prog1
            (multiple-value-bind (matched result)
                (key-state-advance key key-state)
              (cond
                (;; A known keybinding was pressed:
                 matched
                 (when result
                   (execute-command result (key-state-sequence key-state) seat)
                   (reset-state))
                 t)
                (;; No keybinding was pressed but we were expecting one.
                 ;; Since this is canceling the keybinding, we still behave like we found somthing
                 handling-keybinding
                 (reset-state)
                 t)
                ;; No action was taken, return nil
                (t  nil))))))))

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
