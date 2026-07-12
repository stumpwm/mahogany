(in-package #:mahogany)

(declaim (type fixnum *keyboard-focus-bits*
               +click-mask+
               +wheel-mask+
               +sloppy-mask+))
(defconstant +click-mask+ 1)
(defconstant +wheel-mask+ (ash 1 1))
(defconstant +sloppy-mask+ (ash 1 2))

(defglobal *keyboard-focus-bits* +click-mask+)

(config-system:define-setf-config
    (keyboard-focus-type :click
     :type (member :click-and-wheel :click :ignore :sloppy))
    "How keyboard focus is changed based on mouse input.
Values:
  :click
     Change when a button on the mouse is clicked
  :click-and-wheel
     Change when a button is clicked or the scroll whell is used.
  :sloppy (not implemented)
     Change focus when the mouse is moved over surface, is clicked,
     or the wheel is used.
  :ignore
     Do not change focus after the mouse is used."
  (:setter (val)
           (setf *keyboard-focus-bits*
                 (case val
                   (:click-and-wheel
                    (logior +click-mask+ +wheel-mask+))
                   (:sloppy
                    (logior +click-mask+ +wheel-mask+ +sloppy-mask+))
                   (:click
                    +click-mask+)
                   (:ignore 0)))
           val)
  (:getter ()
           (macrolet ((not-set-p (&rest args)
                        `(= *keyboard-focus-bits* (logior ,@args))))
             (cond
               ((not-set-p +click-mask+ +wheel-mask+ +sloppy-mask+)
                :sloppy)
               ((not-set-p +click-mask+ +wheel-mask+)
                :click-and-wheel)
               ((not-set-p +click-mask+)
                :click)
               ((= *keyboard-focus-bits* 0)
                :ignore)))))

(config:defconfig *gimme-key-enable* nil
  boolean
  "Should we show a popup with key hints?")

(config:defconfig *gimme-key-theme*
    (mh/theme:augment-theme
     *message-theme*
     :font "monospace 10")
    ;; (mh/theme:make-theme
    ;;  :font "monospace"
    ;;  :background-color (colors:rgb 0.1 0.1 0.1)
    ;;  :border-color (colors:rgb 0.3 0.3 0.3))
  mh/theme:theme
  "Theme data for displaying gimme-key messages")

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
  (declare (type key key)
           (type key-state key-state)
           (optimize (speed 3)))
  (when (not (key-modifier-key-p key))
    (let* ((handling-keybinding (key-state-active-p key-state)))
      (flet ((reset-state ()
               (log-string :trace "Reseting keyboard state")
               (state-ungrab-seat *compositor-state*)
               (server-keystate-reset *compositor-state*)))
        (multiple-value-bind (matched result)
            (key-state-advance key key-state)
          (cond
            (;; A known keybinding was pressed:
             matched
             (cond
               (result
                (reset-state)
                (when *gimme-key-enable*
                 ;; NOTE kills the toast, cause i don't know how to do this cleanly
                 (toast-message *compositor-state* "" :ms-delay 1))
                ;; Only consume the pressed key if we have a command
                ;; and not a special keyword:
                (if (eq :pass-through result)
                    (return-from check-and-run-keybinding nil)
                    (execute-command result
		                             (key-state-sequence key-state) seat)))
               (t (state-grab-seat *compositor-state*)
                  (when (and *gimme-key-enable* (not result))
                    (toast-message *compositor-state*
                                   (mahogany/keyboard:gimme-key-format key-state)
                                   :ms-delay 0
                                   :theme *gimme-key-theme*))
                  t)))
            (;; No keybinding was pressed but we were expecting one.
             handling-keybinding
             (toast-message *compositor-state* (%unkown-keybinding-message key-state key))
             (reset-state)
             ;; Since we still took an action (canceling the keybinding),
             ;; still consume the pressed key
             t)
            ;; No action was taken, don't consume the pressed key:
            (t  nil)))))))

(defun handle-key-event (state key seat event-state)
  (declare (type key key)
           (type (unsigned-byte 32) event-state)
           (type mahogany-state state)
           (optimize (speed 3)))
  (let ((key-state (state-key-state state)))
    (declare (type key-state key-state))
    (if (= event-state wl:+wl-keyboard-key-state-pressed+)
        (check-and-run-keybinding key seat key-state)
        (key-state-active-p key-state))))

(defun %focus-frame-under-cursor (seat)
  (declare (optimize (speed 2)))
  (declare (type cffi:foreign-pointer seat))
  (let* ((group (state-current-group *compositor-state*))
         (found (tree:frame-at (mahogany-group-tiled-container group)
                               (hrt:hrt-seat-cursor-lx seat)
                               (hrt:hrt-seat-cursor-ly seat))))
    (if found
        (state-focus-frame *compositor-state* found seat)
        nil)))

(hrt:define-hrt-callback handle-mouse-wheel-event :void
    ((seat (:pointer (:struct hrt:hrt-seat)))
     (event :pointer))
    ()
  (declare (optimize (speed 3)))
  (when (not (zerop (logand *keyboard-focus-bits* +wheel-mask+)))
    (%focus-frame-under-cursor seat))
  (hrt:hrt-seat-notify-axis seat event))

(hrt:define-hrt-callback handle-mouse-button-event :void
    ((seat (:pointer (:struct hrt:hrt-seat)))
     (event :pointer))
    ()
  (declare (optimize (speed 3)))
  (when (not (zerop (logand *keyboard-focus-bits* +click-mask+)))
    (%focus-frame-under-cursor seat))
  (hrt:hrt-seat-notify-button seat event))

(hrt:define-hrt-callback keyboard-callback :bool
    ((seat (:pointer (:struct hrt:hrt-seat)))
     (info (:pointer (:struct hrt:hrt-keypress-info))))
    ()
  (declare (optimize (speed 3)))
  (cffi:with-foreign-slots ((hrt:keysyms hrt:modifiers hrt:keysyms-len hrt:wl-key-state)
                            info (:struct hrt:hrt-keypress-info))
    ;; I'm not sure why this is an array, but it's what tinywl does:
    (dotimes (i hrt:keysyms-len)
      (let ((key (make-key (cffi:mem-aref hrt:keysyms :uint32 i) hrt:modifiers)))
        (when (handle-key-event *compositor-state* key seat hrt:wl-key-state)
          (return t))))))
