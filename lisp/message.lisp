(in-package #:mahogany)

(config:defconfig *message-gravity* :gravity-center
  hrt:toast-message-gravity
  "Where toast messages are placed on the screen")

(config:defconfig *message-delay* 3000
  fixnum
  "How long toast messages are displayed by default, in milliseconds")

(config:defconfig *message-theme* (mh/theme:make-theme)
  mh/theme:theme
  "Theme data for displaying toast messages")

(defun toast-message (state text &key (gravity *message-gravity*)
                                   (ms-delay *message-delay*)
                                   (theme *message-theme*))
  (declare (type mahogany-state state))
  (with-accessors ((cur-group mahogany-current-group)
                   (hrt-server mahogany-state-server))
      state
    (let* ((cur-output (group-current-output cur-group)))
      (alexandria:when-let ((hrt-output (mahogany-output-hrt-output cur-output)))
        (hrt:toast-message hrt-server hrt-output text
                           gravity
                           theme
                           ms-delay)))))
