(defpackage :mahogany/backend/input/keyboard
  (:use :cl :mahogany/backend/util :cffi)
  (:import-from :mahogany/log
		#:log-string)
  (:import-from :wayland-server-core
		#:wl-signal-add
		#:wl-list-remove
		#:wl_listener
		#:link)
  (:import-from :xkb
		#:with-xkb-context
		#:with-keymap-from-names))

(in-package :mahogany/backend/input/keyboard)

(export '(init-default-keyboard-rules
	  keyboard
	  make-keyboard
	  destroy-keyboard
	  set-keyboard-keymap))

(defvar *listener-hash* (make-hash-table))

(defgeneric set-keyboard-keymap (keyboard keymap)
  (:documentation "Set the keymap for the keyboard"))

(defclass keyboard ()
  ((wlr-keyboard :initarg :wlr-keyboard
		 :reader keyboard-wlr-keyboard
		 :type wlr:keyboard)
   (key-listener :initarg :key-listener
		 :reader keyboard-key-listener
		 :type wl_listener)))

(defun init-default-keyboard-rules (rules)
  (setf (cffi:foreign-slot-value rules '(:struct xkb:rule-names)
				 :rules)
	(or (uiop:getenv "XKB_DEFAULT_RULES") ""))
  (setf (cffi:foreign-slot-value rules '(:struct xkb:rule-names)
				 :model)
	(or (uiop:getenv "XKB_DEFAULT_MODEL") ""))
  (setf (cffi:foreign-slot-value rules '(:struct xkb:rule-names)
				 :layout)
	(or (uiop:getenv "XKB_DEFAULT_LAYOUT") ""))
  (setf (cffi:foreign-slot-value rules '(:struct xkb:rule-names)
				 :variant)
	(or (uiop:getenv "XKB_DEFAULT_VARIANT") ""))
  (setf (cffi:foreign-slot-value rules '(:struct xkb:rule-names)
				 :options)
	(or (uiop:getenv "XKB_DEFAULT_OPTIONS") "")))

(cffi:defcallback keyboard-key-notify :void
    ((listener :pointer)
     (event (:pointer (:struct wlr:event-keyboard-key))))
  (let* ((owner-keyboard (get-listener-owner listener *listener-hash*))
	 (wlr-keyboard (cffi:foreign-slot-value (keyboard-wlr-keyboard owner-keyboard)
						'(:struct wlr:input-device)
    						:keyboard))
	 (keycode (+ 8 (foreign-slot-value event '(:struct wlr:event-keyboard-key) :keycode))))
    (with-foreign-object (syms :pointer)
      (let ((num-syms (xkb:state-key-get-syms (foreign-slot-value wlr-keyboard
								    '(:struct wlr:keyboard)
								    :xkb-state)
					      keycode syms)))
	(log-string :trace "Num keysyms: ~A" num-syms)
	(dotimes (i num-syms)
	  ;; ref twice, as syms is a pointer to a pointer:
	  (let ((keysym (mem-aref (mem-ref syms :pointer) 'xkb:keysym i)))
	    (log-string :trace "Keysym: ~A" keysym)
	    ;; (when (eql keysym #xff1b)
	    ;;   (wl-display-terminate (sample-state-display *sample-state*)))
	    ))
	  (finish-output)))))

(defun make-keyboard (device rules)
  (let* ((key-listener (make-listener keyboard-key-notify))
	 (new-keyboard (make-instance 'keyboard :wlr-keyboard device
						:key-listener key-listener))
	 (wlr-keyboard (cffi:foreign-slot-value device '(:struct wlr:input-device)
    								    :keyboard)))
    (wl-signal-add (foreign-slot-pointer wlr-keyboard
					 '(:struct wlr:keyboard) :event-key)
    		   key-listener)

    (with-xkb-context (context (:no-flags))
	(with-keymap-from-names (keymap (context rules :no-flags))
	  (wlr:keyboard-set-keymap wlr-keyboard keymap)))
    (register-listener key-listener new-keyboard *listener-hash*)))

(defun destroy-keyboard (keyboard)
  (with-accessors ((listener keyboard-key-listener)) keyboard
    (unregister-listener listener *listener-hash*)
    (wl-list-remove (foreign-slot-pointer listener
					  '(:struct wl_listener) 'link))
    (foreign-free listener)))
