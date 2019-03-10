(in-package :mahogany/backend)

(defvar *listener-hash* (make-hash-table))

(defgeneric set-keyboard-keymap (keyboard keymap)
  (:documentation "Set the keymap for the keyboard"))

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
	 (wlr-keyboard (cffi:foreign-slot-value (input-device-wlr-input owner-keyboard)
						'(:struct wlr:input-device)
    						:keyboard))
	 (keycode (+ 8 (foreign-slot-value event '(:struct wlr:event-keyboard-key) :keycode))))
    (wlr:seat-set-keyboard (seat-wlr-seat (input-device-seat owner-keyboard))
    			   (input-device-wlr-input owner-keyboard))
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
	     (when (eql keysym #xff1b)
	       (stop-backend (get-server)))))
	(finish-output)))))

(defcallback keyboard-modifier-notify :void
    ((listener :pointer)
     (event (:pointer (:struct wlr:event-keyboard-key))))
  (log-string :trace "Modifier pressed"))

(defmethod (setf input-device-seat) :before (new-seat (keyboard keyboard))
  ;; if the keyboard is the current keyboard for its seat, remove it:
  (when (equal (input-device-wlr-input keyboard)
	       (wlr:seat-get-keyboard (seat-wlr-seat (input-device-seat keyboard))))
    (log-string :debug "Changing the seat of a keyboard")
    (wlr:seat-set-keyboard (seat-wlr-seat (input-device-seat keyboard)) (null-pointer))))

(defun make-keyboard (device seat rules)
  (let* ((key-listener (make-listener keyboard-key-notify))
	 (modifier-listener (make-listener keyboard-modifier-notify))
	 (new-keyboard (make-instance 'keyboard :wlr-input-device device
				      :key-listener key-listener
				      :modifier-listener modifier-listener
				      :seat seat))
	 (wlr-keyboard (cffi:foreign-slot-value device '(:struct wlr:input-device)
    						:keyboard)))
    (with-wlr-accessors ((key-signal :event-key :pointer t)
			 (modifier-signal :event-modifiers :pointer t))
	wlr-keyboard (:struct wlr:keyboard)
      (wl-signal-add key-signal key-listener)
      (wl-signal-add modifier-signal modifier-listener))
    (with-xkb-context (context (:no-flags))
	(with-keymap-from-names (keymap (context rules :no-flags))
	  (wlr:keyboard-set-keymap wlr-keyboard keymap)))
    (register-listeners new-keyboard  *listener-hash*
			key-listener modifier-listener)
    new-keyboard))

(defun destroy-keyboard (keyboard)
  (with-accessors ((key-listener keyboard-key-listener)
		   (modifier-listener keyboard-modifier-listener))
      keyboard
    (cleanup-listener key-listener *listener-hash*)
    (cleanup-listener modifier-listener *listener-hash*)))

(defmethod destroy-device ((keyboard keyboard))
  (destroy-keyboard keyboard))
