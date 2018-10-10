(in-package :mahogany/backend)

(defclass input-manager ()
  ((server :initarg :server
	   :reader mh-server)
   ;; alist of seats: small enough to not need a hashtable
   (seats :initarg :seats
	  :accessor input-seats
	  :type list)
   (keyboards :accessor keyboards
	      :initform ()
	      :type list)
   (input-listener :initarg :input-listener
		   :reader input-listener))
  (:default-initargs
   :seats ())
  (:documentation "Class used to manage seats and their devices"))

(alexandria:define-constant +default-seat-name+ "MAHOGANY-SEAT" :test #'string=)

(defcallback handle-seat-destroy :void
    ((listener :pointer)
     (seat (:pointer (:struct wlr:seat))))
  (declare (ignore seat listener))
  (log-string :info "Seat Destroyed"))

(defun add-new-seat (input-manager seat-name)
  "Create a new seat named seat-name and add it to the given input manager.
Returns the newly created seat."
  (let* ((new-seat (make-seat (get-display (mh-server input-manager))
			      seat-name))
	(destroy-listener (make-listener handle-seat-destroy))
	(wlr-seat (seat-wlr-seat new-seat)))
    (wl-signal-add (foreign-slot-pointer wlr-seat '(:struct wlr:seat)
					 :event-destroy)
		   destroy-listener)
    (register-listener destroy-listener (list input-manager new-seat) *listener-hash*)
    (setf (input-seats input-manager) (acons seat-name new-seat (input-seats input-manager)))
    new-seat))

(defun get-desired-seat (input-manager device)
  ;; device will be used in the future to help decide which seat to pick.
  (declare (ignore device))
  ;; TODO: add functions, rules, ect to decide which seat the device
  ;; goes to.
  (if-let ((seat (first (assoc +default-seat-name+ (input-seats input-manager) :test #'string=))))
    seat
    (add-new-seat input-manager +default-seat-name+)))

(cffi:defcallback keyboard-destroy-notify :void
    ((listener :pointer)
     (keyboard (:pointer (:struct wlr:input-device))))
  (declare (ignore keyboard))
  (format t "A keyboard was destroyed~%")
  (multiple-value-bind (manager keyboard)
      (values-list (get-listener-owner listener *listener-hash*))
    (destroy-device keyboard)
    (setf (keyboards manager) (delete keyboard (keyboards manager)))
    (unregister-listener listener *listener-hash*)
    (wl-list-remove (foreign-slot-pointer listener
					  '(:struct wl_listener) 'link))
    (foreign-free listener)))

(defun add-keyboard (manager device seat)
  (let ((destroy-listener (make-listener keyboard-destroy-notify))
	(new-keyboard (with-foreign-object (rules '(:struct xkb:rule-names))
			(init-default-keyboard-rules rules)
			(make-keyboard device seat rules))))
    (wl-signal-add (cffi:foreign-slot-pointer device '(:struct wlr:input-device)
					      :event-destroy)
		   destroy-listener)
    (register-listener destroy-listener (list manager new-keyboard) *listener-hash*)
    new-keyboard))

(cffi:defcallback handle-new-input :void
    ((listener :pointer)
     (input (:pointer (:struct wlr:input-device))))
  (let* ((listener-owner (get-listener-owner listener *listener-hash*))
	(seat (get-desired-seat listener-owner input)))
    (log-string :info "new input device ~A"
		(cffi:foreign-slot-value input
					 '(:struct wlr:input-device) :type))

    (case (cffi:foreign-slot-value input
  				   '(:struct wlr:input-device) :type)
      (:keyboard (push (add-keyboard listener-owner input seat) (keyboards listener-owner)))
      (t (log-string :info "Something that isn't a keyboard was added")))))

(defun make-input-manager (server)
  (let ((new-input-listener (make-listener handle-new-input)))
    (wayland-server-core:wl-signal-add (cffi:foreign-slot-pointer (get-backend server)
								  '(:struct wlr:backend)
								  :event-new-input)
				       new-input-listener)
    (let ((new-manager (make-instance 'input-manager
				      :input-listener new-input-listener
				      :server server)))
      (register-listener new-input-listener new-manager *listener-hash*)
      (the input-manager new-manager))))

(defun destroy-input-manager (input-manager)
  (with-accessors ((input-listener input-listener)) input-manager
    (unregister-listener input-listener *listener-hash*)
    (wl-list-remove (foreign-slot-pointer input-listener
					  '(:struct wl_listener) 'link))
    (foreign-free input-listener)))
