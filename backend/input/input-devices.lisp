(in-package :mahogany/backend)

(defclass input-device ()
  ((wlr-input-device :initarg :wlr-input-device
		     :reader input-device-wlr-input
		     :type wlr:input-device)
   (seat-owner :initarg :seat
	       :accessor input-device-seat
	       :type seat)))

(defclass keyboard (input-device)
  ((key-listener :initarg :key-listener
		 :reader keyboard-key-listener
		 :type wl_listener)
   (modifier-listener :initarg :modifier-listener
		      :reader keyboard-modifier-listener
		      :type wl_listener)
   (modifier-state :accessor keyboard-modifier-state
		   :type modifier-state)
   (executing-keybinding :reader executing-keybinding-p
			 :writer keyboard-executing-keybinding
			 :type boolean
			 :initfrom nil)))

(defclass pointing-device (input-device)
  ()
  (:documentation "A object to hold a wlr pointer"))

(defclass cursor ()
  ((wlr-cursor :initarg :wlr-cursor
	      :reader cursor-wlr-cursor
	      :type wlr:cursor)
   (xcursor-manager :initarg :xcursor-manager
		    :reader cursor-xcursor-manager
		    :type wlr:xcursor-manager)
   (motion-listener :initarg :motion-listener
		    :accessor cursor-motion-listener
		    :type wl_listener)
   (motion-absolute-listener :initarg :motion-absolute-listener
			     :accessor cursor-motion-absolute-listener
			     :type wl_listener)
   (axis-listener :initarg :axis-listener
		  :accessor cursor-axis-listener
		  :type wl_listener)
   (button-listener :initarg :button-listener
		    :accessor cursor-button-listener
		    :type wl_listener))
  (:documentation "Represents a wlr-cursor to which multiple
pointing devices can be attached to."))

(defclass seat ()
  ((wlr-seat :initarg :wlr-seat
	     :reader seat-wlr-seat
	     :type wlr:seat)
   (cursor :initarg :cursor
	   :reader seat-cursor
	   :type cursor)))

(defgeneric destroy-device (device)
  (:documentation "Remove the device from its associated systems and
destroy its wlr device"))

(defgeneric get-device-name (input-device)
  (:documentation "Get the name of the input device")
  (:method ((device input-device))
    (foreign-slot-value (input-device-wlr-input device)
			'(:struct wlr:input-device)
			:name)))

(defgeneric get-device-vendor (input-device)
  (:documentation "Get the device vendor id")
  (:method ((device input-device))
    (foreign-slot-value (input-device-wlr-input device)
			'(:struct wlr:input-device)
			:vendor)))

(defgeneric get-device-product (input-device)
  (:documentation "Get the device product id")
  (:method ((device input-device))
    (foreign-slot-value (input-device-wlr-input device)
			'(:struct wlr:input-device)
			:product)))
