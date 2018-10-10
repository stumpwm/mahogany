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
		 :type wl_listener)))

(defclass pointing-device (input-device)
  ())

(defclass seat ()
  ((wlr-seat :initarg :wlr-seat
	     :reader seat-wlr-seat
	     :type wlr:seat)
   (wlr-cursor :initarg :wlr-cursor
	       :reader seat-wlr-cursor
	       :type wlr:cursor)
   (keyboards :initarg :keyboards
	      :accessor seat-keyboards
	      :type list)
   (cursors :initarg :cursors
	      :accessor seat-cursors
	      :type list))
  (:default-initargs
   :keyboards ()
    :cursors ()))

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
