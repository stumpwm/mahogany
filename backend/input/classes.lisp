(in-package :mahogany/backend)

(defgeneric destroy-device (device)
  (:documentation "Remove the device from its associated systems and
destroy its wlr device"))

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
