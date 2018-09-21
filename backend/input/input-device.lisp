(defpackage :mahogany/backend/input/input-device
  (:use :cl :cffi))

(in-package :mahogany/backend/input/input-device)

(export '(input-device
	  input-device-wlr-input
	  get-device-name
	  get-device-number
	  get-device-vendor
	  get-device-product))

(defclass input-device ()
  ((wlr-input-device :initarg :wlr-input-device
		     :reader input-device-wlr-input
		     :type wlr:input-device)))

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
  (:documentation "Get the device produce id")
  (:method ((device input-device))
    (foreign-slot-value (input-device-wlr-input device)
			'(:struct wlr:input-device)
			:product)))
