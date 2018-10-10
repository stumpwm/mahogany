(in-package :mahogany/backend)

(defmethod destroy-device ((device pointing-device))
  (wlr:cursor-detach-input-device (seat-wlr-cursor (input-device-seat device)) (input-device-wlr-input device)))

(defun make-pointing-device (device seat)
  "Create a new pointer object with device and assign it to seat."
  (let ((new-mh-pointer (make-instance 'pointing-device
			 :wlr-input-device device
			 :seat seat)))
    (wlr:cursor-attach-input-device (cursor-wlr-cursor (seat-cursor seat)) device)
    new-mh-pointer))
