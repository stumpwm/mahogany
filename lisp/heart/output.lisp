(in-package #:hrt)

(defstruct (output (:constructor %make-output
					  (hrt-output full-name)))
  (hrt-output cffi:null-pointer :type cffi:foreign-pointer :read-only t)
  (full-name "" :type string :read-only t))

(defun %get-output-full-name (hrt-output)
  (let ((make (hrt-output-make hrt-output))
	(name (hrt-output-name hrt-output))
	(serial (hrt-output-serial hrt-output))
	(model (hrt-output-model hrt-output)))
    (concatenate 'string
		 (or name "")
		 (or make "")
		 (or model "")
		 (or serial ""))))

(defun make-output (hrt-output)
  (declare (type cffi:foreign-pointer hrt-output))
  (let ((name (%get-output-full-name hrt-output)))
    (%make-output hrt-output name)))

(defun destroy-output (mh-output)
  (declare (ignore mh-output)))

(declaim (inline output-resolution output-position))
(defun output-resolution (output)
  (declare (type output output))
  (with-return-by-value ((width :int) (height :int))
    (hrt-output-resolution (output-hrt-output output) width height)))

(defun output-position (output)
  (declare (type output output))
  (with-return-by-value ((x :int) (y :int))
    (hrt-output-position (output-hrt-output output) x y)))

(defun output-usable-area (output)
  (declare (type output output))
  (let* ((hrt-output (output-hrt-output output))
         (box (cffi:foreign-slot-pointer hrt-output '(:struct hrt-output)
                                      'usable-area)))
    (cffi:with-foreign-slots ((x y width height) box (:struct wlr-box))
      (values x y width height))))

(declaim (inline output-name output-make output-model output-serial))
(defun output-name (output)
  (declare (type output output))
  (hrt-output-name (output-hrt-output output)))

(defun output-make (output)
  (declare (type output output))
  (hrt-output-make (output-hrt-output output)))

(defun output-model (output)
  (declare (type output output))
  (hrt-output-serial (output-hrt-output output)))

(defun output-serial (output)
  (declare (type output output))
  (hrt-output-serial (output-hrt-output output)))
