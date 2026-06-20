(in-package #:hrt)

(defstruct (output (:constructor %make-output
					  (hrt-output full-name)))
  (hrt-output (cffi:null-pointer) :type cffi:foreign-pointer :read-only t)
  (full-name "" :type string :read-only t))

(defstruct output-config
  (scale 1 :type (or number null) :read-only t)
  (refresh-rate nil :type (or number null) :read-only t)
  (custom-mode nil :type boolean :read-only t)
  ;; a cons of (width . height)
  (dimensions nil :type (or cons null) :read-only t)
  ;; a cons of (x . y)
  (position nil :type (or cons null) :read-only t))

(defun %transfer-output-config (hrt-config config)
  (cffi:with-foreign-slots
      ((scale custom-mode width height refresh-rate custom-position x y)
       hrt-config (:struct hrt-output-config))
    (setf scale (if (output-config-scale config)
                    (coerce (output-config-scale config) 'double-float)
                    0.0d0))
    (alexandria:when-let ((dimensions (output-config-dimensions config)))
      (setf custom-mode (output-config-custom-mode config)
            refresh-rate (alexandria:if-let ((rate (output-config-refresh-rate config)))
                           (coerce rate 'single-float)
                           0.0)
            width (car dimensions)
            height (cdr dimensions)))
    (alexandria:when-let ((position (output-config-position config)))
      (setf custom-position t
            x (car position)
            y (cdr position)))))

(defmacro with-output-config ((var config) &body body)
  `(cffi:with-foreign-object (,var '(:struct hrt-output-config))
     (%transfer-output-config ,var ,config)
     ,@body))

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

(defun output-init (output config)
  (if config
      (with-output-config (hrt-config config)
        (mahogany/log:log-string
         :info "Appyling configuration to output ~S: ~S"
         (output-full-name output) config)
        (hrt-output-init (output-hrt-output output) hrt-config))
      (hrt-output-init (output-hrt-output output) (cffi:null-pointer))))

(defun destroy-output (mh-output)
  (declare (ignore mh-output)))

#-hrt-debug
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

#-hrt-debug
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
