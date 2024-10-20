(in-package #:mahogany)

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

(defun construct-mahogany-output (hrt-output)
  (let ((name (%get-output-full-name hrt-output)))
    (%make-mahogany-output hrt-output name)))

(cffi:defcallback handle-new-output :void ((output (:pointer (:struct hrt-output))))
  (let ((mh-output (construct-mahogany-output output)))
    (mahogany-state-output-add *compositor-state* mh-output)))

(cffi:defcallback handle-output-removed :void ((output (:pointer (:struct hrt-output))))
  (mahogany-state-output-remove *compositor-state* output))

(cffi:defcallback handle-output-layout-change :void ()
  (mahogany-state-output-reconfigure *compositor-state*))
