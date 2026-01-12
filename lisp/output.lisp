(in-package #:mahogany)

(defun %get-output-full-name (hrt-output)
  (let ((make (hrt:hrt-output-make hrt-output))
        (name (hrt:hrt-output-name hrt-output))
        (serial (hrt:hrt-output-serial hrt-output))
        (model (hrt:hrt-output-model hrt-output)))
    (concatenate 'string
                 (or name "")
                 (or make "")
                 (or model "")
                 (or serial ""))))

(defun make-mahogany-output (hrt-output)
  (let ((name (%get-output-full-name hrt-output)))
    (%make-mahogany-output hrt-output name)))

(defun destroy-mahogany-output (mh-output)
  (declare (ignore mh-output)))
