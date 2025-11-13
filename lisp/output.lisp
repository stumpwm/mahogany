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

(defun make-mahogany-output (hrt-output hrt-scene)
  (let ((name (%get-output-full-name hrt-output))
	(scene (hrt:hrt-scene-output-create hrt-scene)))
    (%make-mahogany-output hrt-output scene name)))

(defun destroy-mahogany-output (mh-output)
  (hrt:hrt-scene-output-destroy (mahogany-output-hrt-scene mh-output)))
