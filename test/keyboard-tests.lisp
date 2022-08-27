(fiasco:define-test-package #:mahogany-tests/keyboard
  (:use #:mahogany/keyboard))

(in-package #:mahogany-tests/keyboard)

(defun expand-key-description (code &rest desc)
  (let ((mask 0))
    (declare (type (unsigned-byte 32)))
    (dolist (mod desc)
      (setf mask (logior mod mask)))
    (make-key code mask)))

(defmacro expect-key (kbd &key to-be)
  `(is (equalp (parse-key ,kbd) (expand-key-description ,@to-be))))

(fiasco:deftest test-parse-key ()

  (expect-key "C-l" :to-be (108 mahogany/keyboard::+modifier-ctrl+))
  (expect-key "C-L" :to-be (76 mahogany/keyboard::+modifier-ctrl+))
  (expect-key "C-s-l" :to-be (108 mahogany/keyboard::+modifier-ctrl+
				  mahogany/keyboard::+modifier-super+))
  (expect-key "C-S-F1" :to-be (65470 mahogany/keyboard::+modifier-ctrl+
				     mahogany/keyboard::+modifier-shift+))
  (expect-key "C--" :to-be (45 mahogany/keyboard::+modifier-ctrl+))
  (expect-key "M-RET" :to-be (65293 mahogany/keyboard::+modifier-alt+))
  (expect-key "-" :to-be (45))

  (signals kbd-parse-error (parse-key "C-"))
  (signals kdb-parse-error (parske-key "B-")))
