(fiasco:define-test-package #:mahogany-tests/theme
  (:use #:mahogany/theme)
  (:local-nicknames (#:colors #:cl-colors2)))

(in-package #:mahogany-tests/theme)

(fiasco:deftest setf-theme-font-keeps-format ()
  (let ((theme (make-theme :font "monospace 15")))
    (setf (theme-font-name theme) "CMU Typewriter Text")
    (is (string= (theme-font theme) "CMU Typewriter Text 15"))))

(fiasco:deftest setf-theme-font-size-keeps-format ()
  (let ((theme (make-theme :font "CMU Typewriter Text 15")))
    (setf (theme-font-size theme) 20)
    (is (string= (theme-font theme) "CMU Typewriter Text 20"))))

(fiasco:deftest theme-font-name-returns-name ()
  (let ((theme (make-theme :font "CMU Typewriter Text 15")))
    (is (string= (theme-font-name theme) "CMU Typewriter Text"))))

(fiasco:deftest theme-font-size-returns-size ()
  (let ((theme (make-theme :font "CMU Typewriter Text 15")))
    (is (= (theme-font-size theme) 15))))
