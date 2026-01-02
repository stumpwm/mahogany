(fiasco:define-test-package #:mahogany-tests/log
  (:use #:mahogany/log))

(in-package #:mahogany-tests/log)

(fiasco:deftest log-string-format-args-have-no-side-effects ()
  (setf (log-level) :ignore)
  (let ((thing 0))
    (flet ((fn () (setf thing (+ 1 thing))))
      (log-string :debug "Counter is: ~A" (fn))
      (fiasco:is (eql thing 0)))))

(defun non-constant-log (lvl stream)
  (let ((*log-output-file* stream))
    (log-string lvl "Test")))

(fiasco:deftest log-string-non-constant-lvl-has-no-side-effects ()
  (setf (log-level) :ignore)
  (let ((thing 0))
    (declare (notinline non-constant-log))
    (flet ((fn () (setf thing (+ 1 thing))))
      (with-output-to-string (stream)
        (non-constant-log :debug stream)
        (fiasco:is (eql thing 0))))))

(fiasco:deftest log-string-ignore-is-ignored ()
  (setf (log-level) :trace)
  (let ((output-result (with-output-to-string (output)
                         (let ((*log-output-file* output))
                           (log-string :ignore "Test")))))
    (is (string-equal output-result ""))))

(fiasco:deftest log-levels-translate ()
  (dolist (level (list :trace :debug :info :warn :error :fatal :ignore))
    (setf (log-level) level)
    (is (eql (log-level) level))))
