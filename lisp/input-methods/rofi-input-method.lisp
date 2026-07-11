(in-package #:mahogany)

(defclass rofi-input-method (input-method) ())

(defmethod prepare-completions-for-input-method
    ((im rofi-input-method)
     (completions cl-interactive:database))
  (cl-interactive:database-strings completions))

(defmethod input-method-read
    ((im rofi-input-method) (prompt string)
     &key completions require-match initial-input
       history
       &allow-other-keys)
  (declare (ignore initial-input history))
  (let ((pset nil))
    (tagbody
     start
       (let ((res (run-simple-rofi prompt completions nil)))
         (cond ((find res completions :test #'string=)
                (return-from input-method-read res))
               (require-match
                (psetf pset t
                       prompt (if pset
                                  prompt
                                  (concatenate 'string "[Invalid entry] "
                                               prompt)))
                (go start))
               (t (return-from input-method-read res)))))))

(defun run-rofi (arguments input)
  "Run rofi syncronously."
  (multiple-value-bind (output error status)
      (uiop:run-program (cons "rofi" arguments)
                        :output '(:string :stripped t)
                        :input (make-string-input-stream
                                (typecase input
                                  (string input)
                                  ((or (cons string cons)
                                       (cons string null))
                                   (format nil "~{~A~^~%~}" input))
                                  (null "")
                                  (otherwise
                                   (error "invalid input to rofi"))))
                        :ignore-error-status t
                        :force-shell nil)
    (when (or (not (= status 0)) (not (string= "" error)))
      (log-string
       :trace
       "Rofi Input method completed with error output (Status ~A): ~A"
       status
       error))
    (case status
      (0
       (values output error status))
      (1
       (values nil error status))
      (t
       (error "rofi exited badly with status ~D" s)))))


(defun run-simple-rofi (prompt input &optional lines)
  (run-rofi (list* "-dmenu" "-i" "-p" prompt (when lines (list "-l" "10")))
            input))
