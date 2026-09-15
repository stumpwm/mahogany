(in-package #:mahogany)

(defclass rofi-input-method (input-method) ())

(defmethod prepare-completions-for-input-method
    ((im rofi-input-method)
     (completions cl-interactive:database))
  (cl-interactive:database-strings completions))

(defmethod prepare-completions-for-input-method
    ((im rofi-input-method)
     (completions list))
  (if (every #'stringp completions)
      completions
      (error 'cl-interactive:unknown-completions-error
	         :input-method im
	         :completions completions)))

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

(defmethod cl-interactive/input-method::input-method-read-index
    ((im rofi-input-method) sequence prompt &key select-multiple)
  (let ((idx-str (run-rofi (list* "-dmenu" "-i" "-p" prompt
                                  (when select-multiple "-multi-select")
                                  (list "-format" "i"))
                           sequence)))
    (let ((idx 0))
      (loop :while (< idx (length idx-str))
            :collect (multiple-value-bind (num new-idx)
                         (parse-integer idx-str :junk-allowed t :start idx)
                       (setf idx new-idx)
                       num)))))

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
       (error 'cl-interactive:cancel-interactive-command
              :reason "Rofi canceled"))
      (t
       (error "rofi exited badly with status ~D" status)))))


(defun run-simple-rofi (prompt input &optional lines)
  (run-rofi (list* "-dmenu" "-i" "-p" prompt (when lines (list "-l" "10")))
            input))
