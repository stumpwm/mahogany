(in-package #:mahogany/keyboard)

(defun sort-keymap-bindings (key-bindings)
  "Sort the `key-BINDINGS' vector of a keymap based on their keys."
  ;; NOTE this might be too costly on top-level keymaps, and it doesn't help
  ;; that it will be called _every_ time you advance the state...
  ;;
  ;; ... doing the naive thing for now.
  (let ((bindings (make-array (hash-table-count key-bindings)
                              :element-type 'binding
                              :initial-contents
                              (loop :for k :being :the :hash-keys
                                      :in key-bindings
                                        :using (hash-value cmd)
                                    :collect (make-binding k cmd)))))
    (sort bindings
          #'(lambda (s1 s2)
              (string-lessp (string-upcase s1) (string-upcase s2)))
          :key #'(lambda (k)
                   (with-output-to-string (s)
                     (pprint-key (binding-key k) s))))))

(defun gimme-key-pprint (map &optional (stream *standard-output*))
  "just like `pprint-kmap`, but even prettier."
  (declare (type kmap map) (type stream stream)
           (optimize (safety 1)))
  (pprint-logical-block (stream nil)
    (pprint-indent :current 2 stream)
    (princ "⤵" stream)
    (loop :for binding :across (sort-keymap-bindings (kmap-bindings map))
          :for key := (binding-key binding)
          :for command := (binding-command binding)
          :do (pprint-newline :mandatory stream)
              (pprint-key key stream)
              (write-string " : " stream)
              (if (kmap-p command)
                  (gimme-key-pprint command stream)
                  (let ((cmd (if (functionp command)
                                 ;;  TODO: Use trivial cltl2 to make
                                 ;; function-lambda-expression portable
                                 (nth-value 2 (function-lambda-expression command))
                                 command)))
                    (write cmd
                     :stream stream))))))

(defun gimme-key-format (key-state)
  (with-output-to-string (s)
    (when-let ((themaplist (key-state-kmaps key-state)))
      (dolist (kmap themaplist)
        (gimme-key-pprint kmap s)
        (format s "~%")))))
