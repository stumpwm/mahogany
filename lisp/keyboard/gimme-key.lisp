(in-package #:mahogany/keyboard)

(defun sort-keymap-bindings (key-bindings)
  "Sort the `key-BINDINGS' vector of a keymap based on their keys."
  ;; NOTE this might be too costly on top-level keymaps, and it doesn't help
  ;; that it will be called _every_ time you advance the state...
  ;;
  ;; ... doing the naive thing for now.
  (sort key-bindings
        #'(lambda (s1 s2)
            (string-lessp (string-upcase s1) (string-upcase s2)))
        :key #'(lambda (k)
                 (with-output-to-string (s)
                   (pprint-key (binding-key k) s)))))

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
                  (write
                   (nth-value 2 (function-lambda-expression command)) ;NOTE not portable...
                   :stream stream)))))

(defun gimme-key-format (key-state)
  (with-output-to-string (s)
    (when-let ((themaplist (key-state-kmaps key-state)))
      (dolist (kmap themaplist)
        (gimme-key-pprint kmap s)
        (format s "~%")))))
