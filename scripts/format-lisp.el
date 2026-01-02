;; Utilities for formatting list code via emacs.

;; Eval each form in order, with the current dir set to the project root.
;; running them all with eval-buffer doesn't work for some reason.

(defun mahogany-format-files (wildcard)
  (let ((f (find-file-noselect wildcard t nil t)))
    (dolist (b f)
      (with-current-buffer b
	(mark-whole-buffer)
	(indent-region (point-min) (point-max))
	(save-buffer)))))

(require 'slime)

(slime)
(sleep-for-seconds 5)
(message "foo")
(slime-repl-eval-string
 "(progn (require 'asdf) (load \"init-build-env.lisp\")
(asdf:load-system \"mahogany\")
(asdf:load-system \"mahogany-test\"))")
(mahogany-format-files "lisp/**/*.lisp")
(mahogany-format-files "*.asd")

;; (directory-files-recursively "lisp" "*.lisp")
