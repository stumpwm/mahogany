#!/usr/bin/env -S sbcl --load "${HOME}/quicklisp/setup.lisp" --script

(require :asdf)

(defun get-cur-filename ()
  (or *load-truename* *compile-file-truename*))

(defun ql-install-dependencies (sys-name)
  (let* ((sys (asdf:find-system sys-name))
         (required (asdf:system-depends-on sys)))
    (format t "~%Install the following dependencies for system ~S?~%~S~%> "
            sys-name required)
    (let ((answer (read-line)))
      (unless (or (string= answer "yes")
                  (string= answer "y"))
        (format t "Not installing dependencies~%")
        (uiop:quit 1)))
    (ql:quickload required)))

(let* ((p (get-cur-filename))
       (dir (reverse (cdr (reverse (pathname-directory p)))))
       (path (make-pathname :directory dir :name
			    "init-build-env" :type "lisp")))
  (format *error-output* "Root project directory at ~S~%" dir)
  (format *error-output* "Setting up environment...~%")
  (load path))

(ql-install-dependencies "mahogany")
(ql-install-dependencies "mahogany-test")
