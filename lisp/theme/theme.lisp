
(defpackage #:mahogany/theme
  (:use :cl)
  (:nicknames #:mh/theme)
  (:local-nicknames (#:colors #:cl-colors2))
  (:export
   #:theme
   #:make-theme
   #:theme-font
   #:theme-font-name
   #:theme-font-size
   #:theme-font-color
   #:theme-background-color
   #:theme-font-border
   #:theme-border-color))

(in-package #:mahogany/theme)

;; INVESTIGATE: we could store these colors as single-float arrays,
;;  and just use the colors package to derive the numbers.
;;  this would have the advantage of removing some type conversion
;;  when transforming this object into a C struct, and allow us
;;  to specify the alpha channel, which the cl-colors2 package doesn't do.
;;  the cl-colors-ng package *does* support alpha channels, so using
;;  that instead might be a good start. It isn't in quicklisp
;;  though...
(defstruct theme
  (font "monospace 15" :type string)
  (font-color (colors:rgb 1.0 1.0 1.0) :type colors:rgb)
  (background-color (colors:rgb 0.0 0.0 0.0) :type colors:rgb)
  (border-color (colors:rgb 1.0 1.0 1.0) :type colors:rgb))

(defun theme-font-name (theme)
  (declare (type theme theme))
  (let ((end (position #\Space (theme-font theme) :from-end t)))
    (subseq (theme-font theme) 0 end)))

(declaim (inline %theme-font-size))
(defun %theme-font-size (theme)
  (declare (type theme theme))
  (let ((end (position #\Space (theme-font theme) :from-end t)))
    (subseq (theme-font theme) (+ 1 end))))

(defun theme-font-size (theme)
  (values (parse-integer (%theme-font-size theme))))

(defun (setf theme-font-name) (name theme)
  (declare (type theme theme))
  (let ((size (%theme-font-size theme)))
    (setf (theme-font theme) (concatenate 'string name " " size))))

(defun (setf theme-font-size) (size theme)
  (declare (type theme theme))
  (let* ((name (theme-font-name theme))
         (full (concatenate 'string
	     name " " (write-to-string size))))
    (setf (theme-font theme) full)))
