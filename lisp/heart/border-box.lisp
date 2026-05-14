(in-package #:hrt)

(defstruct (border-box-style (:constructor make-border-box-style (ptr)))
  (ptr nil :type cffi:foreign-pointer))

(defun border-box-create (layer style x y width height)
  (hrt-border-box-create layer (border-box-style-ptr style)
                         x y width height))

(defun border-box-style-create (border-style color line-width)
  (declare (type colors:rgb color))
  (cffi:with-foreign-object (c-color :float 4)
    (write-color-array c-color color)
    (alexandria:if-let ((hrt-style (hrt-border-box-style-create
                                    border-style c-color line-width)))
      (let ((style (make-border-box-style hrt-style)))
        ;; Style objects are reference counted so we don't need to worry about
        ;; keeping track of them:
        (tg:finalize style (lambda ()
                             (hrt-border-box-style-unref hrt-style)))
        style)
      (error 'mahogany/util:mahogany-panic
             "Could not make border-box-style"))))

(defun border-box-style-update (style border-style color line-width)
  (declare (type border-box-style style)
           (type colors:rgb color))
  (let ((ptr (border-box-style-ptr style)))
    (cffi:with-foreign-object (c-color :float 4)
      (write-color-array c-color color)
      (hrt-border-box-style-update ptr border-style c-color line-width))))
