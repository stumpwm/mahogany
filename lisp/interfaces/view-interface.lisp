(defpackage #:mahogany/wm-interface
  (:use :cl)
  (:export #:set-position
           #:set-dimensions))

(in-package #:mahogany/wm-interface)

(defgeneric set-position (object x y)
  (:documentation "Set the x-y position of the object. If the object
is part of a scene tree, this sets the position relative to the
parent object."))

(defgeneric set-dimensions (object width height)
  (:documentation "Set the dimensions of the the object."))
