(in-package #:mahogany/wm-interface)

(defgeneric get-visible-views (wm)
  (:documentation "Get the views to display."))

(defgeneric add-view (wm view)
  (:documentation "Add the view to the wm"))

(defgeneric remove-view (wm view)
  (:documentation "Remove the view from the wm"))
