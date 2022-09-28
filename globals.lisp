(in-package #:mahogany)

(declaim (type *compositor-state* mahogany-state))
(defglobal *compositor-state* (make-instance 'mahogany-state))
