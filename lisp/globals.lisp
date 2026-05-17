(in-package #:mahogany)

(declaim (type mahogany-state *compositor-state*))
(defglobal *compositor-state* (make-mahogany-state))
