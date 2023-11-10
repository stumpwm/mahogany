(in-package #:mahogany)

(defstruct (mahogany-output (:constructor make-mahogany-output (hrt-output)))
  (hrt-output cffi:null-pointer :type cffi:foreign-pointer :read-only t))

(cffi:defcallback handle-new-output :void ((output (:pointer (:struct hrt-output))))
  (log-string :trace "New output added")
  (vector-push-extend (make-mahogany-output output) (mahogany-state-outputs *compositor-state*)))

(cffi:defcallback handle-output-removed :void ((output (:pointer (:struct hrt-output))))
  (log-string :trace "Output removed")
  (with-accessors ((outputs mahogany-state-outputs)) *compositor-state*
    (setf outputs (delete output outputs :key #'mahogany-output-hrt-output))))
