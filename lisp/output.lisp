(in-package #:mahogany)

(cffi:defcallback handle-new-output :void ((output (:pointer (:struct hrt-output))))
  (log-string :trace "New output added")
  (vector-push-extend (make-mahogany-output output) (mahogany-state-outputs *compositor-state*)))

(cffi:defcallback handle-output-removed :void ((output (:pointer (:struct hrt-output))))
  (log-string :trace "Output removed")
  (with-accessors ((outputs mahogany-state-outputs)) *compositor-state*
    (setf outputs (delete output outputs :key #'mahogany-output-hrt-output))))

(cffi:defcallback output-mode-change-callback :void ((output (:pointer (:struct hrt-output))))
  (multiple-value-bind (width height) (hrt:output-resolution output)
    (log-string :trace "Output mode changed (w: ~S h: ~S)" width height)))
