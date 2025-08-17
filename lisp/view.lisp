(in-package #:mahogany)

(cffi:defcallback handle-new-view-event :void
    ((view (:pointer (:struct hrt:hrt-view))))
  (log-string :trace "New view callback called!")
  (mahogany-state-view-add *compositor-state* view))

(cffi:defcallback handle-view-mapped :void
    ((view (:pointer (:struct hrt:hrt-view))))
  (log-string :trace "View Mapped!")
  (mahogany-state-view-map *compositor-state* view))

(cffi:defcallback handle-view-unmapped :void
    ((view (:pointer (:struct hrt:hrt-view))))
  (log-string :trace "View unmapped!")
  (mahogany-state-view-unmap *compositor-state* view))

(cffi:defcallback handle-view-destroyed-event :void
    ((view (:pointer (:struct hrt:hrt-view))))
  (log-string :trace "View destroyed callback called!")
  (mahogany-state-view-remove *compositor-state*  view))

(cffi:defcallback handle-request-fullscreen :bool
    ((view (:pointer (:struct hrt:hrt-view)))
     (output (:pointer (:struct hrt:hrt-output)))
     (fullscreen :bool))
  (log-string :trace "fullscreen requested: view ~S on output ~S" view output)
  (mahogany-state-view-fullscreen *compositor-state* view output fullscreen))
