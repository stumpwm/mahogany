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

(cffi:defcallback handle-view-minimize :void
	((view (:pointer (:struct hrt:hrt-view))))
  (log-string :trace "View minimize callback called!")
  (mahogany-state-view-minimize *compositor-state* view))

(cffi:defcallback handle-view-maximize :void
	((view (:pointer (:struct hrt:hrt-view))))
  (log-string :trace "View maximize callback called!")
  (mahogany-state-view-maximize *compositor-state* view))
