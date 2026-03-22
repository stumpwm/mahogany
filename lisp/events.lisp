(in-package #:mahogany)

(hrt:define-hrt-callback handle-new-view-event :void
    ((view (:pointer (:struct hrt:hrt-view))))
    ()
  (log-string :trace "New view callback called!")
  (mahogany-state-view-add *compositor-state* view))

(hrt:define-hrt-callback handle-view-size-changed :void
    ((view (:pointer (:struct hrt:hrt-view))))
    ()
  (log-string :trace "View size changed")
  (mahogany-state-view-size-changed *compositor-state* view))

(hrt:define-hrt-callback handle-view-mapped :void
    ((view (:pointer (:struct hrt:hrt-view))))
    ()
  (log-string :trace "View Mapped!")
  (mahogany-state-view-map *compositor-state* view))

(hrt:define-hrt-callback handle-view-unmapped :void
    ((view (:pointer (:struct hrt:hrt-view))))
    ()
  (log-string :trace "View unmapped!")
  (mahogany-state-view-unmap *compositor-state* view))

(hrt:define-hrt-callback handle-view-destroyed-event :void
    ((view (:pointer (:struct hrt:hrt-view))))
    ()
  (log-string :trace "View destroyed callback called!")
  (mahogany-state-view-remove *compositor-state*  view))

(hrt:define-hrt-callback handle-view-minimize :void
    ((view (:pointer (:struct hrt:hrt-view))))
    ()
  (log-string :trace "View minimize callback called!")
  (mahogany-state-view-minimize *compositor-state* view))

(hrt:define-hrt-callback handle-view-maximize :void
    ((view (:pointer (:struct hrt:hrt-view))))
    ()
  (log-string :trace "View maximize callback called!")
  (mahogany-state-view-maximize *compositor-state* view))

(hrt:define-hrt-callback handle-request-fullscreen :bool
    ((view (:pointer (:struct hrt:hrt-view)))
     (output (:pointer (:struct hrt:hrt-output)))
     (fullscreen :bool))
    ()
  (log-string :trace "fullscreen requested: view ~S on output ~S" view output)
  (mahogany-state-view-fullscreen *compositor-state* view output fullscreen))

(hrt:define-hrt-callback handle-new-output :void
    ((output (:pointer (:struct hrt:hrt-output))))
    ()
  (mahogany-state-output-add *compositor-state* output))

(hrt:define-hrt-callback handle-output-removed :void
    ((output (:pointer (:struct hrt:hrt-output))))
    ()
  (mahogany-state-output-remove *compositor-state* output))

(hrt:define-hrt-callback handle-output-layout-change :void
    ()
    ()
  (mahogany-state-output-reconfigure *compositor-state*))
