(in-package #:mahogany)

(defmacro silence-notes (&body body)
  `(locally
       (declare
        #+sbcl
        (sb-ext:muffle-conditions sb-ext:compiler-note))
     ,@body))

(defmacro %with-found-view (state (view-var view-ptr) &body body)
  ;; There might be a way to prevent SBCL from needing to box
  ;; the 64 bit pointers in order to hash them, but it's not obvious.
  ;; Silence the note to make more important notes more visible:
  `(alexandria:if-let ((,view-var (silence-notes
                                   (gethash (cffi:pointer-address ,view-ptr)
                                            (slot-value ,state 'views)))))
     (progn
       ,@body)
     (silence-notes
      (log-string :error "Could not find mahogany view associated with pointer ~S"
                  (cffi:pointer-address ,view-ptr)))))

(hrt:define-hrt-callback handle-new-view-event :void
    ((view (:pointer (:struct hrt:hrt-view))))
    ()
  (log-string :trace "New view callback called!")
  (silence-notes
   (mahogany-state-view-add *compositor-state* view)))

(hrt:define-hrt-callback handle-view-size-changed :void
    ((view-ptr (:pointer (:struct hrt:hrt-view))))
    ()
  (log-string :trace "View size changed")
  (%with-found-view *compositor-state* (view view-ptr)
    (mahogany-state-view-size-changed *compositor-state* view)))

(hrt:define-hrt-callback handle-view-mapped :void
    ((view-ptr (:pointer (:struct hrt:hrt-view))))
    ()
  (log-string :trace "View Mapped!")
  (%with-found-view *compositor-state* (view view-ptr)
    (mahogany-state-view-map *compositor-state* view)))

(hrt:define-hrt-callback handle-view-unmapped :void
    ((view-ptr (:pointer (:struct hrt:hrt-view))))
    ()
  (log-string :trace "View unmapped!")
  (%with-found-view *compositor-state* (view view-ptr)
    (mahogany-state-view-unmap *compositor-state* view)))

(hrt:define-hrt-callback handle-view-destroyed-event :void
    ((view (:pointer (:struct hrt:hrt-view))))
    ()
  (log-string :trace "View destroyed callback called!")
  (silence-notes
   (mahogany-state-view-remove *compositor-state*  view)))

(hrt:define-hrt-callback handle-view-minimize :void
    ((view-ptr (:pointer (:struct hrt:hrt-view))))
    ()
  (log-string :trace "View minimize callback called!")
  (%with-found-view *compositor-state* (view view-ptr)
    (mahogany-state-view-minimize *compositor-state* view)))

(hrt:define-hrt-callback handle-view-maximize :void
    ((view-ptr (:pointer (:struct hrt:hrt-view))))
    ()
  (log-string :trace "View maximize callback called!")
  (%with-found-view *compositor-state* (view view-ptr)
    (mahogany-state-view-maximize *compositor-state* view)))

(hrt:define-hrt-callback handle-request-fullscreen :bool
    ((view-ptr (:pointer (:struct hrt:hrt-view)))
     (output-ptr (:pointer (:struct hrt:hrt-output)))
     (fullscreen :bool))
    (:error-val nil)
  (let ((output (%find-output output-ptr *compositor-state*)))
    (%with-found-view *compositor-state* (view view-ptr)
      (mahogany-state-view-fullscreen *compositor-state* view output fullscreen))))

(hrt:define-hrt-callback handle-new-output :void
    ((output-ptr (:pointer (:struct hrt:hrt-output))))
    ()
  (silence-notes
   (mahogany-state-output-add *compositor-state* output-ptr)))

(hrt:define-hrt-callback handle-output-removed :void
    ((output-ptr (:pointer (:struct hrt:hrt-output))))
    ()
  (silence-notes
   (mahogany-state-output-remove *compositor-state* output-ptr)))

(hrt:define-hrt-callback handle-output-layout-change :void
    ()
    ()
  (mahogany-state-output-reconfigure *compositor-state*))

(hrt:define-hrt-callback handle-layer-shell-recieved :void
    ((surface :pointer))
    ()
  (log-string :trace "Layer shell recieved")
  (silence-notes
   (mahogany-state-layer-shell-handle *compositor-state* surface)))

(hrt:define-hrt-callback handle-layer-shell-mapped :void
    ((surface :pointer))
    ()
  (log-string :trace "Layer shell mapped")
  (silence-notes
   (state-layer-surface-add *compositor-state* surface)))

(hrt:define-hrt-callback handle-layer-shell-unmapped :void
    ((surface :pointer))
    ()
  (log-string :trace "Layer shell unmapped")
  (silence-notes
   (state-layer-surface-remove *compositor-state* surface)))

(hrt:define-hrt-callback handle-layer-shell-arrange :void
    ((output-ptr (:pointer (:struct hrt:hrt-output))))
    ()
  (let ((output (%find-output output-ptr *compositor-state*)))
    (mahogany-state-layers-arrange *compositor-state* output)))

(hrt:define-hrt-callback handle-layer-shell-keyboard-interactivity :void
    ((surface :pointer))
    ()
  (silence-notes
    (log-string :trace "Keyboard interactivity of surface ~S changed"
                surface)))

(hrt:define-hrt-callback handle-layer-shell-layer-changed :void
    ((surface :pointer))
    ()
  (silence-notes
    (log-string :trace "Surface ~S moved to layer ~S"
                surface surface)))
