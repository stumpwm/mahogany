(in-package #:hrt)

(defstruct (view (:constructor %make-view (hrt-view)))
  (hrt-view (cffi:null-pointer) :type cffi:foreign-pointer :read-only t))

#-hrt-debug
(declaim (inline init-view))
(defun view-init (view-ptr)
  (declare (type cffi:foreign-pointer view-ptr))
  (%make-view view-ptr))

#-hrt-debug
(declaim (inline focus-view))
(defun focus-view (view seat)
  (declare (type view view))
  (hrt-view-focus (view-hrt-view view) seat))

#-hrt-debug
(declaim (inline unfocus-view))
(defun unfocus-view (view seat)
  (declare (type view view))
  (hrt-view-unfocus (view-hrt-view view) seat))

#-hrt-debug
(declaim (inline view-focused-p))
(defun view-focused-p (view)
  (declare (type view view))
  (hrt-view-focused (view-hrt-view view)))

#-hrt-debug
(declaim (inline view-mapped-p))
(defun view-mapped-p (view)
  (declare (type view view))
  (hrt-view-mapped (view-hrt-view view)))

#-hrt-debug
(declaim (inline view-set-hidden))
(defun view-set-hidden (view hidden)
  (declare (type view view)
           (type boolean hidden))
  (hrt-view-set-hidden (view-hrt-view view) hidden)
  (dirty-view-transaction))

#-hrt-debug
(declaim (inline view-reparent))
(defun view-reparent (view new-parent)
  (declare (type view view)
           (type cffi:foreign-pointer new-parent))
  (hrt-view-reparent (view-hrt-view view) new-parent))

#-hrt-debug
(declaim (inline view-request-close))
(defun view-request-close (view)
  (declare (type view view))
  (hrt-view-request-close (view-hrt-view view)))

#-hrt-debug
(declaim (inline view-set-fullscreen))
(defun view-set-fullscreen (view fullscreen)
  (declare (type view view))
  (hrt-view-fullscreen (view-hrt-view view) fullscreen))

#-hrt-debug
(declaim (inline view-fullscreen-p))
(defun view-fullscreen-p (view)
  (declare (type view view))
  (hrt-view-fullscreened (view-hrt-view view)))

#-hrt-debug
(declaim (inline view-configure))
(defun view-configure (view)
  (declare (type view view))
  (mahogany/log:log-string :trace "Sending configure to view ~A" view)
  (hrt-view-send-configure (view-hrt-view view)))

(defmethod mh/interface:set-dimensions ((view view) width height)
  "Request that the given view change its size."
  ;; We don't need to dirty the view transaction here,
  ;; as this is *requesting* that the view change size,
  ;; not actually changing the size.
  (hrt-view-set-size (view-hrt-view view) (round width) (round height)))

(defmethod mh/interface:set-position ((view view) x y)
  (hrt-view-set-relative (view-hrt-view view) (round x) (round y))
  (dirty-view-transaction))
