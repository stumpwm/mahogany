(in-package #:hrt)

(defstruct (view (:constructor %make-view (hrt-view)))
  (hrt-view (cffi:null-pointer) :type cffi:foreign-pointer :read-only t))

(declaim (inline focus-view))
(defun focus-view (view seat)
  (declare (type view view))
  (hrt-view-focus (view-hrt-view view) seat))

(declaim (inline unfocus-view))
(defun unfocus-view (view seat)
  (declare (type view view))
  (hrt-view-unfocus (view-hrt-view view) seat))

(declaim (inline view-mapped-p))
(defun view-mapped-p (view)
  (declare (type view view))
  (hrt-view-mapped (view-hrt-view view)))

(declaim (inline view-set-hidden))
(defun view-set-hidden (view hidden)
  (declare (type view view)
	   (type boolean hidden))
  (hrt-view-set-hidden (view-hrt-view view) hidden))

(declaim (inline view-reparent))
(defun view-reparent (view new-parent)
  (declare (type view view))
  (hrt-view-reparent (view-hrt-view view) new-parent))

(declaim (inline view-request-close))
(defun view-request-close (view)
  (declare (type view view))
  (hrt-view-request-close (view-hrt-view view)))

(declaim (inline view-set-fullscreen))
(defun view-set-fullscreen (view fullscreen)
  (declare (type view view))
  (hrt-view-set-fullscreen (view-hrt-view view) fullscreen))

(declaim (inline view-configure))
(defun view-configure (view)
  (declare (type view view))
  (hrt-view-send-configure (view-hrt-view view)))

(defmethod mh/interface:set-dimensions ((view view) width height)
  (hrt-view-set-size (view-hrt-view view) width height))

(defmethod mh/interface:set-position ((view view) x y)
  (hrt-view-set-relative (view-hrt-view view) x y))
