(in-package :mahogany/tree)

(defclass view-frame (frame)
  ((view :initarg :view
         :accessor frame-view
         :initform nil
         :type (or hrt:view null)
         :documentation "The client of the frame")
   (next :initarg next-frame
         :initform nil
         :type (or view-frame null)
         :reader frame-next)
   (prev :initarg prev-frame
         :initform nil
         :type (or view-frame null)
         :reader frame-prev)
   (seat :initform nil)))

(defmethod (setf %frame-prev) (prev (frame view-frame))
  (setf (slot-value frame 'prev) prev))

(defmethod (setf %frame-next) (next (frame view-frame))
  (setf (slot-value frame 'next) next))

(defmethod (setf frame-view) :after (view (frame view-frame))
  "Place the view in the frame and make it have the same dimensions
and position as the frame"
  (when view
    (set-position view (round (frame-x frame)) (round (frame-y frame)))
    (set-dimensions view (round (frame-width frame)) (round (frame-height frame)))
    (when (frame-focused frame)
      (hrt:focus-view view (slot-value frame 'seat)))))

(defmethod mark-frame-focused :after ((frame view-frame) seat)
  (setf (slot-value frame 'seat) seat)
  (alexandria:when-let ((hrt-view (frame-view frame)))
    (log-string :trace "view frame focused")
    (hrt:focus-view hrt-view seat)))

(defmethod unmark-frame-focused :after ((frame view-frame) seat)
  (alexandria:when-let ((hrt-view (frame-view frame)))
    (log-string :trace "view frame unfocused")
    (hrt:unfocus-view hrt-view seat))
  (setf (slot-value frame 'seat) nil))

(defmethod print-object ((object view-frame) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (width height x y view)
        object
      (format stream ":w ~A :h ~A :x ~A :y ~A view: ~S"
              (round width) (round height) (round x) (round y) view))))

(defmethod (setf frame-x) :before (new-x (frame view-frame))
  (when (frame-view frame)
    (set-position (frame-view frame) (round new-x) (round (frame-y frame)))))

(defmethod (setf frame-y) :before (new-y (frame view-frame))
  (when (frame-view frame)
    (set-position (frame-view frame) (round (frame-x frame)) (round new-y))))

(defmethod set-dimensions :before ((frame view-frame) width height)
  (when (frame-view frame)
    (set-dimensions (frame-view frame) (round width) (round height))))

(defmethod set-position :before ((frame view-frame) x y)
  (when (frame-view frame)
    (set-position (frame-view frame) (round x) (round y))))

(defmethod (setf frame-width) :before (new-width (frame view-frame))
  (when (frame-view frame)
    (set-dimensions (frame-view frame) (round new-width) (round (frame-height frame)))))

(defmethod (setf frame-height) :before (new-height (frame view-frame))
  (when (frame-view frame)
    (set-dimensions (frame-view frame) (round (frame-width frame)) (round new-height))))

(defmethod find-view-frame ((frame view-frame) view)
  (when (equal (frame-view frame) view)
    frame))

;; (defmethod find-view-frame ((frame view-frame)
;;        (view sb-sys:system-area-pointer))
;;   (let ((hrt-view (frame-view frame)))
;;  (when (and hrt-view
;;       (equal (hrt:view-hrt-view (frame-view frame)) view))
;;    frame)))
