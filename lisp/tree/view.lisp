(in-package :mahogany/tree)

(defclass view-frame (frame)
  ((view :initarg :view
	 :accessor frame-view
	 :initform nil
	 :type (or hrt:view null)
	 :documentation "The client of the frame")))

(defun fit-view-into-frame (view frame)
  "Make the view fit in the dimensions of the frame"
  (set-position view (round (frame-x frame)) (round (frame-y frame)))
  (set-dimensions view (round (frame-width frame)) (round (frame-height frame))))

(defun put-view-in-frame (view view-frame)
  "Place the view in the frame and make it have the same dimensions
and position as the frame"
  (setf (frame-view view-frame) view)
  (fit-view-into-frame view view-frame))

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
