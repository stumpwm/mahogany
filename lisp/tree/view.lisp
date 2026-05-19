(in-package :mahogany/tree)

(defparameter *frame-focus-border-style*
  nil
  "Border of empty frames that are focused")

(defparameter *frame-unfocus-border-style*
  nil
  "Border of empty frames that are not focused")

(defclass view-frame (frame)
  ((view :initarg :view
         :accessor frame-view
         :initform nil
         :type (or hrt:view null)
         :documentation "The client of the frame")
   (next :initarg next-frame
         :initform nil
         :type (or tree-node null)
         :reader frame-next)
   (prev :initarg prev-frame
         :initform nil
         :type (or tree-node null)
         :reader frame-prev)
   (border-box :initform nil
               :type (or null cffi:foreign-pointer))
   (focus-style :initarg focus-style
                :type cffi:foreign-pointer)
   (unfocus-style :initarg unfocus-style
                  :type cffi:foreign-pointer)
   (seat :initform nil)))

(defmethod initialize-instance :after ((frame view-frame)
                                       &key focused view
                                       &allow-other-keys)
  (with-slots (border-box) frame
    (let ((layer-container (frame-find-layer frame))
          (box-style (if focused *frame-focus-border-style*
                         *frame-unfocus-border-style*)))
      (setf border-box (hrt::border-box-create
                        (layer-container-layer layer-container)
                        box-style
                        (round (frame-x frame)) (round (frame-y frame))
                        (round (frame-width frame))
                        (round (frame-height frame)))))
    (when view
      (hrt:hrt-border-box-set-enabled border-box nil))))

(defun cleanup-frame (frame)
  (log-string :trace "Cleaning up frame ~S" frame)
  (hrt:hrt-border-box-destroy (slot-value frame 'border-box))
  (setf (slot-value frame 'border-box) nil))

(defmethod replace-frame ((root view-frame) frame &optional (cleanup-func #'identity))
  (unless (eql root frame)
    (funcall cleanup-func frame)
    (cleanup-frame f)
    (%replace-frame frame frame)))

(defmethod (setf %frame-prev) (prev (frame view-frame))
  (setf (slot-value frame 'prev) prev))

(defmethod (setf %frame-next) (next (frame view-frame))
  (setf (slot-value frame 'next) next))

(defmethod (setf frame-view) :after (view (frame view-frame))
  "Place the view in the frame and make it have the same dimensions
and position as the frame"
  (with-slots (border-box) frame
    (cond
      (view
       (hrt:hrt-border-box-set-enabled border-box nil)
       (hrt:with-view-transaction ()
         (set-position view (round (frame-x frame)) (round (frame-y frame)))
         (set-dimensions view (round (frame-width frame)) (round (frame-height frame)))
         (when (frame-focused frame)
           (hrt:focus-view view (slot-value frame 'seat)))))
      (t
       (hrt:hrt-border-box-set-enabled border-box t)))))

(defmethod mark-frame-focused :after ((frame view-frame) seat)
  (setf (slot-value frame 'seat) seat)
  (hrt:border-box-set-style (slot-value frame 'border-box) *frame-focus-border-style*)
  (alexandria:when-let ((hrt-view (frame-view frame)))
    (log-string :trace "view frame focused")
    (hrt:focus-view hrt-view seat)))

(defmethod unmark-frame-focused :after ((frame view-frame) seat)
  (hrt:border-box-set-style (slot-value frame 'border-box) *frame-unfocus-border-style*)
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
  (let ((round-x (round new-x))
        (round-y (round (frame-y frame))))
    (when (frame-view frame)
      (set-position (frame-view frame) round-x round-y))
    (hrt:hrt-border-box-set-relative (slot-value frame 'border-box)
                                     round-x round-y)))

(defmethod (setf frame-y) :before (new-y (frame view-frame))
  (let ((round-y (round new-y))
        (round-x (round (frame-x frame))))
    (when (frame-view frame)
      (set-position (frame-view frame) round-x round-y))
    (hrt:hrt-border-box-set-relative (slot-value frame 'border-box)
                                     round-x round-y)))

(defmethod set-dimensions :before ((frame view-frame) width height)
  (let ((w-adjusted (round width))
        (h-adjusted (round height)))
    (when (frame-view frame)
      (set-dimensions (frame-view frame) w-adjusted h-adjusted))
    (hrt:hrt-border-box-set-size (slot-value frame 'border-box)
                                 w-adjusted h-adjusted)))

(defmethod set-position :before ((frame view-frame) x y)
  (let ((round-x (round x))
        (round-y (round y)))
    (when (frame-view frame)
      (set-position (frame-view frame) round-x round-y))
    (hrt:hrt-border-box-set-relative (slot-value frame 'border-box)
                                     round-x round-y)))

(defmethod (setf frame-width) :before (new-width (frame view-frame))
  (let ((round-width (round new-width))
        (round-height (round (frame-height frame))))
    (when (frame-view frame)
      (set-dimensions (frame-view frame) round-width round-height))
    (hrt:hrt-border-box-set-size (slot-value frame 'border-box)
                                 round-width round-height)))

(defmethod (setf frame-height) :before (new-height (frame view-frame))
  (let ((round-width (round (frame-width frame)))
        (round-height (round new-height)))
    (when (frame-view frame)
      (set-dimensions (frame-view frame) round-width round-height))
    (hrt:hrt-border-box-set-size (slot-value frame 'border-box)
                                 round-width round-height)))

(defmethod find-view-frame ((frame view-frame) view)
  (when (equal (frame-view frame) view)
    frame))

;; (defmethod find-view-frame ((frame view-frame)
;;        (view sb-sys:system-area-pointer))
;;   (let ((hrt-view (frame-view frame)))
;;  (when (and hrt-view
;;       (equal (hrt:view-hrt-view (frame-view frame)) view))
;;    frame)))
