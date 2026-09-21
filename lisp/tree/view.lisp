(in-package :mahogany/tree)

(defparameter *frame-focus-border-style*
  nil
  "Border of empty frames that are focused")

(defparameter *frame-unfocus-border-style*
  nil
  "Border of frames that are not focused")

(defparameter *frame-unfocus-empty-border-style*
  nil
  "Border of empty frames that are not focused")

(defclass view-frame (frame)
  ((view :initarg :view
         :accessor frame-surface
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
   (seat :initform nil)))

(defmethod initialize-instance :after ((frame view-frame)
                                       &key focused view
                                       &allow-other-keys)
  (with-slots (border-box) frame
    (let ((layer-container (frame-find-layer frame))
          (box-style (if focused *frame-focus-border-style*
                         *frame-unfocus-border-style*)))
      (setf border-box (hrt:border-box-create
                        (layer-container-layer layer-container)
                        box-style
                        (round (frame-x frame)) (round (frame-y frame))
                        (round (frame-width frame))
                        (round (frame-height frame)))))
    (when view
      (hrt:hrt-border-box-set-enabled border-box nil))))

(defmethod (setf frame-surface) :around (new-view (frame view-frame))
  (let ((old-view (frame-surface frame)))
    (call-next-method)
    (when old-view
      (setf (hrt::view-container old-view) nil))
    (when new-view
      (setf (hrt::view-container new-view) frame))))

(defun cleanup-frame (frame)
  (log-string :trace "Cleaning up frame ~S" frame)
  (hrt:hrt-border-box-destroy (slot-value frame 'border-box))
  (setf (slot-value frame 'border-box) nil)
  (alexandria:when-let ((view (frame-surface frame)))
    (setf (hrt::view-container view) nil)))

(defmethod replace-frame ((root view-frame) frame &optional (cleanup-func #'identity))
  (unless (eql root frame)
    (funcall cleanup-func frame)
    (cleanup-frame frame)
    (%replace-frame frame frame)))

(defmethod (setf %frame-prev) (prev (frame view-frame))
  (setf (slot-value frame 'prev) prev))

(defmethod (setf %frame-next) (next (frame view-frame))
  (setf (slot-value frame 'next) next))

(defun frame-view-dimensions (frame)
  (let* ((total-border-width (round (* 2 2)))
         (w-adjust (- (frame-width frame) total-border-width))
         (h-adjust (- (frame-height frame) total-border-width)))
    (values w-adjust h-adjust)))

(defun %view-frame-set-dimensions (view width height)
  (let* ((total-border-width (round (* 2 2)))
         (w-adjust (- width total-border-width))
         (h-adjust (- height total-border-width)))
    (set-dimensions view w-adjust h-adjust)))

(defun %view-frame-set-position (view x y)
  (let* ((adjust 2)
         (x-adjust (+ x adjust))
         (y-adjust (+ y adjust)))
    (set-position view x-adjust y-adjust)))

(defmethod (setf frame-surface) :after (view (frame view-frame))
  "Place the view in the frame and make it have the same dimensions
and position as the frame"
  (with-slots (border-box focused) frame
    (cond
      (view
       (unless focused
         ;; (hrt:hrt-border-box-set-enabled border-box nil))
         (hrt:border-box-set-style (slot-value frame 'border-box)
                                   *frame-unfocus-border-style*))
       (hrt:with-view-transaction ()
         (%view-frame-set-position view (round (frame-x frame)) (round (frame-y frame)))
         (%view-frame-set-dimensions view (round (frame-width frame)) (round (frame-height frame)))
         (when (frame-focused frame)
           (hrt:focus-view view (slot-value frame 'seat)))))
      (t
       (unless focused
         (hrt:border-box-set-style (slot-value frame 'border-box)
                                   *frame-unfocus-empty-border-style*))))))

(defmethod mark-frame-focused :after ((frame view-frame) seat)
  (setf (slot-value frame 'seat) seat)
  (hrt:border-box-set-style (slot-value frame 'border-box) *frame-focus-border-style*)
  (alexandria:when-let ((hrt-view (frame-surface frame)))
    (log-string :trace "view frame focused")
    (hrt:focus-view hrt-view seat)
    (hrt:hrt-border-box-set-enabled (slot-value frame 'border-box) t)))

(defmethod unmark-frame-focused :after ((frame view-frame) seat)
  (alexandria:if-let ((hrt-view (frame-surface frame)))
    (progn
      (log-string :trace "view frame unfocused")
      (hrt:unfocus-view hrt-view seat)
      (hrt:border-box-set-style (slot-value frame 'border-box)
                                *frame-unfocus-border-style*))
    (hrt:border-box-set-style (slot-value frame 'border-box)
                                *frame-unfocus-empty-border-style*))
  (setf (slot-value frame 'seat) nil))

(defmethod print-object ((object view-frame) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (width height x y view)
        object
      (let ((*print-circle* t))
        (format stream ":w ~A :h ~A :x ~A :y ~A view: ~S"
                (round width) (round height) (round x) (round y) view)))))

(defmethod (setf frame-x) :before (new-x (frame view-frame))
  (let ((round-x (round new-x))
        (round-y (round (frame-y frame))))
    (when (frame-surface frame)
      (%view-frame-set-position (frame-surface frame) round-x round-y))
    (hrt:hrt-border-box-set-relative (slot-value frame 'border-box)
                                     round-x round-y)))

(defmethod (setf frame-y) :before (new-y (frame view-frame))
  (let ((round-y (round new-y))
        (round-x (round (frame-x frame))))
    (when (frame-surface frame)
      (%view-frame-set-position (frame-surface frame) round-x round-y))
    (hrt:hrt-border-box-set-relative (slot-value frame 'border-box)
                                     round-x round-y)))

(defmethod set-dimensions :before ((frame view-frame) width height)
  (let ((w-adjusted (round width))
        (h-adjusted (round height)))
    (when (frame-surface frame)
      (%view-frame-set-dimensions (frame-surface frame) w-adjusted h-adjusted))
    (hrt:hrt-border-box-set-size (slot-value frame 'border-box)
                                 w-adjusted h-adjusted)))

(defmethod set-position :before ((frame view-frame) x y)
  (let ((round-x (round x))
        (round-y (round y)))
    (when (frame-surface frame)
      (%view-frame-set-position (frame-surface frame) round-x round-y))
    (hrt:hrt-border-box-set-relative (slot-value frame 'border-box)
                                     round-x round-y)))

(defmethod (setf frame-width) :before (new-width (frame view-frame))
  (let ((round-width (round new-width))
        (round-height (round (frame-height frame))))
    (when (frame-surface frame)
      (%view-frame-set-dimensions (frame-surface frame) round-width round-height))
    (hrt:hrt-border-box-set-size (slot-value frame 'border-box)
                                 round-width round-height)))

(defmethod (setf frame-height) :before (new-height (frame view-frame))
  (let ((round-width (round (frame-width frame)))
        (round-height (round new-height)))
    (when (frame-surface frame)
      (%view-frame-set-dimensions (frame-surface frame) round-width round-height))
    (hrt:hrt-border-box-set-size (slot-value frame 'border-box)
                                 round-width round-height)))

(defmethod find-view-frame ((frame view-frame) view)
  (when (equal (frame-surface frame) view)
    frame))

;; (defmethod find-view-frame ((frame view-frame)
;;        (view sb-sys:system-area-pointer))
;;   (let ((hrt-view (frame-surface frame)))
;;  (when (and hrt-view
;;       (equal (hrt:view-hrt-view (frame-surface frame)) view))
;;    frame)))
