(in-package :mahogany/tree)

;; Since these hold forgein objects, we can't dump them to the image;
;; they need to be set before any frame is created.
;; Once present, they can be updated as needed using the
;; hrt:border-box-style-update function.
(defparameter *frame-focus-border-style*
  nil
  "Border of empty frames that are focused")

(defparameter *frame-unfocus-border-style*
  nil
  "Border of frames that are not focused")

(defparameter *frame-unfocus-empty-border-style*
  nil
  "Border of empty frames that are not focused")

;; Let's keep the border width a fixnum
;; to avoid half pixels:
(declaim (type fixnum *frame-border-width*))
(defglobal *frame-border-width* 1)

(defun init-frame-border-styles ()
  ;; For border collapsing to work, all borders need to be the same
  ;; width:
  (let ((width (coerce *frame-border-width* 'double-float)))
    (setf *frame-focus-border-style*
          ;; Keep these the same width to avoid weird
          ;; frame resizing issues
          (hrt:border-box-style-create
           :hrt-border-solid (cl-colors2:as-rgb "#ACE1AF") ; 9900a4
           width)
          *frame-unfocus-border-style*
          (hrt:border-box-style-create
           :hrt-border-solid (cl-colors2:as-rgb "000000") ; 9900a4
           (* width 2)) ; Add width to hide any gaps from rounded corners
          *frame-unfocus-empty-border-style*
          (hrt:border-box-style-create
           :hrt-border-dotted (cl-colors2:as-rgb "cccccc")
           ;; Note: As long as the width isn't greater than the
           ;; declared width, we are fine, especially
           ;; for empty frames:
          width))))

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

(defun %view-frame-get-dimensions (frame adjust width height)
  (let* ((w-adjust (- width (if (frame-bordered-left frame)
								adjust
								(+ adjust adjust))))
         (h-adjust (- height (if (frame-bordered-top frame)
								 adjust
								 (+ adjust adjust)))))
    (values w-adjust h-adjust)))

(defun %view-frame-get-position (frame adjust x y)
  (let* ((x-adjust (+ x (if (frame-bordered-left frame)
							0
							adjust)))
         (y-adjust (+ y (if (frame-bordered-top frame)
							0
							adjust))))
    (values x-adjust y-adjust)))

(defun %border-box-get-dimensions (frame adjust width height)
  (let ((w-adjust (+ width (if (frame-bordered-left frame)
							   adjust
							   0)))
		(h-adjust (+ height (if (frame-bordered-top frame)
								adjust
								0))))
    (values w-adjust h-adjust)))

(defun %border-box-get-position (frame adjust x y)
  (let* ((x-adjust (- x (if (frame-bordered-left frame)
							adjust
							0)))
         (y-adjust (- y (if (frame-bordered-top frame)
							adjust
							0))))
	(values x-adjust y-adjust)))

(defmethod initialize-instance :after ((frame view-frame)
                                       &key focused view
                                         &allow-other-keys)
  (with-slots (border-box) frame
    (let* ((rounded-x (round (frame-x frame)))
           (rounded-y (round (frame-y frame)))
           (rounded-width (round (frame-width frame)))
           (rounded-height (round (frame-height frame)))
           (box-style (if focused
                          *frame-focus-border-style*
                          (if view
                              *frame-unfocus-border-style*
                              *frame-unfocus-empty-border-style*)))
           (adjust *frame-border-width*))
      (let* ((layer-container (frame-find-layer frame)))
        (multiple-value-bind (x y)
            (%border-box-get-position frame adjust rounded-x rounded-y)
          (multiple-value-bind (width height)
              (%border-box-get-dimensions frame adjust rounded-width rounded-height)
            (setf border-box (hrt:border-box-create
                              (layer-container-layer layer-container)
                              box-style
                              x
                              y
                              width
                              height)))))
      (hrt:hrt-border-box-lower-to-bottom border-box)
      (when view
        (multiple-value-bind (width height)
            (%view-frame-get-dimensions frame adjust rounded-width rounded-height)
          (set-dimensions view width height))
        (multiple-value-bind (x y)
            (%view-frame-get-position frame adjust rounded-x rounded-y)
          (set-position view x y))))))

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
  (let ((adjust *frame-border-width*))
    (%view-frame-get-dimensions frame adjust
                                (round (frame-width frame))
                                (round (frame-height frame)))))

(defun %view-frame-set-dimensions (frame adjust width height)
  (multiple-value-call
      #'set-dimensions
    (frame-surface frame)
    (%view-frame-get-dimensions frame adjust width height)))

(defun %view-frame-set-position (frame adjust x y)
  (multiple-value-call
      #'set-position
    (frame-surface frame)
   (%view-frame-get-position frame adjust x y)))

(defun %border-box-set-position (frame adjust x y)
  (multiple-value-call
      #'hrt:hrt-border-box-set-relative
    (slot-value frame 'border-box)
    (%border-box-get-position frame adjust x y)))

(defun %border-box-set-dimensions (frame adjust width height)
  (multiple-value-call
      #'hrt:hrt-border-box-set-size
    (slot-value frame 'border-box)
    (%border-box-get-dimensions frame adjust width height)))

(defmethod (setf frame-surface) :after (view (frame view-frame))
  "Place the view in the frame and make it have the same dimensions
and position as the frame"
  (with-slots (border-box focused) frame
    (cond
      (view
       (unless focused
         (hrt:border-box-set-style (slot-value frame 'border-box)
                                   *frame-unfocus-border-style*))
       (hrt:with-view-transaction ()
         (let ((adjust *frame-border-width*))
           (multiple-value-bind (width height)
               (%view-frame-get-dimensions
                frame adjust
                (round (frame-width frame)) (round (frame-height frame)))
             (set-dimensions view width height))
           (multiple-value-bind (x y)
               (%view-frame-get-position
                frame adjust
                (round (frame-x frame))
                (round (frame-y frame)))
             (set-position view x y)))
         (when (frame-focused frame)
           (hrt:focus-view view (slot-value frame 'seat)))))
      (t
       (unless focused
         (hrt:border-box-set-style (slot-value frame 'border-box)
                                   *frame-unfocus-empty-border-style*))))))

(defmethod mark-frame-focused :after ((frame view-frame) seat)
  (setf (slot-value frame 'seat) seat)
  (hrt:hrt-border-box-raise-to-top (slot-value frame 'border-box))
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
      (hrt:hrt-border-box-lower-to-bottom (slot-value frame 'border-box))
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
        (round-y (round (frame-y frame)))
        (adjust *frame-border-width*))
    (when (frame-surface frame)
      (%view-frame-set-position frame adjust
                                round-x round-y))
    (%border-box-set-position frame adjust round-x round-y)))

(defmethod (setf frame-y) :before (new-y (frame view-frame))
  (let ((round-y (round new-y))
        (round-x (round (frame-x frame)))
        (adjust *frame-border-width*))
    (when (frame-surface frame)
      (%view-frame-set-position frame adjust round-x round-y))
    (%border-box-set-position frame adjust round-x round-y)))

(defmethod set-dimensions :before ((frame view-frame) width height)
  (let ((w-adjusted (round width))
        (h-adjusted (round height))
        (adjust *frame-border-width*))
    (when (frame-surface frame)
      (%view-frame-set-dimensions frame adjust w-adjusted h-adjusted))
    (%border-box-set-dimensions frame adjust
                                w-adjusted h-adjusted)))

(defmethod set-position :before ((frame view-frame) x y)
  (let ((round-x (round x))
        (round-y (round y))
        (adjust *frame-border-width*))
    (when (frame-surface frame)
      (%view-frame-set-position frame adjust round-x round-y))
    (%border-box-set-position frame adjust round-x round-y)))

(defmethod (setf frame-width) :before (new-width (frame view-frame))
  (let ((round-width (round new-width))
        (round-height (round (frame-height frame)))
        (adjust *frame-border-width*))
    (when (frame-surface frame)
      (%view-frame-set-dimensions frame adjust round-width round-height))
    (%border-box-set-dimensions frame adjust
                                round-width round-height)))

(defmethod (setf frame-height) :before (new-height (frame view-frame))
  (let ((round-width (round (frame-width frame)))
        (round-height (round new-height))
        (adjust *frame-border-width*))
    (when (frame-surface frame)
      (%view-frame-set-dimensions frame adjust round-width round-height))
    (%border-box-set-dimensions frame adjust round-width round-height)))

(defmethod find-view-frame ((frame view-frame) view)
  (when (equal (frame-surface frame) view)
    frame))

;; (defmethod find-view-frame ((frame view-frame)
;;        (view sb-sys:system-area-pointer))
;;   (let ((hrt-view (frame-surface frame)))
;;  (when (and hrt-view
;;       (equal (hrt:view-hrt-view (frame-surface frame)) view))
;;    frame)))
