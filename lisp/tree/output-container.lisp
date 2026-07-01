(in-package #:mahogany/tree)

(defclass output-container ()
  ((hrt-output :initarg :output
               :reader output-container-output
               :type hrt:output)
   (background-layer :initarg :background
                     :type layer-container
                     :reader output-container-background)
   (bottom-layer :initarg :bottom
                 :type layer-container
                 :reader output-container-bottom)
   (top-layer :initarg :top
              :type layer-container
              :reader output-container-top)
   (overlay-layer :initarg :overlay
                  :type layer-container
                  :reader output-container-overlay)))

(declaim (inline output-container-output-ptr))
(defun output-container-output-ptr (container)
  (declare (type output-container container))
  (hrt:output-hrt-output (output-container-output container)))

(defun make-output-container (output)
  (declare (type hrt:output output))
  (let ((output-scene (hrt:output-scene output)))
    (macrolet ((make-layer (layer)
                 ;; We use a macro so we can open code the access to the layer:
                 `(make-instance 'layer-container
                                 :hrt-layer (hrt:output-scene-layer output-scene
                                                                    ,layer))))
      (make-instance 'output-container
                     :output output
                     :background (make-layer :layer-background)
                     :bottom (make-layer :layer-bottom)
                     :top (make-layer :layer-top)
                     :overlay (make-layer :layer-overlay)))))

(defmethod frame-position ((container output-container))
  (hrt:output-position (output-container-output container)))

(defun %left-to-right (a b)
  (multiple-value-bind (a-x a-y)
      (frame-position a)
    (multiple-value-bind (b-x b-y)
        (frame-position b)
      (if (= a-y b-y)
          (< a-x b-x)
          (< a-y b-y)))))

(defun sort-by-location (items &optional modify)
  "Rearrange the frame-next and frame-prev pointers in ITEMS so that
the linked list is sorted left-to-right. Returns a sorted sequence
of the same type as ITEMS."
  (declare (type sequence items))
  (let ((seq (if modify items (copy-seq items))))
    (when (> (length items) 0)
      (let* ((sorted (sort seq #'%left-to-right))
             (first (elt sorted 0))
             (cur first))
        (loop for elem being the elements of sorted
              do (setf (%frame-next cur) elem
                       (%frame-prev elem) cur
                       cur elem))
        (setf (%frame-next cur) first
              (%frame-prev first) cur)
        seq))
    seq))
