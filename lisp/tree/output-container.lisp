(in-package #:mahogany/tree)

(defclass output-container ()
  ((hrt-output :initarg :output
               :reader output-container-output
               :type hrt:output)
   (background-layer :initarg :background
                     :type list
                     :initform nil
                     :reader output-container-background)
   (bottom-layer :initarg :bottom
                 :initform nil
                 :type list
                 :reader output-container-bottom)
   (top-layer :initarg :top
              :initform nil
              :type list
              :reader output-container-top)
   (overlay-layer :initarg :overlay
                  :initform nil
                  :type list
                  :reader output-container-overlay)))

(defun output-container-add-layer-shell (output-container surface)
  (declare (type output-container output-container)
           (type hrt::layer-surface surface))
  (let ((node (make-instance 'layer-shell-frame
                             :layer-shell surface))
        (layer (hrt::layer-surface-layer surface)))
    (macrolet ((add-to-layer (slot)
                 `(with-slots (,slot) output-container
                    (push node ,slot)
                    (setf ,slot
                          (sort-by-location ,slot t)))))
      (case layer
        (:layer-background
         (add-to-layer background-layer))
        (:layer-bottom
         (add-to-layer bottom-layer))
        (:layer-top
         (add-to-layer top-layer))
        (:layer-overlay
         (add-to-layer overlay-layer))))
    node))

(defun output-container-remove-layer-shell (output-container surface)
  (declare (type output-container output-container)
           (type hrt::layer-surface surface))
  (macrolet ((remove-from-layer (slot)
               `(with-slots (,slot) output-container
                  (let ((var (find surface ,slot
                                   :key (lambda (x)
                                          (slot-value x 'layer-shell)))))
                    (cond
                      (var
                       (setf ,slot (sort-by-location (remove var ,slot) t))
                       var)
                      (t
                       (log-string
                        :error
                        "Could not find layer shell an any layers: ~S"
                        surface)
                       nil))))))
    (case (hrt::layer-surface-layer surface)
      (:layer-background
       (remove-from-layer background-layer))
      (:layer-bottom
       (remove-from-layer bottom-layer))
      (:layer-top
       (remove-from-layer top-layer))
      (:layer-overlay
       (remove-from-layer overlay-layer)))))

(declaim (inline output-container-output-ptr))
(defun output-container-output-ptr (container)
  (declare (type output-container container))
  (hrt:output-hrt-output (output-container-output container)))

(defun make-output-container (output)
  (declare (type hrt:output output))
  ;; (let ((output-scene (hrt:output-scene output)))
  ;; (macrolet ((make-layer (layer)
  ;;              ;; We use a macro so we can open code the access to the layer:
  ;;              `(make-instance 'layer-container
  ;;                              :hrt-layer (hrt:output-scene-layer output-scene
  ;;                                                                 ,layer))))
  (make-instance 'output-container
                 :output output
                 ;; :background (make-layer :layer-background)
                 ;; :bottom (make-layer :layer-bottom)
                 ;; :top (make-layer :layer-top)
                 ;; :overlay (make-layer :layer-overlay)
                 ))
;;))

(defmethod frame-position ((container output-container))
  (hrt:output-position (output-container-output container)))

(defun output-container-search-top (container x y)
  (declare (type output-container container))
  (macrolet ((search-layer (layer)
               ;; Since the list is sorted, we might be able to
               ;; do something smarter than look at the whole
               ;; list, but it should be fairly short:
               `(dolist (f (slot-value container (quote ,layer)))
                  (when (in-frame-p f x y)
                    (return-from output-container-search-top f)))))
    (search-layer overlay-layer)
    (search-layer top-layer)))

(defun output-container-search-bottom (container x y)
  (declare (type output-container container))
  ;; The lists are sorted, so we can skip some
  (macrolet ((search-layer (layer)
               `(dolist (f (slot-value container (quote ,layer)))
                  (when (in-frame-p f x y)
                    (return-from output-container-search-bottom f)))))
    (search-layer bottom-layer)
    (search-layer background-layer)))

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
