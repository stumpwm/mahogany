(in-package :mahogany/tree)

;; various hooks
(defvar *split-frame-hook* nil
  "Hook that is called when a frame is split. It calls a function
with two arguments: the first is the newly created frame, and the second
is the parent of the new frame.")

(defvar *new-frame-hook* nil
  "Hook that is called whenever a new leaf frame is created. The function
called should expect one argument, the newly created frame.")

(defvar *remove-split-hook* nil
  "Hook for when a frame is removed. Called with the deleted frame
as the first argument.")

(defvar *new-split-type* :binary
  "Directs a newly split frame to have two or many children.
Valid choices are :binary or :many. You can change the split type
of an already existing frame with the `set-split-frame-type` function")

(defclass frame ()
  ((x :initarg :x
      :accessor frame-x
      :type real)
   (y :initarg :y
      :accessor frame-y
      :type real)
   (width :initarg :width
	  :accessor frame-width
	  :type real)
   (height :initarg :height
	   :accessor frame-height
	   :type real)
   (parent :initarg :parent
	   :accessor frame-parent)
   (focused :initarg :focused
	    :reader frame-focused
	    :initform nil
	    :type boolean))
  (:documentation "A frame that is displayed on an output"))

(defgeneric frame-prev (frame)
  (:documentation "Get the previous edge node in the tree from this node"))

(defgeneric frame-next (frame)
  (:documentation "Get the next edge node in the tree from this node"))

;; Use a different symbol for the setters so we don't export them:
(defgeneric (setf %frame-prev) (prev frame)
  (:documentation "Set the previous frame from this frame"))

(defgeneric (setf %frame-next) (next frame)
  (:documentation "Set tne next frame for this frame"))

(defclass tree-container ()
  ((tree :initarg :root
	:accessor root-tree
	:type frame
	:documentation "Holds the root of a frame-tree"))
  (:documentation "A class that contains a frame-tree"))

(deftype split-frame-type ()
  '(member :vertical :horizontal))

(defclass tree-frame (frame)
  ((children :initarg :children
	     :initform nil
	    :accessor tree-children
	    :type list)
   (split-direction :initarg :split-direction
		    :reader tree-split-direction
		    :type split-frame-type))
  (:documentation "An inner node of a frame-tree"))

(defclass floating-frame (frame)
  ((top-frame :initarg :top-frame
	      :accessor top-frame
	      :type frame)))

(defclass binary-tree-frame (tree-frame)
  ()
  (:documentation "An inner node of a frame-tree that can only have two children"))

(defclass poly-tree-frame (tree-frame)
  ()
  (:documentation "An inner node of a frame-tree that can have more than two children"))

;; frame-tree interface
(defgeneric set-split-frame-type (frame type)
  (:documentation "Sets the split frame type. Note that this may change the
the layout of the tree depending on the frame type.
See *new-split-type* for more details"))

(defgeneric split-frame-v (frame &key ratio direction)
  (:documentation "Split the frame vertically. Returns a tree of the split frames.
The parent tree is modified appropriately.
   RATIO: the size of newly created frame compared to the given frame. If not given, then
     the the size is split evenly between the other child frame(s)
   DIRECTION: where the new frame is placed. Either :left or :right"))

(defgeneric split-frame-h (frame &key ratio direction)
  (:documentation "Split the frame horizontally. Returns a tree of the split frames.
The parent tree is modified appropriately.
   RATIO: the size of newly created frame compared to the given frame. If not given, then
     the the size is split evenly between the other child frame(s)
   DIRECTION: where the new frame is placed. Either :top or :bottom"))

(defgeneric remove-frame-from-parent (parent frame cleanup-func)
  (:documentation "Remove the frame from the tree. Parent must be the direct parent of frame."))

(defun remove-frame (frame &optional (cleanup-func #'identity))
  "Remove the frame from the tree. The remaining children grow to equally take up the available space.
e.g. If there are three frames of width (20, 40, 40), and the 20 width one is removed, the new widths
will be (40, 40). If a tree only has one child left, it is replaced with its child.
CLEANUP-FUNC is called on the removed frame(s) after they are removed."
  (check-type frame frame)
  (remove-frame-from-parent (frame-parent frame) frame cleanup-func))

(defgeneric replace-frame (root frame &optional cleanup-func)
  (:documentation "Replace ROOT with FRAME. Call CLEANUP-FUNC on every view-frame that is removed
from the tree. "))

(defgeneric find-empty-frame (root)
  (:documentation "Finds the first veiw-frame in the given tree that doesn't have
a view assigned to it."))

(defgeneric frame-at (root x y)
  (:documentation "Get the frame that occupies the specified coordinates."))

(defgeneric mark-frame-focused (frame seat)
  (:documentation "Mark the frame as being focused")
  (:method ((frame frame) seat)
    (declare (ignore seat))
    (log-string :trace "frame focused")
    (setf (slot-value frame 'focused) t)))

(defgeneric unmark-frame-focused (frame seat)
  (:documentation "Mark the frame as being focused")
  (:method ((frame frame) seat)
    (declare (ignore seat))
    (log-string :trace "frame focused")
    (setf (slot-value frame 'focused) nil)))

;; helper functions:

(defun root-frame-p (frame)
  ;; the root frame's parent will be a tree-container:
  (typep (frame-parent frame) 'tree-container))

(defun find-frame-container (frame)
  "Find the toplevel frame container for this frame"
  (declare (type frame frame))
  (do ((cur-frame frame (frame-parent cur-frame)))
      ((typep cur-frame 'tree-container) cur-frame)))

(defun make-basic-tree (&key (x 0) (y 0) (width 100) (height 100))
  (let ((container (make-instance 'tree-container))
	(frame (make-instance 'view-frame :x x :y y :width width :height height)))
    (setf (%frame-next frame) frame
	  (%frame-prev frame) frame)
    (setf (frame-parent frame) container)
    (setf (root-tree container) frame)
    (values container frame)))

(snakes:defgenerator leafs-in (frame)
  (check-type frame frame)
  (if (typep frame 'tree-frame)
      (let ((stack (tree-children frame)))
	(iter (for child = (pop stack))
	      (while child)
	      (if (typep child 'tree-frame)
		  (appendf stack (tree-children child))
		  (snakes:yield child))))
      (snakes:yield frame)))
