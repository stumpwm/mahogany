(defpackage :mahogany/tree/frame-interface
  (:use :cl))

(in-package :mahogany/tree/frame-interface)

(export '(split-frame-h
	  split-frame-v
	  ;; remove-frame
	  ;; isolate-frame
	  swap-children
	  *split-frame-hook*
	  *new-frame-hook*
	  *remove-split-hook*
	  *new-split-type*
	  frame
	  tree-frame
	  binary-tree-frame
	  poly-tree-frame
	  root-frame-p
	  frame-x
	  frame-y
	  frame-width
	  frame-height
	  frame-parent))

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
Valid choices are :binary or :poly. You can change the split type
of an already existing frame with the `set-split-frame-type` function")

(defclass frame ()
  ((x :initarg :x
      :accessor frame-x
      :type integer)
   (y :initarg :y
      :accessor frame-y
      :type integer)
   (width :initarg :width
	  :accessor frame-width
	  :type integer)
   (height :initarg :height
	   :accessor frame-height
	   :type integer)
   (parent :initarg :parent
	   :accessor frame-parent))
  (:documentation "A frame that is displayed on an output"))

(defclass tree-container ()
  ((tree :initarg :root
	:accessor root-tree
	:type frame
	:documentation "Holds the root of a frame-tree"))
  (:documentation "A class that contains a frame-tree"))

(defclass tree-frame (frame)
  ((children :initarg :children
	     :initform nil
	    :accessor tree-children
	    :type list)
   (split-type :initarg :split-type
	       :reader tree-split-type)
   (split-direction :initarg :split-direction
		    :reader tree-split-direction))
  (:documentation "An inner node of a frame-tree"))

(defclass floating-frame (frame)
  ((top-frame :initarg :top-frame
	      :accessor top-frame
	      :type frame)))

(defclass binary-tree-frame (tree-frame)
  ()
  (:default-initargs
   :split-type :binary)
  (:documentation "An inner node of a frame-tree that can only have two children"))

(defclass poly-tree-frame (tree-frame)
  ()
  (:default-initargs
   :split-type :poly)
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
     the the size is split evenly between the other child frame(s)"))

(defgeneric split-frame-h (frame &key ratio direction)
  (:documentation "Split the frame horizontally. Returns a tree of the split frames.
The parent tree is modified appropriately.
   RATIO: the size of newly created frame compared to the given frame. If not given, then
     the the size is split evenly between the other child frame(s)"))

;; (defgeneric remove-frame (frame)
;;   (:documentation "Remove the frame from the tree. Parent can be any
;; frame higher in the heirarchy."))

;; (defgeneric isolate-frame (frame)
;;   (:documentation "Make the frame the only frame in its tree"))

;; (defgeneric remove-child (parent child)
;;   (:documentation "Remove the child from the parent. Returns a boolean signifying
;; that more children

(defgeneric swap-positions (frame1 frame2)
  (:documentation "Swap the positions of the two frames in their trees."))

(defgeneric find-empty-frame (root)
  (:documentation "Finds a veiw-frame in the given tree that doesn't have
a view assigned to it."))

;; floating frame interface

(defvar *request-client-decoration* nil
  "Controls whether client-side borders and windows are drawn")

(defgeneric is-visible-p (floating-frame)
  (:documentation "Returns if the frame is visible on an output."))

;; helper functions:

(declaim (inline root-frame-p))
(defun root-frame-p (frame)
  ;; the root frame's parent will be a tree-container:
  (typep (frame-parent frame) 'tree-container))
