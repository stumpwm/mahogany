(defpackage :mahogany/tree/group-interface
  (:use :cl))

(defclass group ()
  ((name :initarg :name
	 :accessor group-name
	 :type string)
   (number :initarg :number
	   :accessor group-number
	   :type integer)
   (views :initarg :views
	  :accessor group-views
	  :type list
	  :documentation "The list of all clients that are associated with this group")
   (tree :initarg :tree
	 :accessor tree-children
	 :type frame
	 :documentation "The tiled frames of this group")
   (floating :initarg :floating-frames
	     :reader group-floating-frames
	     :type list
	     :documentation "The current floating frames of this group")
   (visible-frames :initarg :visible-frames
		   :accessor group-visible-frames
		   :type list
		   :documentation "The frames that are currently visible in the group")))

(defgeneric add-new-view (group view)
  (:documentation "Add a new veiw to a group"))

(defgeneric move-view (owner view new-owner)
  (:documentation "Moves a view from one group to another"))

(defgeneric move-frame (owner frame new-owner &optional move-type)
  (:documentation "Moves a frame from one group to another. Specify move-type to
make the new frame floating or tiled. By default, the frame will be whatever type
it was in its original group."))

(defgeneric remove-view (owner view)
  (:documentation "Remove the view from the group."))

(defgeneric group-indicate-focus (group)
  (:documentation "The group is asked to in some way show the user where the keyboard focus is."))

(defgeneric group-focus-view (group view)
  (:documentation "The group is asked to focus the specified window wherever it is."))

(defgeneric group-wake-up (group)
  (:documentation "When the group becomes the current group, this
function is called. This call is expected to set the focus."))

(defgeneric group-suspend (group)
  (:documentation "When the group is no longer the current group, this
function is called."))

(defgeneric group-add-output (group output)
  (:documentation "Add a new output to the group."))

(defgeneric group-remove-output (group output)
  (:documentation "Remove the output from the group."))
