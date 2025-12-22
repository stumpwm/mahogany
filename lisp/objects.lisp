(in-package #:mahogany)

(defstruct (mahogany-output (:constructor %make-mahogany-output
					  (hrt-output hrt-scene full-name)))
  (hrt-output cffi:null-pointer :type cffi:foreign-pointer :read-only t)
  (hrt-scene  cffi:null-pointer :type cffi:foreign-pointer :read-only t)
  (full-name "" :type string :read-only t))

(defstruct (mahogany-group (:constructor %make-mahogany-group (name number hrt-group)))
  (name "" :type string)
  (number 1 :type fixnum :read-only t)
  (hrt-group (cffi:null-pointer) :type cffi:foreign-pointer :read-only t)
  (tree-container (make-instance 'tree:tree-container) :type tree:tree-container :read-only t)
  (output-map (make-hash-table :test 'equal) :type hash-table :read-only t)
  (current-frame nil :type (or tree:frame null))
  (hidden-views (ring-list:make-ring-list) :type ring-list:ring-list)
  (views nil :type list))

(defclass mahogany-state ()
  ((hrt-server :type hrt-server
	       :initarg server
	       :accessor mahogany-state-server)
   (hrt-scene :type hrt-scene
	      :accessor mahogany-state-scene)
   (key-state :type key-state
	      :initform (make-key-state nil)
	      :accessor mahogany-state-key-state)
   (current-group :type mahogany-group
		  :accessor mahogany-current-group)
   (keybindings :type list
		:initform nil
				:reader mahogany-state-keybindings)
   (active-kmap-modes :type list
					  :initform nil
					  :accessor mahogany-active-kmap-modes)
   (prefix-key :type key
			   :initform (kbd "C-t")
			   :reader mahogany-state-prefix-key
			   :documentation "The prefix key used for prefix-bound kmaps.")
   (outputs :type (vector mahogany-output *)
	    :initform (make-array 0
				  :element-type 'mahogany-output
				  :adjustable t
				  :fill-pointer t)
	    :accessor mahogany-state-outputs)
   (groups :type vector
	   :accessor mahogany-state-groups
	   :initform (make-array 0 :element-type 'mahogany-group :adjustable t :fill-pointer t))
   (hidden-groups :initform (ring-list:make-ring-list)
				  :type ring-list:ring-list
				  :reader mahogany-state-hidden-groups)
   (views :type hash-table
	  :initform (make-hash-table)
	  :reader mahogany-state-views)))
