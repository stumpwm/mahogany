(in-package #:mahogany)

(defstruct (mahogany-output (:constructor make-mahogany-output (hrt-output)))
  (hrt-output cffi:null-pointer :type cffi:foreign-pointer :read-only t))

(defclass mahogany-state ()
  ((hrt-server :type hrt-server
	       :initarg server
	       :accessor mahogany-state-server)
   (key-state :type key-state
	      :initform (make-key-state nil)
	      :accessor mahogany-state-key-state)
   (keybindings :type list
		:initform nil
		:reader mahogany-state-keybindings)
   (outputs :type vector
	    :initform (make-array 0
				  :element-type 'mahogany-output
				  :adjustable t
				  :fill-pointer t)
	    :accessor mahogany-state-outputs)
   (views :type list
	  :initform nil
	  :reader mahogany-state-views)))
