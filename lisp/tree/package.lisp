(defpackage #:mahogany/tree
  (:use :cl
	#:alexandria
	#:iterate
	#:mahogany/log
	#:mahogany/util
	#:mahogany/wm-interface)
  (:export #:*split-frame-hook*
	   #:*new-frame-hook*
	   #:*remove-split-hook*
	   #:*new-split-type*
	   #:frame
	   #:frame-at
	   #:frame-x
	   #:frame-y
	   #:frame-width
	   #:frame-height
	   #:frame-parent
	   #:tree-container
	   #:make-basic-tree
	   #:root-tree
	   #:tree-frame
	   #:tree-children
	   #:tree-split-direction
	   #:binary-tree-frame
	   #:poly-tree-frame
	   #:split-frame-v
	   #:split-frame-h
	   #:remove-frame
	   #:swap-positions
	   #:find-empty-frame
	   #:get-empty-frames
	   #:get-populated-frames
	   #:root-frame-p
	   ;; View-frame functions / objects
	   #:view-frame
	   #:frame-view
	   #:put-view-in-frame
	   #:leafs-in))
