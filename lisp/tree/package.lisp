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
	   #:tree-container-add
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
	   #:get-populated-frames
	   #:root-frame-p
	   #:find-root-frame
	   #:find-first-leaf
	   #:mark-frame-focused
	   #:unmark-frame-focused
	   #:replace-frame
	   #:reconfigure-node
	   ;; View-frame functions / objects
	   #:view-frame
	   #:frame-view
	   #:frame-next
	   #:frame-prev
	   #:leafs-in))
