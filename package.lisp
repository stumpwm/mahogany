(defpackage #:mahogany/wm-interface
  (:use :cl)
  (:export #:view ; view interface
	   #:view-x
	   #:view-y
	   #:view-opacity
	   #:set-dimensions))

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
	   #:view-frame
	   #:frame-view
	   #:frame-modes
	   #:fit-view-into-frame
	   #:leafs-in))

(defpackage #:mahogany/keyboard
  (:use :cl
	#:alexandria
	#:mahogany/log
	#:mahogany/util)
  (:export #:key
	   #:make-key
	   #:print-key
	   #:key-keysym
	   #:key-modifier-mask
	   #:key-modifier-key-p
	   #:parse-key
	   #:kbd
	   #:kbd-parse-error
	   #:define-kmap
	   #:define-key
	   #:kmap-p
	   #:kmap
	   #:kmap-lookup
	   #:key-state
	   #:make-key-state
	   #:key-state-sequence
	   #:key-state-advance
	   #:key-state-active-p))

(defpackage #:mahogany
  (:use :cl
	#:alexandria
	#:mahogany/log
	#:mahogany/wm-interface
	#:mahogany/util
	#:mahogany/core
	#:mahogany/tree
	#:mahogany/keyboard))
