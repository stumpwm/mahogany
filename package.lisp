(defpackage #:mahogany/log
  (:use :cl #:cl-ansi-text))

(defpackage #:mahogany/util
  (:use #:cl))

(defpackage #:mahogany/backend-interface
  (:use :cl)
  (:export #:set-window-manager
	   #:start-backend
	   #:stop-backend
	   #:cleanup-backend
	   ;; output interface
	   #:output-width
	   #:output-height
	   ;; view interface
	   #:set-dimensions
	   #:view-y
	   #:view-x))

(defpackage #:mahogany/wm-interface
  (:use :cl)
  (:export #:get-visible-views
	   #:set-backend
	   #:add-view
	   #:remove-view))

(defpackage #:mahogany/tree
  (:use :cl #:mahogany/log #:alexandria #:mahogany/util #:iterate
	#:mahogany/backend-interface)
  (:export #:*split-frame-hook*
	   #:*new-frame-hook*
	   #:*remove-split-hook*
	   #:*new-split-type*
	   #:frame
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
	   #:set-dimensions
	   #:root-frame-p
	   #:view-frame
	   #:frame-view
	   #:frame-modes
	   #:fit-view-into-frame
	   #:leafs-in))

(defpackage #:mahogany/backend
  (:use :cl :cffi #:mahogany/log #:alexandria #:wlr/macros #:wlr/common-c-types
	#:mahogany/backend-interface #:mahogany/wm-interface)
  (:import-from #:wayland-server-core
		#:wl-display-add-socket-auto
		#:wl-display-destroy
		#:wl-display-destroy-clients
		#:wl-display-run
		#:wl-display-terminate
		#:wl-signal-add
		#:wl-list-remove
		#:wl-list-empty
		#:wl_listener
		#:wl_list
		#:prev
		#:notify
		#:link)
  (:import-from #:xkb
		#:with-xkb-context
		#:with-keymap-from-names)
  (:export #:get-server))

(defpackage #:mahogany/wm
  (:use :cl #:mahogany/log #:alexandria
	#:mahogany/wm-interface
	#:mahogany/backend-interface
	#:mahogany/tree)
  (:export #:window-manager))

(defpackage #:mahogany
  (:use :cl #:mahogany/log #:alexandria #:mahogany/wm-interface
	#:mahogany/backend-interface
	#:mahogany/tree)
  (:export #:*wm* #:*backend*))
