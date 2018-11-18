(defpackage #:mahogany/log
  (:use :cl :cl-ansi-text))

(defpackage #:mahogany/util
  (:use #:cl))

(defpackage #:mahogany/tree
  (:use :cl #:mahogany/log #:alexandria #:mahogany/util)
  (:export #:split-frame-h
	   #:split-frame-v
	   #:swap-positions
	   #:*split-frame-hook*
	   #:*new-frame-hook*
	   #:*remove-split-hook*
	   #:*new-split-type*
	   #:frame
	   #:tree-frame
	   #:binary-tree-frame
	   #:poly-tree-frame
	   #:root-frame-p
	   #:frame-x
	   #:frame-y
	   #:frame-width
	   #:frame-height
	   #:frame-parent
	   #:tree-container
	   #:tree-children
	   #:root-tree))

(defpackage #:mahogany/backend
  (:use :cl :cffi #:mahogany/log #:alexandria #:wlr/macros #:wlr/common-c-types)
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
		#:with-keymap-from-names))
