(defpackage #:mahogany/log
  (:use :cl :cl-ansi-text))

(defpackage #:mahogany/backend-interface
  (:use :cl)
  (:export #:set-dimensions)

(defpackage #:mahogany/wm-interface
  (:use :cl)
  (:export #:get-visible-views
	   #:add-view
	   #:remove-view))

(defpackage #:mahogany/backend
  (:use :cl :cffi #:mahogany/log #:alexandria #:wlr/macros #:wlr/common-c-types
	#:mahogany/backend-interface)
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
