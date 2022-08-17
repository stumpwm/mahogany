(defpackage #:mahogany/core
  (:use :cl #:wayland-server-core #:xkb)
  (:nicknames #:hrt)
  (:export #:hrt-output-callbacks
	   #:hrt-seat-callbacks
	   #:hrt-seat
	   #:hrt-output
	   #:hrt-keypress-info
	   #:output-added
	   #:output-removed
	   #:button-event #:wheel-event #:keyboard-keypress-event
	   #:hrt-server
	   #:hrt-server-init
	   #:hrt-server-start
	   #:hrt-server-run
	   #:hrt-server-finish))
