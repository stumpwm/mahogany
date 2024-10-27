(defpackage #:mahogany
  (:use :cl
	#:alexandria
	#:mahogany/log
	#:mahogany/wm-interface
	#:mahogany/util
	#:mahogany/core
	#:mahogany/keyboard)
  (:local-nicknames (#:tree #:mahogany/tree)))
