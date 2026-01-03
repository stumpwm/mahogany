(defpackage #:mahogany
  (:use :cl
        #:alexandria
        #:mahogany/log
        #:mahogany/wm-interface
        #:mahogany/util
        #:mahogany/keyboard)
  (:local-nicknames (#:tree #:mahogany/tree)))

(defpackage #:mahogany-user
  (:use :cl
        #:mahogany)
  (:documentation "User package for the mahogany system."))
