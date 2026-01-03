(defpackage #:mahogany
  (:use :cl
        #:alexandria
        #:mahogany/log
        #:mahogany/wm-interface
        #:mahogany/util
        #:mahogany/keyboard)
  (:local-nicknames (#:tree #:mahogany/tree))
  (:export #:define-kmap
           #:kbd
           #:add-to-kmap
           #:*root-map*
           #:*top-map*
           #:*group-map*
           #+:hrt-debug
           #:*debug-map*))

(defpackage #:mahogany-user
  (:use :cl
        #:mahogany)
  (:documentation "User package for the mahogany system."))
