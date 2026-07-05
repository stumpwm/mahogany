(defpackage #:mahogany/output-config
  (:use #:cl)
  (:export #:find-output-configurations
           #:find-output-config
           #:define-output-config
           #:define-output-layout))

(defpackage #:mahogany
  (:use :cl
        #:alexandria
        #:mahogany/log
        #:mahogany/wm-interface
        #:mahogany/util
        #:mahogany/keyboard)
  (:import-from #:mahogany/output-config
                #:find-output-configurations
                #:find-output-config
                #:define-output-config
                #:define-output-layout)
  (:import-from #:cl-interactive/input-method
                #:input-method
                #:prepare-completions-for-input-method
                #:input-method-read)
  (:local-nicknames (#:tree #:mahogany/tree)
                    (#:colors #:cl-colors2)
                    (#:config #:config-system))
  (:export #:define-kmap
           #:kbd
           #:add-to-kmap
           #:*root-map*
           #:*top-map*
           #:*group-map*
           #+:hrt-debug
           #:*debug-map*))

(defpackage #:mahogany-user
  (:use :cl)
  (:local-nicknames (#:mh #:mahogany))
  (:documentation "User package for the mahogany system."))
