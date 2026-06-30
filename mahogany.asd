;;;; mahogany.asd

#+sbcl
(require 'sb-posix)

(asdf:defsystem #:mahogany
  :description "Mahogany is a tiling window manager for wayland a la stumpwm"
  :author "Stuart Dilts"
  :license  "GPL 2.0"
  :version "0.0.1"
  :depends-on (#:uiop
               #:alexandria
               #:cl-ansi-text
               #:cl-colors2
               #:terminfo
               #:adopt
               #:xkbcommon
               #:iterate
               #:atomics
               #:fset
               #:bordeaux-threads
               #:float-features
               #:cl-interactive
               #:trivial-garbage
               #:cffi)
  :in-order-to ((test-op (test-op mahogany-test)))
  :pathname #p"lisp/"
  :components ((:file "log")
               (:file "util")
               (:file "system" :depends-on ("util" "config" "log"))
               (:module config
                :components ((:file "config-system")))
               (:module ring-list
                :components ((:file "ring-list")))
               (:module interfaces
                :components ((:file "view-interface")))
               (:module theme
                :components ((:file "theme")))
               (:module heart
                :depends-on ("interfaces" "theme" "util" "log")
                :components ((:file "package")
                             (:file "wl-bindings" :depends-on ("package"))
                             (:file "wlr-bindings" :depends-on ("wl-bindings"))
                             (:file "hrt-libs" :depends-on ("package"))
                             (:file "hrt-bindings" :depends-on ("wlr-bindings"))
                             (:file "callback" :depends-on ("package"))
                             (:file "cffi-util" :depends-on ("package"))
                             (:file "hrt-debug" :depends-on ("hrt-bindings")
                              :if-feature :hrt-debug)
                             (:file "message"
                              :depends-on ("cffi-util" "hrt-bindings" "output"))
                             (:file "output"
                              :depends-on ("cffi-util" "hrt-bindings"))
                             (:file "subprocess"
                              :depends-on ("cffi-util" "hrt-bindings" "callback"))
                             (:file "view"
                              :depends-on ("cffi-util" "hrt-bindings"))
                             (:file "scene-group"
                              :depends-on ("cffi-util" "hrt-bindings" "output"))
                             (:file "border-box"
                              :depends-on ("cffi-util" "hrt-bindings"))
                             (:file "server"
                              :depends-on ("package" "hrt-bindings" "callback"))
                             (:file "layer-shell")
                             (:file "transaction" :depends-on ("server"))))
               (:module keyboard
                :depends-on ("util")
                :components ((:file "package")
                             (:file "keytrans")
                             (:file "key")
                             (:file "kmap")))
               (:module tree
                :depends-on ("log" "util" "interfaces" "heart")
                :components ((:file "package")
                             (:file "tree-interface")
                             (:file "tree-parent" :depends-on ("tree-interface"))
                             (:file "output-node" :depends-on ("tree-interface"))
                             (:file "frame" :depends-on ("tree-interface"))
                             (:file "view" :depends-on ("tree-interface"))))
               (:module input-methods
                :depends-on ("package")
                ;; It would be cool to figure out module loading
                ;; so that we could have a bunch of input methods
                ;; but not have them loaded until needed:
                :components ((:file "rofi-input-method")
                             #+sbcl
                             (:file "foot-input-method")))
               (:file "package")
               (:file "command" :depends-on ("input-methods" "globals" "message"))
               (:file "objects" :depends-on ("package" "ring-list"))
               (:file "output-config" :depends-on ("heart"))
               (:file "message" :depends-on ("heart" "config"))
               (:file "group" :depends-on ("objects" "heart" "globals"))
               (:file "state" :depends-on ("objects" "keyboard" "heart" "group"))
               (:file "globals" :depends-on ("objects" "system"))
               (:file "kmap-modes"
                :depends-on ("objects" "globals" "keyboard" "input" "command"))
               (:file "events"
                :depends-on ("globals" "state" "objects" "heart"))
               (:file "input"
                :depends-on ("state" "keyboard" "heart" "command" "message"))
               (:file "key-bindings"
                :depends-on ("kmap-modes" "state" "tree" "input" "command"))
               (:file "main" :depends-on ("heart" "keyboard" "input" "package"))))

(asdf:defsystem #:mahogany/executable
  :build-operation program-op
  :entry-point "mahogany::main"
  :build-pathname "build/mahogany"
  :depends-on (#:mahogany))
