(in-package #:mahogany)

(defstruct (mahogany-group (:constructor %make-mahogany-group
                               (name number hrt-group tiled-container)))
  (name "" :type string)
  (number 1 :type fixnum :read-only t)
  (active-p nil :type boolean)
  (hrt-group (cffi:null-pointer) :type cffi:foreign-pointer :read-only t)
  (tiled-container nil :type tree:layer-container :read-only t)
  (output-map (make-hash-table :test 'equal) :type hash-table :read-only t)
  (current-frame nil :type (or tree:frame tree:output-node null))
  (hidden-views (ring-list:make-ring-list) :type ring-list:ring-list)
  ;; Map that contains views hidden by fullscreen windows.
  ;; view -> %hidden-view-info
  (hidden-view-map (make-hash-table) :type hash-table)
  (views nil :type list))

(defclass mahogany-state ()
  ((hrt-server :type hrt-server
               :initarg server
               :accessor state-server)
   (key-state :type key-state
              :initform (make-key-state nil)
              :accessor server-key-state)
   (current-group :type mahogany-group
                  :accessor state-current-group)
   (keybindings :type list
                :initform nil
                :reader state-keybindings)
   (active-kmap-modes :type list
                      :initform nil
                      :accessor state-active-kmap-modes)
   (prefix-key :type key
               :initform (kbd "C-t")
               :reader state-prefix-key
               :documentation "The prefix key used for prefix-bound kmaps.")
   (outputs :type (vector hrt:output *)
            :initform (make-array 0
                                  :element-type 'hrt:output
                                  :adjustable t
                                  :fill-pointer t)
            :accessor state-outputs)
   (groups :type vector
           :accessor state-groups
           :initform (make-array 0 :element-type 'mahogany-group :adjustable t :fill-pointer t))
   (hidden-groups :initform (ring-list:make-ring-list)
                  :type ring-list:ring-list
                  :reader state-hidden-groups)
   (views :type hash-table
          :initform (make-hash-table)
          :reader state-views)))
