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

(defstruct (mahogany-state (:conc-name state-))
  (server nil :type (or null cffi:foreign-pointer))
  (key-state (make-key-state nil):type key-state)
  (%current-group nil :type (or null mahogany-group))
  (%current-frame nil)
  (%keybindings nil :type list)
  (active-kmap-modes nil :type list)
  (%prefix-key (kbd "C-t") :type key)
  (outputs (make-array 0 :element-type 'hrt:output :adjustable t :fill-pointer t)
   :type (vector hrt:output *))
  (groups (make-array 0 :element-type 'mahogany-group :adjustable t :fill-pointer t)
   :type (vector mahogany-group *))
  (hidden-groups (ring-list:make-ring-list)
   :type ring-list:ring-list
   :read-only t)
  (views (make-hash-table)
   :type hash-table
   :read-only t)
  (layer-surfaces (make-hash-table)
   :type hash-table
   :read-only t))

(declaim (inline state-current-group))
(defun state-current-group (state)
  (declare (type mahogany-state state))
  (state-%current-group state))

(declaim (inline state-current-frame))
(defun state-current-frame (state)
  (declare (type mahogany-state state))
  (state-%current-frame state))

(declaim (inline state-keybindings))
(defun state-keybindings (state)
  (declare (type mahogany-state state))
  (state-%keybindings state))

(declaim (inline state-prefix-key))
(defun state-prefix-key (state)
  (declare (type mahogany-state state))
  (state-%prefix-key state))

(declaim (inline server-seat))
(defun server-seat (state)
  (hrt:hrt-server-seat (state-server state)))
