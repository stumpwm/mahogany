(in-package #:mahogany)

(defstruct (kmap-mode (:constructor
                       make-kmap-mode (name top-binding prefix-binding doc)))
  (name nil :type symbol :read-only t)
  (doc "" :type (or null string) :read-only t)
  (top-binding (make-kmap) :type kmap :read-only t)
  (prefix-binding (make-kmap) :type kmap :read-only t)
  (top-kmap nil :type (or null kmap)))

(defun kmap-mode-deactivate (state kmap-mode)
  (declare (type kmap-mode kmap-mode)
           (mahogany-state state))
  (with-accessors ((active-bindings mahogany-state-keybindings)
                   (active-modes mahogany-active-kmap-modes))
      state
    (labels ((relevant-kmap-p (kmap)
               (declare (type kmap kmap))
               (or (eq (kmap-mode-top-kmap kmap-mode) kmap)
                   (eq (kmap-mode-top-binding kmap-mode) kmap))))
      (setf active-bindings (delete-if #'relevant-kmap-p active-bindings)))
    (setf (kmap-mode-top-kmap kmap-mode) nil)
    (setf active-modes (delete kmap-mode active-modes))))

(defun %kmap-mode-active-in-state (state kmap-mode)
  (declare (type kmap-mode kmap-mode)
           (type mahogany-state state))
  (member kmap-mode (mahogany-active-kmap-modes state) :test #'eq))

(defun kmap-mode-activate (state kmap-mode)
  (declare (type kmap-mode kmap-mode)
           (type mahogany-state state))
  ;; if it already appears to be active, do nothing:
  (when (%kmap-mode-active-in-state state kmap-mode)
    (assert (not (null (kmap-mode-top-kmap kmap-mode))))
    (return-from kmap-mode-activate))
  (with-accessors ((keybindings mahogany-state-keybindings)
                   (active-kmap-modes mahogany-active-kmap-modes)
                   (prefix-key mahogany-state-prefix-key))
      state
    (let ((top-kmap (define-kmap
                      prefix-key (kmap-mode-prefix-binding kmap-mode))))
      (setf (kmap-mode-top-kmap kmap-mode) top-kmap)
      (push top-kmap keybindings)
      (push (kmap-mode-top-binding kmap-mode) keybindings)
      (push kmap-mode active-kmap-modes)))
  (values))

(defparameter *kmap-modes* nil
  "List of defined kmap modes")

(defun %validate-mode-symbol (symbol)
  (let* ((name (symbol-name symbol))
         (len (length name)))
    (cond
      ((<= len 5)
       (error "Mode symbol cannot be named \"-mode\""))
      ((not (string-equal "-mode" (subseq name (- len 5))))
       (error "Mode symbol name must end in \"-mode\"")))))

;; TODO: Make it possible to redefine kmap-modes,
;;  get a list of available kmap modes
(defmacro define-kmap-mode (name &key
                                   documentation
                                   (top-binding (make-kmap))
                                   (prefix-binding (make-kmap)))

  (%validate-mode-symbol name)
  `(let ((kmap-mode (make-kmap-mode (quote ,name)
                                    ,top-binding
                                    ,prefix-binding
                                    ,documentation)))
     (pushnew kmap-mode *kmap-modes* :key #'kmap-mode-name)
     (defun ,name (&optional (activate t))
       (if activate
           (kmap-mode-activate *compositor-state* kmap-mode)
           (kmap-mode-deactivate *compositor-state* kmap-mode)))))

(defun (setf mahogany-state-prefix-key) (key state)
  (declare (type key key)
           (type mahogany-state state))
  (with-accessors ((active-modes mahogany-active-kmap-modes))
      state
    (let ((new-bindings nil))
      ;; go backwards so pushing gets us the same order:
      (dolist (mode active-modes)
        (let ((new-top-kmap (define-kmap
                              key (kmap-mode-prefix-binding mode))))
          (setf (kmap-mode-top-kmap mode) new-top-kmap)
          (push new-top-kmap new-bindings)
          (push (kmap-mode-top-binding mode) new-bindings)))
      (setf (mahogany-state-keybindings state) new-bindings
            (slot-value state 'prefix-key) key))))
