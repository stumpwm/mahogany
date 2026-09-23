(in-package #:wl)

;; This should really be a function, but
;; I couldn't get SBCL to use `foreign-slot-offset`'s compiler
;; macro (which looks up the value at compile time)
;; by making this a function and marking it as inline.
;; It appears that compiler macros aren't expanded after inlining,
;; which makes sense depending on what intermediate representation
;; actually gets inlined.
(defmacro container-of (ptr type member)
  `(cffi:make-pointer (- (cffi:pointer-address ,ptr)
                         (cffi:foreign-slot-offset ,type ,member))))

(cffi:defcstruct wl-list
  (prev :pointer)
  (next :pointer))

(defmacro list-for-each ((elem (head member type)) &body body)
  (let ((head-var (gensym "head"))
        (cur-list (gensym "cur-list")))
    ;; Follow the `next` pointers until we get back to where we started.
    ;; The current element is grabbed using the container-of macro.
    `(let* ((,head-var ,head))
       (do* ((,cur-list (cffi:foreign-slot-value ,head-var '(:struct wl:wl-list)
                                                 'next)
                        (cffi:foreign-slot-value ,cur-list '(:struct wl:wl-list)
                                                 'next))
             (,elem (container-of ,cur-list ,type ,member)
                    (container-of ,cur-list ,type ,member)))
            ((cffi:pointer-eq ,cur-list ,head-var))
         ,@body))))

(cffi:defcstruct wl-listener
  "wl_listener struct"
  (link (:struct wl-list))
  (notify :pointer))

(cffi:defcenum wl-output-transform
  +output-transform-normal+
  +output-transform-90+
  +output-transform-180+
  +output-transform-270+
  +output-transform-flipped+
  +output-transform-flipped-90+
  +output-transform-flipped-180+
  +output-transform-flipped-270+)

(cffi:defcenum wl-keyboard-key-state
  +wl-keyboard-key-state-released+
  +wl-keyboard-key-state-pressed+
  +wl-keyboard-key-state-repeated+)

(cffi:defcenum zwlr-layer-surface-v1-keyboard-interactivity
  (:keyboard-interactivity-none 0)
  (:keyboard-interactivity-exclusive 1)
  (:keyboard-interactivity-on-demand 2))

(cffi:defcenum zwlr-layer-shell-v1-layer
  (:layer-background 0)
  (:layer-bottom 1)
  (:layer-top 2)
  (:layer-overlay 3))
