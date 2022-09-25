(in-package #:mahogany/keyboard)

(defstruct (binding (:constructor make-binding (key command)))
  (key nil :type key)
  command)

(defstruct kmap
  ;; TODO: Maybe this should just be a hash table instead?
  (bindings (make-array 0 :element-type 'binding :adjustable t :fill-pointer t)
	    :type (vector binding)
	    :read-only t))

(defun define-key (map key cmd)
  "Add a binding from the given key to the given command in the keymap. If the command
is nil, remove the binding for the given key."
  (declare (type kmap map)
	   (type key key))
  (let ((bindings (kmap-bindings map))
	(new-binding (make-binding key cmd)))
    (dotimes (i (length bindings))
      (let ((val (binding-key (aref bindings i))))
	(when (equalp val key)
	  (setf (aref bindings i) new-binding)
	  (return-from define-key new-binding))))
    (vector-push-extend new-binding bindings 1))
  map)

(defmacro define-kmap (&body body)
  "Create a keymap and add the given bindings to it.

Example:
   (define-kmap
     (kbd \"C-s\") 'foo
     (kbd \"M-;\") 'bar)"
  (let* ((map-var (gensym "kmap")))
    `(let ((,map-var (make-kmap)))
       ,@(do* ((next body (cddr next))
	       (key (car body) (car next))
	       (binding (cadr body) (cadr next))
	       (forms nil forms))
	      ((null next) (nreverse forms))
	   (push `(define-key ,map-var ,key ,binding) forms))
       ,map-var)))

(declaim (inline %kmap-symbol-p))
(defun %kmap-symbol-p (x)
  (and (symbolp x)
       (boundp x)
       (kmap-p (symbol-value x))))

(declaim (inline %kmap-symbol-p))
(defun kmap-or-kmap-symbol-p (x)
  (or (kmap-p x)
      (%kmap-symbol-p x)))

(defun kmap-lookup (keymap key)
  "Find the command associated with the given key in the keymap"
  (declare (type key key)
	   (type kmap keymap))
  (let ((ret (find key (kmap-bindings keymap) :key 'binding-key :test 'equalp)))
    (when ret
      (binding-command ret))))

(defstruct (key-state (:constructor make-key-state (kmaps)))
  (sequence nil :type list)
  (kmaps nil :type list))

(defun key-state-advance (key bindings-state)
  "Advance the key state with the given key by destructively modifying the
given state and its properties.
Arguments:
   KEY: The key object to advance the state with
   BINDINGS-STATE: The key-state object to advance

Returns:
   NIL: The key state is not finished and needs more keys to reach the end.
   T:   The key state has reached the end of the kmaps without finding a command
   non-T object: The key state has finished and the returned object was found."
  (declare (type key-state bindings-state)
           (type key key)
	   (optimize speed))
  (with-accessors ((kmaps key-state-kmaps)
                   (key-seq key-state-sequence))
      bindings-state
    (push key key-seq)
    (let* ((matching-values (mapcar (lambda (m)
				      (kmap-lookup (if (%kmap-symbol-p m)
						      (symbol-value m)
						      m)
						  key))
				    kmaps))
	   (match (find-if-not #'null matching-values)))
      (cond
	((kmap-or-kmap-symbol-p match)
	 (setf (key-state-kmaps bindings-state) (delete-if-not 'kmap-or-kmap-symbol-p matching-values))
	 (values nil))
	(match
	 (setf (key-state-kmaps bindings-state) nil)
	 (values match))
	(t
	 (setf kmaps nil)
	 (values t))))))
