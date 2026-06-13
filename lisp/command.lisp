(in-package #:mahogany)

(setf cl-interactive:*default-input-method* (make-instance 'foot-input-method))

(defun interactively-read-string (com im arg prompt)
  (declare (ignore com arg))
  (cl-interactive:input-method-read im prompt :require-match nil))

(defmacro defcommand (name args &body impl)
  "Define a command that can be invoked interactively via a keybinding.

ARGS defines the interactive and non-interactive arguments to the function;
Interactive functions are denoted with (ARG-NAME ARG-SPEC), where ARG-SPEC
describes how the argument is obtained interactively.

Non-interactive arguments are injected from the surrounding context. Currently,
two symbols are supported:
+ mahogany::seat, which is the seat that triggered the keybinding
+ mahogany::sequence, which is the sequence of keys pressed to trigger the
  keybinding.

BODY is the set of valid options for defgeneric.

See the documentation for cl-interactive::define-command for more details.
"
  `(cl-interactive:define-command ,name ,args
     ,@impl))
