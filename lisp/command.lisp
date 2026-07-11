(in-package #:mahogany)

(setf cl-interactive:*default-input-method*
      #+(and sbcl linux)
      (make-instance 'foot-input-method)
      #-(and sbcl linux)
      (make-instance 'rofi-input-method))

(defparameter *input-methods-available*
  (list
   'rofi-input-method
   #+(and sbcl linux)
   'foot-input-method))

(defun interactively-read-string (com im arg prompt)
  (declare (ignore com arg))
  (cl-interactive:input-method-read im prompt :require-match nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-command-name (command input-method arg prompt)
    (declare (ignore command arg))
    (cl-interactive:completing-read input-method prompt
                                    :completions cl-interactive:*default-command-database*)))

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
If these symbols appear in the argument list, those args will
be given the described values.

BODY is the set of valid options for defgeneric.

See the documentation for cl-interactive:define-command for more details.
"
  `(cl-interactive:define-command ,name ,args
     ,@impl))

(defun execute-command (function key-sequence seat)
  ;; If there are no interactive arguments,
  ;; we could just execute the command directly, but
  ;; doing it in another thread allows for interactive
  ;; error handling and keeps the behavior consistent between
  ;; commands with and without interactive arguments.
  (bt2:make-thread
   (lambda ()
     (cl-interactive:with-gathered-args
           ((sequence key-sequence)
            (seat seat))
         gathered
       (multiple-value-bind (func arg-list)
           (cl-interactive:gather-args-interactively
            function
            :already-gathered gathered)
         (when func
           (hrt:run-in-main-thread
            (lambda ()
              (log-string :debug "Calling command ~S with args:~%~4T~S"
                          func arg-list)
              (hrt:with-view-transaction ()
                (handler-case
                    (cl-interactive:call-command-with-argument-list func arg-list)
                  (invalid-operation (condition)
                    (toast-message *compositor-state* (condition-text condition)
                                   :theme *message-error-theme*))))))))))))

(defcommand colon (sequence
                   seat
                   (cmd-line (:function read-command-name ": ")))
  (:method (sequence seat (exec string))
    (let ((cmd (cl-interactive:find-command cl-interactive:*default-command-database*
                                            exec)))
      (if cmd
          (execute-command cmd sequence seat)
          (toast-message *compositor-state*
                         (format nil "Command not found: ~A" exec)
                         :theme *message-error-theme*)))))
