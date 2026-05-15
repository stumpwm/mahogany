(defpackage #:config-system
  (:use :cl #:alexandria)
  (:export config-info
           config-info-name
           config-info-type
           config-info-default
           config-info-doc
           config-info-value
           config-value
           describe-all-config-info
           describe-config-info
           describe-config
           config-error
           config-not-found-error
           invalid-datum-error
           define-config-enum
           defconfig
           define-setf-config
           all-config-info
           get-config-info
           set-config
           set-config-atomic
           reset-config
           with-atomic-update))

(in-package #:config-system)

(defparameter *config-vars* (make-hash-table)
  "Map that holds all of the configuration variables for the system")

(defstruct config-info
  (name t :type symbol :read-only t)
  (type nil :read-only t) ; this is a type specifier
  (default nil :read-only t)
  (doc "" :type string :read-only t)
  (set-function nil :type function :read-only t)
  (get-function nil :type function :read-only t))

(declaim (inline config-info-value))
(defun config-info-value (config-info)
  (declare (type config-info config-info))
  (funcall (config-info-get-function config-info)))

(defun %full-symbol-string (symb)
  "Get the full name of the symbol complete with the package name"
  (declare (type symbol symb))
  (let* ((pkg (symbol-package symb))
         (pkg-name (package-name pkg))
         (symb-name (symbol-name symb)))
    (concatenate 'string pkg-name
                 (multiple-value-bind (symb status) (find-symbol symb-name pkg-name)
                   (declare (ignore symb))
                   (if (eql status :internal)
                       "::"
                       ":"))
                 symb-name)))

(defun describe-config-info (info &optional (stream *standard-output*))
  (declare (type config-info info))
  (format stream "Setting Name: ~A~%  Documentation:~%    ~A~%  Default value: ~S~%  Current value: ~S~%"
          (%full-symbol-string (config-info-name info))
          (config-info-doc info)
          (config-info-default info)
          (config-info-value info))
  (alexandria:when-let ((type-specifier (config-info-type info)))
    (format stream "  Type designator: ~S~%" type-specifier)))

(defun describe-config (config-name &optional (stream *standard-output*))
  "Print information about the configuration CONFIG-NAME to the given stream"
  (if-let ((config-info (config-system:get-config-info config-name)))
    (progn
      (config-system:describe-config-info config-info)
      t)
    (progn
      (format stream "No config with symbol ~S" config-name)
      nil)))

(defun %make-match-readtable (str)
  "Try to make the given string have the correct case for a symbol"
  (declare (type string str))
  (ccase (readtable-case *readtable*)
    ;; we are missing :invert, but I don't feel like implementing that.
    (:upcase (string-upcase str))
    (:downcase (string-downcase str))
    (:preserve str)))

(defun %map-config-matching (name-matches package-matches function)
  (let ((name-scanner (ppcre:create-scanner (%make-match-readtable name-matches)))
        (pkg-scanner (ppcre:create-scanner (%make-match-readtable package-matches))))
    (maphash (lambda (key value)
               (declare (ignorable key))
               (let ((pkg-name (package-name (symbol-package (config-info-name value))))
                     (symb-name (symbol-name (config-info-name value))))
                 (when (and (funcall name-scanner symb-name 0 (length symb-name))
                            (funcall pkg-scanner pkg-name 0 (length pkg-name)))
                   (funcall function value))))
             *config-vars*)))

(defun describe-all-config-info (&key (stream *standard-output*) (name-matches ".*") (package-matches ".*"))
  (%map-config-matching name-matches package-matches
                        (lambda (info)
                          (describe-config-info info stream)
                          (format stream "~%"))))

(define-condition config-error (error) ())

(define-condition invalid-datum-error (config-error)
  ((place-symbol :initarg :place :initform nil
                 :accessor invalid-datum-error-place
                 :type symbol)
   (value :initarg :value :initform nil
          :accessor invalid-datum-error-value))
  (:report
   (lambda (c s)
     (with-slots (place-symbol value) c
       (format s "The value ~S is invalid for variable ~S." value place-symbol)))))

(define-condition config-not-found-error (config-error)
  ((place-symbol :initarg :place :initform nil
                 :accessor config-not-found-error-place
                 :type symbol)
   (alternatives :initarg :alternatives :initform nil
                 :accessor config-not-found-alternatives
                 :type list))
  (:report
   (lambda (c s)
     (with-slots (place-symbol alternatives) c
       (if alternatives
           (format s  "Setting ~A not found. Did you mean one of these? ~A"
                   (%full-symbol-string place-symbol) alternatives)
           (format s  "Setting ~A not found." (%full-symbol-string place-symbol)))))))

;; TODO: Instead of overwriting the config object, updating it if already exists
;;  could lead to better warnings and optimizations, as we could guarantee that a
;;  symbol is always associated with the same config object.
(defun %add-config-info (name default-value type-specifier documentation setter getter)
  (setf (gethash name *config-vars*)
        (make-config-info :name name :default default-value
                          :type type-specifier :doc documentation
                          :set-function setter
                          :get-function getter)))

(defmacro defconfig (name default type-specifier documentation)
  "Create and register a configurable variable with the given default value,
type specifier, and documentation.

Example:
   (config-system:defconfig config-var :default symbol \"Documentation\")"
  (check-type documentation string)
  (check-type name symbol)
  (with-gensyms (setf-arg)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (declaim (type ,type-specifier ,name))
         (defvar ,name ,default ,@(when documentation
                                    (list documentation))))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (if (typep ,name (quote ,type-specifier))
             (%add-config-info (quote ,name) ,name (quote ,type-specifier) ,documentation
                               (lambda (,setf-arg)
                                 (if (typep ,setf-arg (quote ,type-specifier))
                                     (setf ,name ,setf-arg)
                                     (error 'invalid-datum-error :place (quote ,name)
                                                                 :value ,setf-arg)))
                               (lambda ()
                                 ,name))
             (error 'invalid-datum-error :place (quote ,name) :value ,name))))))

(defun %check-define-setf-config-clauses (getter setter)
  (let ((missing nil))
    (when (not getter)
      (push :getter missing))
    (when (not setter)
      (push :setter missing))
    (when missing
      (error (format nil "Missing section~P ~{~S~^, ~} in ~S"
                     (length missing) missing 'define-setf-config)))))

(defconstant +no-default+ 'no-default)

(defmacro define-setf-config ((name &key type (default (quote +no-default+))) documentation &body body)
  "Define a configuration object whose values are modified by a setf function
and retreived using a getter function.

Example:
  (define-setf-config
      (focus-theme :type symbol)
      \"documentation\"
    (:setter (test)
      (setf focus-theme-var test))
    (:getter ()
      focus-theme-var))"
  (let ((setter (assoc :setter body))
        (getter (assoc :getter body))
        (forms nil))
    (%check-define-setf-config-clauses getter setter)
    (destructuring-bind (setter-symb (parameter)
                         &body body)
        setter
      (declare (ignore setter-symb))
      (push `(defun (setf ,name) (,parameter)
               ,(if type
                  `(if (typep ,parameter (quote ,type))
                        (progn
                          ,@body)
                        (error 'invalid-datum-error :place (quote ,name) :value ,parameter))
                  body))
            forms))
    (destructuring-bind (setter-symb ()
                     &body body)
        getter
      (declare (ignore setter-symb))
      (push `(defun ,name ()
               ,@body)
            forms))
    (when type
      (push `(declaim (ftype (function () (values ,type)) ,name))
            forms))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@forms
       (%add-config-info (quote ,name) ,default t ,documentation
                  (function (setf ,name))
                  (function ,name)))))


(defun all-config-info (&key (name-matches ".*") (package-matches ".*"))
  "List all of the available customizable settings matching the given criteria."
  (let ((accumulate (list)))
    (%map-config-matching name-matches package-matches
                          (lambda (info)
                            (push info accumulate)))
    accumulate))

(defun %find-possible-settings (setting-name table)
  "Find the settings that have the same symbol name of SETTING-NAME but are in a different package"
  (declare (type hash-table table)
           (type symbol setting-name))
  (let ((name (symbol-name setting-name)))
    ;; for some reason, the package isn't always included with the symbol name even
    ;; if the top level isn't in the symbol's package, so we need to manually get the symbol name.
    (mapcar #'%full-symbol-string
            (remove-if-not (lambda (x) (string-equal (symbol-name x) name))
                           (mapcar #'config-info-name (hash-table-values table))))))

(declaim (ftype (function (symbol)
                          (values (or config-info null) boolean &optional))
                get-config-info))
(defun get-config-info (setting-name)
  "Find the info for the config variable stored in the symbol SETTING-NAME."
  ;; (declare (type symbol setting-name))
  (gethash setting-name *config-vars*))

(defmacro with-config-info ((var) setting-name &body body)
  `(let ((,var (get-config-info ,setting-name)))
     (if ,var
         (progn ,@body)
         (error 'config-not-found-error
                :place ,setting-name
                :alternatives
                (%find-possible-settings ,setting-name *config-vars*)))))

(defun config-value (setting-name)
  (declare (type symbol setting-name))
  (with-config-info (info) setting-name
    (config-info-value info)))

(defun (setf config-value) (setting value)
  (declare (type symbol setting))
  (with-config-info (info) setting
    (funcall (config-info-set-function info) value)))

(defmacro %set-config (setting-name value)
  (with-gensyms (actual-value info)
    `(let ((,actual-value ,value))
       (if-let ((,info (get-config-info (quote ,setting-name))))
         (funcall (config-info-set-function ,info) ,actual-value)
         (error 'config-not-found-error
                :place (quote ,setting-name)
                :alternatives (%find-possible-settings (quote ,setting-name) *config-vars*))))))

(defmacro set-config (&rest settings)
  "Set the given configuration variables to the given values. Used like setf"
  (assert (= (mod (length settings) 2) 0))
  (let ((accumulate (list 'progn)))
    (do ((cur settings (cddr cur)))
        ((not cur))
      (push (list '%set-config (first cur) (second cur)) accumulate))
    (nreverse accumulate)))

(defmacro reset-config (&rest settings)
  "Reset the list of settings to their default values"
  (let ((accumulate (list))
        (let-var (gensym)))
    (dolist (setting-name settings)
      (check-type setting-name symbol)
      (push `(let ((,let-var (get-config-info (quote ,setting-name))))
               (funcall (config-info-set-function ,let-var) (config-info-default ,let-var)))
            accumulate))
    `(progn
       ,@(nreverse accumulate))))

(defun %make-storage-pair (symbol)
  (declare (type symbol symbol))
  (cons (gensym (symbol-name symbol)) symbol))

(defmacro with-atomic-update (settings &body body)
  "If an error occurs during the execution of BODY, reset the provided variables to their original value"
  (let ((setting-pairs (mapcar #'%make-storage-pair settings)))
    `(let ,(let ((settings-list (list)))
             (dolist (item setting-pairs (nreverse settings-list))
               (push (list (car item) (cdr item)) settings-list)))
       (handler-case
           (progn
             ,@body)
         (warning (w)
           (warn w))
         (error (condition)
           (setf ,@(loop for pair in setting-pairs
                         append (list (cdr pair) (car pair))))
           (error condition))
         (t (c)
           (signal c))))))

(defmacro set-config-atomic (&rest settings)
  "Set the listed settings to the provided values. If an error signal is raised during execution,
all of the settings are set back to their original value"
  (assert (= (mod (length settings) 2) 0))
  (let ((setting-vars (do ((cur settings (cddr cur))
                           (vars (list)))
                          ((not cur) (nreverse vars))
                        (push (first cur) vars))))
    `(with-atomic-update ,setting-vars
       (set-config ,@settings))))
