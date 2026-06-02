(in-package #:mahogany)

(defstruct output-match-data
  (name nil :type (or null string) :read-only t)
  (make nil :type (or null string) :read-only t)
  (model nil :type (or null string) :read-only t)
  (serial nil :type (or null string) :read-only t)
  (config nil :type hrt:output-config :read-only t))

(defstruct (output-layout-config (:constructor %make-output-layout-config (name outputs)))
  (name nil :type string :read-only t)
  (outputs nil :type list :read-only t))

(defun %output-config-from-clauses (clauses)
  (let ((found (make-hash-table))
        scale refresh-rate custom-mode dimensions position)
    (dolist (c clauses)
      (when (not (listp c))
        (error (format nil "output config property spec must be a list, not ~S"
                       c)))
      (let ((present (gethash (first c) found)))
        (when present
          (error (format nil "duplicate property ~S in output config property list"
                         (first c))))
        (setf (gethash (first c) found) c)))
    (macrolet ((with-clause (key lambda-list &body body)
                 (let ((spec (gensym "spec")))
                   `(alexandria:when-let ((,spec (gethash ,key found)))
                      (destructuring-bind ,lambda-list (cdr ,spec)
                        ,@body)))))
      (with-clause :scale (s)
        (setf scale s))
      (with-clause :position (x y)
        (setf position (cons x y)))
      (with-clause :mode (width height &key refresh custom)
        (unless (and width height)
          (error "Both width and height need to be specified when setting a mode"))
        (setf custom-mode custom
              dimensions (cons width height)
              refresh-rate refresh)))
    (hrt:make-output-config :scale scale
                            :refresh-rate refresh-rate
                            :custom-mode custom-mode
                            :dimensions dimensions
                            :position position)))

(defun %output-match-data-from-clause (clause config)
  (etypecase clause
    (list
     (apply #'make-output-match-data
            (nconc (list :config config) clause)))
    (string
     (make-output-match-data :name clause :config config))))

(defun build-output-match-data (o)
  (let* ((config (%output-config-from-clauses (cdr o)))
         (match-data (%output-match-data-from-clause (first o) config)))
    match-data))

(defun make-output-layout-config (name outputs)
  (let ((settings nil))
    (dolist (o outputs)
      (let ((match-data (build-output-match-data o)))
        (push match-data settings)))
    (%make-output-layout-config name settings)))

(defvar *output-configurations* (make-hash-table)
  "Name output configurations that define how a single output should be configured.")

(defmacro define-output-config (name &body config)
  (let ((name-symb (gensym "name")))
    `(let ((,name-symb ,name))
       (setf (gethash ,name-symb *output-configurations*)
             (build-output-match-data (quote ,config))))))

(defvar *output-layout-configurations* (make-hash-table)
  "Named output layout configurations that define how a set of outputs
should be configured and laid out.")

(defmacro define-output-layout (name &body outputs)
  (let ((name-symb (gensym "name")))
    `(let* ((,name-symb ,name))
       (setf (gethash ,name-symb *output-layout-configurations*)
             (make-output-layout-config ,name-symb (quote ,outputs))))))

(defun output-match-data-matches-p (output match-data)
  (declare (type hrt:output output)
           (type output-match-data match-data))
  (with-accessors ((name output-match-data-name)
                   (make output-match-data-make)
                   (model output-match-data-model)
                   (serial output-match-data-serial))
      match-data
    (macrolet ((present-compare (match-accessor val)
                 `(if ,match-accessor
                      (string= ,match-accessor ,val)
                      t)))
      (and
       (present-compare name (hrt:output-name output))
       (present-compare make (hrt:output-make output))
       (present-compare model (hrt:output-model output))
       (present-compare serial (hrt:output-serial output))))))

(defun find-output-config (output)
  "Find an individual output configuration that matches the given output."
  (loop :for c :being :the :hash-value :of *output-configurations*
        :when (output-match-data-matches-p output c)
          :return c))
