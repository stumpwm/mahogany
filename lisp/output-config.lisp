(in-package #:mahogany/output-config)

(defstruct output-match-data
  (name nil :type (or null string) :read-only t)
  (make nil :type (or null string) :read-only t)
  (model nil :type (or null string) :read-only t)
  (serial nil :type (or null string) :read-only t)
  (config nil :type hrt:output-config :read-only t))

(defstruct (output-layout-config
            (:constructor %make-output-layout-config
                (name priority outputs)))
  (name nil :type string :read-only t)
  (priority 0 :type fixnum :read-only t)
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

(defun %compare-specificity (a b)
  (declare (type output-match-data a b))
  (flet ((%score-match-data (x)
           (+
            (if (output-match-data-serial x) 8 0)
            (if (output-match-data-model x) 4 0)
            (if (output-match-data-make x) 2 0)
            (if (output-match-data-name x) 1 0))))
    (> (%score-match-data a) (%score-match-data b))))

(defun make-output-layout-config (name priority outputs)
  (let ((settings nil))
    (dolist (o outputs)
      (let ((match-data (build-output-match-data o)))
        (push match-data settings)))
    ;; Sort the configs now so that we don't need to
    ;; repeatedly do it when matching configurations:
    (setf settings (sort settings #'%compare-specificity))
    (%make-output-layout-config name priority settings)))

(defvar *output-configurations* (make-hash-table :test 'equalp)
  "Name output configurations that define how a single output should be configured.")

(defmacro define-output-config (name &body config)
  (let ((name-symb (gensym "name")))
    `(let ((,name-symb ,name))
       (setf (gethash ,name-symb *output-configurations*)
             (build-output-match-data (quote ,config))))))

(defvar *output-layout-configurations* (make-hash-table :test 'equalp)
  "Named output layout configurations that define how a set of outputs
should be configured and laid out.")

(defmacro define-output-layout (name-or-options &body outputs)
  (let* ((name-symb (gensym "name"))
         (priority-symb (gensym "priority")))
    (multiple-value-bind (name-val priority-val)
        (if (listp name-or-options)
            (destructuring-bind (name-val priority-val)
                name-or-options
              (values name-val priority-val))
            (values name-or-options 0))
      `(let ((,name-symb ,name-val)
             (,priority-symb ,priority-val))
         (setf (gethash ,name-symb *output-layout-configurations*)
               (make-output-layout-config
                ,name-symb
                ,priority-symb
                (quote ,outputs)))))))

(defun score-output-match-data-match (output match-data)
  (declare (type hrt:output output)
           (type output-match-data match-data))
  (with-accessors ((name output-match-data-name)
                   (make output-match-data-make)
                   (model output-match-data-model)
                   (serial output-match-data-serial))
      match-data
    (macrolet ((present-compare (match-accessor val score)
                 `(if (and ,match-accessor (string= ,match-accessor ,val))
                      ,score
                      0)))
      (+
       (present-compare name (hrt:output-name output) 1)
       (present-compare make (hrt:output-make output) 2)
       (present-compare model (hrt:output-model output) 4)
       (present-compare serial (hrt:output-serial output) 8)))))

(defun find-max-score (table items score-fn)
  (declare (type hash-table table)
           (type (function (t t) fixnum) score-fn))
  (let ((score 0))
    (with-hash-table-iterator (iter table)
      (multiple-value-bind (more key matching)
          (iter)
        (declare (ignore key))
        (unless more
          (return-from find-max-score))
        (setf score (funcall score-fn items matching))
        (loop
          (multiple-value-bind (more key cur)
              (iter)
            (declare (ignore key))
            (unless more
              (return))
            (let ((cur-score (funcall score-fn items cur)))
              (when (> cur-score score)
                (setf score cur-score
                      matching cur)))))
        (values matching score)))))

(defun find-output-config (output)
  "Find an individual output configuration that matches the given output."
  (find-max-score *output-configurations*
                  output
                  #'score-output-match-data-match))

(defstruct (%config-match
            (:constructor make-%config-match (output config score)))
  (output nil :type hrt:output)
  (config nil :type output-match-data)
  (score 0 :type fixnum))

(defun score-layout-configuration (outputs config)
  (declare (type output-layout-config config)
           ;; Make this code work with both arrays and lists;
           ;; it's an array right now, but that may change:
           (type sequence outputs))
  ;; Correctly matching configurations to their outputs
  ;; probably involves scoring each possible combination, then picking
  ;; the highest scoring output for each configuration.
  ;; Using a greedy algorithm where the most-specific
  ;; configurations are matched first should get us there
  ;; most of the time; during testing, I wasn't able to come up
  ;; with a configuration that this didn't work with. Maybe it
  ;; is the optimial solution?
  (let ((found nil)
        (remaining outputs))
    (dolist (c (output-layout-config-outputs config))
      (unless (> (length remaining) 0)
        (return nil))
      (let* ((cur (elt remaining 0))
             (score (score-output-match-data-match cur c)))
        (map nil (lambda (o)
                   (let ((cur-score (score-output-match-data-match o c)))
                      (when (> cur-score score)
                        (setf score cur-score
                              cur o))))
             (mahogany/util:rest-seq remaining))
        (cond
          ((> score 0)
           (push (make-%config-match cur c score) found)
           (setf remaining (remove cur remaining)))
          (t
           (return-from score-layout-configuration nil)))))
    found))

(defun %filter-layout-matches-length (matches)
  (let ((max-length (apply #'max (mapcar (lambda (x)
                                           (length (cdr x)))
                                         matches))))
    (loop :for v :in matches
          :nconcing (when (= (length (cdr v)) max-length)
                      (list v)))))

(defun %filter-layout-matches-priority (matches)
  (let ((priority-vals (list))
        (priority most-negative-fixnum))
    (dolist (match matches)
      (let ((cur-priority (output-layout-config-priority (car match))))
        (cond
          ((= cur-priority priority)
           (push match priority-vals))
          ((> cur-priority priority)
           (setf priority cur-priority
                 priority-vals (list match))))))
    priority-vals))

(defun maximum (list predicate key)
  (when list
    (let* ((m0 (first list))
           (m1 (funcall key m0)))
      (mapc (lambda (e0 &aux (e1 (funcall key e0)))
              (when (funcall predicate e1 m1)
                (psetf m0 e0 m1 e1)))
            list)
      m0)))

(defun find-output-layout-config (outputs)
  (let* ((matching
           (loop :for v :being :the :hash-value :of *output-layout-configurations*
                 :nconcing (let ((scores (score-layout-configuration outputs v)))
                             (if scores
                                 (list (cons v scores))
                                 nil))))
         (num-matching (length matching)))
    (when (= num-matching 0)
      (return-from find-output-layout-config nil))
    (when (= num-matching 1)
      (return-from find-output-layout-config (cdr (car matching))))
    ;; filter out the matches that match fewer outputs:
    (let ((remaining (%filter-layout-matches-length matching)))
      (when (= (length remaining) 1)
        (return-from find-output-layout-config (cdr (car remaining))))
      ;; Now look at the configuration's priority:
      (let ((priority-vals (%filter-layout-matches-priority remaining)))
        (when (= (length priority-vals) 1)
          (return-from find-output-layout-config (cdr (car priority-vals))))
        ;; Finally, take the sum of the scores and use that:
        (let* ((scores (mapcar (lambda (x)
                                 (cons
                                  (reduce (lambda (total y)
                                            (+ total (%config-match-score y)))
                                          (cdr x)
                                          :initial-value 0)
                                  x))
                               priority-vals))
               (max-score (maximum scores #'< #'car)))
          (cddr max-score))))))

(defun find-output-configurations (outputs)
  (let ((layout (find-output-layout-config outputs))
        (configurations (make-hash-table :test 'equalp)))
    (dolist (o outputs)
      (let ((base (find-output-config o))
            (from-layout (alexandria:when-let
                             ((l (find o layout :key '%config-match-output)))
                           (%config-match-config l))))
        (cond
          ((and base from-layout)
           (setf (gethash o configurations)
                 (hrt:output-config-merge base from-layout)))
          (base
           (setf (gethash o configurations)
                 base))
          (from-layout
           (setf (gethash o configurations)
                 from-layout)))))
    configurations))
