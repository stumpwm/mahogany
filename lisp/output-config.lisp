(in-package #:mahogany/output-config)

(defstruct output-match-data
  (name nil :type (or null string) :read-only t)
  (make nil :type (or null string) :read-only t)
  (model nil :type (or null string) :read-only t)
  (serial nil :type (or null string) :read-only t)
  (config nil :type hrt:output-config :read-only t))

(defstruct (output-layout-config
            (:constructor %make-output-layout-config
                (name priority outputs exact)))
  (name nil :type string :read-only t)
  (priority 0 :type fixnum :read-only t)
  (exact nil :type boolean :read-only t)
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

(defun make-output-layout-config (name priority outputs exact)
  (let ((settings nil))
    (dolist (o outputs)
      (let ((match-data (build-output-match-data o)))
        (push match-data settings)))
    ;; Sort the configs now so that we don't need to
    ;; repeatedly do it when matching configurations:
    (setf settings (sort settings #'%compare-specificity))
    (%make-output-layout-config name priority settings exact)))

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
    (multiple-value-bind (name-val priority-val exact)
        (if (listp name-or-options)
            (destructuring-bind (name-val &key (priority 0) (exact t))
                name-or-options
              (values name-val priority exact))
            (values name-or-options 0 t))
      `(let ((,name-symb ,name-val)
             (,priority-symb ,priority-val))
         (setf (gethash ,name-symb *output-layout-configurations*)
               (make-output-layout-config
                ,name-symb
                ,priority-symb
                (quote ,outputs)
                ,exact))))))

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
        (when (< 0 score)
          (values matching score))))))

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

(defun %score-layout-configuration (outputs config)
  "Return a list of %config-match objects that represents how well the
outputs match with the given configuration. If the config does not match,
return nil."
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
           (return-from %score-layout-configuration nil)))))
    (if (and found (output-layout-config-exact config))
        (if (= (length found)
               (length outputs))
            found
            nil)
        found)))

(defun %populate-config-map (outputs output-scores)
  "Create a map of outputs to their combined configuration
using a sequence of %config-match objects."
  (declare (type list outputs)
           (type sequence output-scores))
  (let ((configurations (make-hash-table :test 'equalp)))
    (dolist (o outputs)
      (let ((base (find-output-config o))
            (from-layout (alexandria:when-let
                             ((l (find o output-scores
                                       :key '%config-match-output)))
                           (output-match-data-config (%config-match-config l)))))
        (cond
          ((and base from-layout)
           (setf (gethash o configurations)
                 (hrt:output-config-merge
                  (output-match-data-config base) from-layout)))
          (base
           (setf (gethash o configurations)
                 (output-match-data-config base)))
          (from-layout
           (setf (gethash o configurations)
                 from-layout))
          (t
           (setf (gethash o configurations)
                 nil)))))
    configurations))

(declaim (ftype (function (output-layout-config sequence)
                          (or null hash-table))
                get-configuration-map))
(defun get-configuration-map (config outputs)
  "Take the given output layout config, apply the default configurations,
and bundle them into a table mapping outputs to their final config."
  (let ((output-scores (%score-layout-configuration outputs config)))
    (if output-scores
        (%populate-config-map outputs output-scores))))

(defstruct (%output-score-pair
            (:constructor make-%output-score-pair (outputs config)))
  (outputs nil :type list)
  (config nil :type output-layout-config))

(defun %filter-layout-matches-length (matches)
  (let ((max-length (apply #'max
                           (mapcar (lambda (x)
                                     (length
                                      (%output-score-pair-outputs x)))
                                   matches))))
    (remove-if-not (lambda (x)
                     (= (length (%output-score-pair-outputs x))
                        max-length))
                   matches)))

(defun %filter-layout-matches-priority (matches)
  (let ((priority-vals (list))
        (priority most-negative-fixnum))
    (dolist (match matches)
      (declare (type %output-score-pair match))
      (let ((cur-priority (output-layout-config-priority
                           (%output-score-pair-config match))))
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

(defun %find-valid-output-layouts (outputs)
  (loop :for config :being
          :the :hash-value :of *output-layout-configurations*
        :nconcing (let ((scores (%score-layout-configuration
                                 outputs config)))
                    (if scores
                        (list (make-%output-score-pair scores config))
                        nil))))

(defun find-valid-output-layouts (outputs)
  (let ((valid (%find-valid-output-layouts outputs)))
    (map 'list #'%output-score-pair-config valid)))

(declaim (ftype (function (t) (or null %output-score-pair))
                find-output-layout-config))
(defun find-output-layout-config (outputs)
  "Find the output config for the give outputs, ignoring their default
configurations"
  (let* ((matching (%find-valid-output-layouts outputs))
         (num-matching (length matching)))
    (when (= num-matching 0)
      (return-from find-output-layout-config nil))
    (when (= num-matching 1)
      (return-from find-output-layout-config (car matching)))
    ;; filter out the matches that match fewer outputs:
    (let ((remaining (%filter-layout-matches-length matching)))
      (when (= (length remaining) 1)
        (return-from find-output-layout-config (car remaining)))
      ;; Now look at the configuration's priority:
      (let ((priority-vals (%filter-layout-matches-priority remaining)))
        (when (= (length priority-vals) 1)
          (return-from find-output-layout-config (car priority-vals)))
        ;; Finally, take the sum of the scores and use that:
        (let* ((scores (mapcar (lambda (x)
                                 (cons
                                  (reduce (lambda (total y)
                                            (+ total (%config-match-score y)))
                                          (%output-score-pair-outputs x)
                                          :initial-value 0)
                                  x))
                               priority-vals))
               (max-score (maximum scores #'> #'car)))
          (cdr max-score))))))

(defun find-output-configurations (outputs)
  "Match the given outputs with their final configurations."
  (let* ((layout (find-output-layout-config outputs))
         (layout-outputs (if layout (%output-score-pair-outputs layout)))
         (configurations (%populate-config-map outputs layout-outputs)))
    (values configurations (when layout (%output-score-pair-config layout)))))
