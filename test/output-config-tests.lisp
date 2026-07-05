(fiasco:define-test-package #:mahogany-tests/output-config
  (:local-nicknames (#:mh/output #:mahogany/output-config)
                    (#:alex #:alexandria))
  (:use #:mahogany/wm-interface
        #:mahogany/test/util))

(in-package #:mahogany-tests/output-config)

(eval-when (:compile-toplevel :load-toplevel)
  (defun %build-cond-form (specs accessor key)
    (let ((cond-cases (mapcan (lambda (x)
                                (alex:when-let ((val (getf (cdr x) key)))
                                  `(((eq ,(car x) output)
                                     ,val))))
                              specs)))
      `(,accessor (output)
                  ,(if cond-cases
                       `(cond
                          ,@cond-cases)
                       `(declare (ignore output))))))
  (defun %output-properties-from-keys (keys)
    (mapcan (lambda (x)
              (list x (symbol-name x)))
            keys)))

(defmacro with-output-properties (specs &body body)
  (let ((vars (mapcar #'first specs))
        (name-form (%build-cond-form specs 'hrt:output-name :name))
        (make-form (%build-cond-form specs 'hrt:output-make :make))
        (model-form (%build-cond-form specs 'hrt:output-model :model))
        (serial-form (%build-cond-form specs 'hrt:output-serial :serial)))
    `(let (,@(mapcar (lambda (x) `(,x (make-mock-output
                                       :full-name ,(symbol-name x))))
                     vars))
       (cl-mock:dflet (,name-form
                       ,make-form
                       ,model-form
                       ,serial-form)
         ,@body))))

(defmacro with-mock-configurations (&body body)
  `(let ((mh/output::*output-configurations* (make-hash-table :test 'equalp)))
     ,@body))

(defmacro define-find-output-config-test (&rest properties)
  (let* ((test-name (format nil "FIND-OUTPUT-CONFIG-MATCHES-~{~A~^-~}"
                           properties))
         (output-properties (%output-properties-from-keys properties)))
    `(fiasco:deftest ,(intern test-name) ()
       (with-mock-configurations
         (with-output-properties ((output ,@output-properties))
           (let* ((config (mh/output::define-output-config "test"
                            ,output-properties))
                  (result (mh/output::find-output-config output)))
             (is (eq result config))))))))

(define-find-output-config-test :name)
(define-find-output-config-test :make)
(define-find-output-config-test :model)
(define-find-output-config-test :serial)
(define-find-output-config-test :name :make :model :serial)
(define-find-output-config-test :name :make)

(defmacro define-find-output-config-priority-test (&key output priority other)
  (let ((test-name (format nil
                           "FIND-OUTPUT-CONFIG-PRIORITIZES-~{~A~^-~}-OVER-~{~A~^-~}"
                           priority other))
        (output-properties (%output-properties-from-keys output))
        (priority-properties (%output-properties-from-keys priority))
        (other-properties (%output-properties-from-keys other)))
    `(fiasco:deftest ,(intern test-name) ()
       (with-mock-configurations
         (with-output-properties ((output ,@output-properties))
           (mh/output::define-output-config "other"
             ,other-properties)
           (let* ((make-config (mh/output::define-output-config "priority"
                                 ,priority-properties))
                  (result (mh/output::find-output-config output)))
             (is (eq result make-config))))))))

(define-find-output-config-priority-test
	:output (:name :make)
    :priority (:make)
    :other (:model))

(define-find-output-config-priority-test
	:output (:name :make :model :serial)
    :priority (:model)
    :other (:name))

(define-find-output-config-priority-test
	:output (:name :make :model :serial)
    :priority (:serial)
    :other (:name :make :model))

(defmacro with-mock-layout (&body body)
  `(let ((mh/output::*output-layout-configurations* (make-hash-table)))
    ,@body))

(defmacro define-layout-test (name args &body body)
  `(fiasco:deftest ,name ,args
     (with-mock-layout
       ,@body)))

(define-layout-test define-output-layout-sets-priority ()
  (let ((layout (mh/output:define-output-layout ("name" 5)
				  ("output"))))
    (is (= (mh/output::output-layout-config-priority layout) 5))))

(define-layout-test define-output-layout-sets-priority-when-not-specified ()
  (let ((layout (mh/output:define-output-layout "name"
				  ("output"))))
    (is (= (mh/output::output-layout-config-priority layout) 0))))

(define-layout-test score-layout-configuration-no-match ()
  (mh/output:define-output-layout "name"
    ((:name "output")))
  (with-output-properties ((output :name "asdf"))
    (let ((result (mh/output::find-output-layout-config (list output))))
      (is (null result)))))

(defun check-match-data (entry &key output matched scored)
  (unless (and output matched scored)
    (error "All keys to check-match-data need to be supplied"))
  (with-accessors ((found-output mh/output::%config-match-output)
                   (match mh/output::%config-match-config)
                   (score mh/output::%config-match-score))
      entry
    (is (= scored score))
    (is (eq found-output output))
    (is (eq matched match))))

(define-layout-test find-output-layout-config-scores-output ()
  (let ((layout (mh/output:define-output-layout "name"
				  ("output"))))
    (with-output-properties ((output :name "output"))
	  (let* ((result (mh/output::find-output-layout-config
                     (list output)))
            (entry (car result)))
        (is entry)
        (check-match-data
         entry
         :output output
         :matched (first (mh/output::output-layout-config-outputs layout))
         :scored 1)))))

(define-layout-test find-output-layout-config-scores-output-2 ()
  (let* ((layout (mh/output:define-output-layout "name"
				   ((:make "HP"))
				   ((:name "output" :make "HP")
				    (:position 10 10)))))
    (with-output-properties ((output
							  :make "HP"
							  :name "output")
						     (o2 :make "HP"))
      (let ((result (mh/output::find-output-layout-config
	                 (list output o2))))
        (is result)
        (is (= (length result) 2))
        (check-match-data
         (first result)
         :output o2
         :matched (second (mh/output::output-layout-config-outputs layout))
         :scored 2)
        (check-match-data
         (second result)
         :output output
         :matched (first (mh/output::output-layout-config-outputs layout))
         :scored 3)))))

(define-layout-test find-output-layout-config-maximizes-matches ()
  (let* ((layout (mh/output:define-output-layout "name"
				   ((:make "HP"))
				   ((:name "output" :make "HP")
				    (:position 10 10)))))
    ;; Make the single one also valid:
    (mh/output:define-output-layout "single"
      ((:name "output" :make "HP")
       (:position 20 20)))
    (with-output-properties ((output
							  :make "HP"
							  :name "output")
						     (o2 :make "HP"))
      (let ((result (mh/output::find-output-layout-config
	                 (list output o2))))
        (is result)
        (is (= (length result) 2))
        (check-match-data
         (first result)
         :output o2
         :matched (second (mh/output::output-layout-config-outputs layout))
         :scored 2)
        (check-match-data
         (second result)
         :output output
         :matched (first (mh/output::output-layout-config-outputs layout))
         :scored 3)))))

(define-layout-test find-output-layout-config-total-score ()
  ;; This one matches, has the same number of outputs,
  ;; same priority, but a higher total score:
  (mh/output:define-output-layout ("single" 10)
    ((:name "output" :make "HP")
     (:position 20 20)))
  (let* ((layout (mh/output:define-output-layout ("name" 10)
				   ((:name "output" :make "HP" :model "model")
				    (:position 10 10)))))
    (with-output-properties ((output
							  :make "HP"
							  :name "output")
						     (o2 :make "HP"))
      (let ((result (mh/output::find-output-layout-config
	                 (make-array 2 :initial-contents (list output o2)))))
        (is result)
        (is (= (length result) 1))
        (check-match-data
         (first result)
         :output output
         :matched (first (mh/output::output-layout-config-outputs layout))
         :scored 3)))))

(define-layout-test find-output-configurations-no-layouts ()
  (with-output-properties ((output :name "output"))
	(let* ((result (mh/output:find-output-configurations
					(list output))))
	  (is (= (hash-table-count result) 0)))))
