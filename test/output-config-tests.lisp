(fiasco:define-test-package #:mahogany-tests/output-config
  (:local-nicknames (#:mh #:mahogany)
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
  `(let ((mh::*output-configurations* (make-hash-table :test 'equalp)))
     ,@body))

(defmacro define-find-output-config-test (&rest properties)
  (let* ((test-name (format nil "FIND-OUTPUT-CONFIG-MATCHES-~{~A~^-~}"
                           properties))
         (output-properties (%output-properties-from-keys properties)))
    `(fiasco:deftest ,(intern test-name) ()
       (with-mock-configurations
         (with-output-properties ((output ,@output-properties))
           (let* ((config (mh::define-output-config "test"
                            ,output-properties))
                  (result (mh::find-output-config output)))
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
           (mh::define-output-config "other"
             ,other-properties)
           (let* ((make-config (mh::define-output-config "priority"
                                 ,priority-properties))
                  (result (mh::find-output-config output)))
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
