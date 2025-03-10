(fiasco:define-test-package #:config-system-tests
	(:use #:config-system #:alexandria))

(in-package #:config-system-tests)

(defmacro defconfig-test (name lambda-list before &body body)
  (multiple-value-bind (forms declarations docstring) (parse-body body :documentation t)
    `(let* ((config-system::*config-vars* (make-hash-table))
            (table config-system::*config-vars*))
       ,@before
       (fiasco:deftest ,name ,lambda-list
          ,@(when docstring
              (list docstring))
          ,@declarations
          (let ((config-system::*config-vars* table))
            ,@forms)))))

(defvar *foo* "test variable")
(defvar *bar* "test variable")

(defconfig-test defconfig-var-stores-info ()
  ((let ((default "1")
         (validator #'stringp))
     (defconfig *foo* default validator "documentation")))
  (let ((info (gethash '*foo* config-system::*config-vars*))
        (default "1")
        (validator #'stringp))
    (is info)
    (is (config-info-default info) default)
    (is (config-info-name info) '*foo*)
    (is (config-info-doc info) "documentation")
    (is (config-info-validator info) validator)))

(defconfig-test get-config-info-finds-info ()
    ((let ((default "1")
           (validator #'stringp))
       (defconfig *foo* default validator "documentation")))
  (let ((info (get-config-info '*foo*))
        (default "1")
        (validator #'stringp))
    (is info)
    (is (config-info-default info) default)
    (is (config-info-name info) '*foo*)
    (is (config-info-doc info) "documentation")
    (is (config-info-validator info) validator)))

(defconfig-test all-config-info-shows-all ()
    ((let ((validator #'stringp))
       (defconfig *foo* "11" validator "documentation")
       (defconfig *bar* "21" validator "documentation")))
  (let ((configs (all-config-info)))
    (is (length configs) 2)
    (is (member (get-config-info '*foo*) configs :test #'eql))
    (is (member (get-config-info '*bar*) configs :test #'eql))))

(defconfig-test all-config-info-gets-correct-values ()
    ((let ((validator #'stringp))
       (defconfig *foo* "11" validator "documentation")
       (defconfig *bar* "21" validator "documentation")))
  (let ((configs (all-config-info)))
    (is (length configs) 2)
    (let ((foo (find (get-config-info '*foo*) configs :test #'eql))
          (bar (find (get-config-info '*bar*) configs :test #'eql)))
      (is foo)
      (is bar)
      (is (config-info-value foo) "11")
      (is (config-info-value bar) "21"))))

(defconfig-test set-config-throws-on-not-found ()
    ((let ((validator #'stringp))
       (defconfig *foo* "11" validator "documentation")))
	(signals config-not-found-error (set-config *bar* 11)))

(defconfig-test set-config-throws-on-invalid ()
    ((let ((validator #'stringp))
       (defconfig *foo* "11" validator "documentation")))
  (signals invalid-datum-error (set-config *foo* 111)))

(defconfig-test set-config-checks-arg-length ()
    ()
  (signals error (macroexpand '(set-config *foo* 111 *bar*))))

(defconfig-test set-config-sets-valid ()
    ((let ((validator #'stringp))
       (defconfig *foo* "11" validator "documentation")))
  (set-config *foo* "12")
  (is *foo* "12"))

(defconfig-test reset-config-works ()
    ((defconfig *foo* "11" #'stringp "documentation")
     (defconfig *bar* "22" #'stringp "documentation"))
  (set-config *foo* "asdf"
                     *bar* "qwerty")
  (reset-config *foo* *bar*)
  (is *foo* "11")
  (is *bar* "22"))

(defconfig-test with-atomic-update-restores-values ()
    ((let ((validator #'stringp))
       (defconfig *foo* "qwerty" validator "documentation")
       (defconfig *bar* "asdf" validator "documentation")))
  (ignore-errors
    (with-atomic-update (*foo* *bar*)
      (set-config *foo* "set")
      (set-config *bar* 11)))
  (is *foo* "qwerty")
  (is *bar* "asdf"))

(defconfig-test set-config-atomic-restores-values ()
    ((let ((validator #'stringp))
       (defconfig *foo* "qwerty" validator "documentation")
       (defconfig *bar* "asdf" validator "documentation")))
  (ignore-errors
    (set-config-atomic *foo* "set"
                              *bar* 11))
  (is *foo* "qwerty")
  (is *bar* "asdf"))
