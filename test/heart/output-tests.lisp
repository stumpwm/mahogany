(fiasco:define-test-package #:mahogany-tests/heart/output
  (:local-nicknames (#:alex #:alexandria))
  (:use #:mahogany/wm-interface
        #:mahogany/test/util))

(in-package #:mahogany-tests/heart/output)

(defmacro define-output-config-merge-test (name default override expected)
  (let ((def-symb (gensym "default"))
        (override-symb (gensym "override"))
        (expected-symb (gensym "expected")))
    `(fiasco:deftest ,name ()
       (let ((,def-symb (hrt:make-output-config ,@default))
             (,override-symb (hrt:make-output-config ,@override))
             (,expected-symb (hrt:make-output-config ,@expected)))
         (let ((result (hrt:output-config-merge ,def-symb ,override-symb)))
           (is (hrt::output-config= result ,expected-symb)))))))

(define-output-config-merge-test output-config-merge-handles-mode
	(:scale 2
     :refresh-rate 60
     :custom-mode t
     :dimensions (cons 600 600)
     :position (cons 20 20))
  (:scale 1
   :dimensions (cons 1080 960)
   :refresh-rate 90)
  (:scale 1
   :dimensions (cons 1080 960)
   :refresh-rate 90
   :custom-mode nil
   :position (cons 20 20)))

(define-output-config-merge-test output-config-merge-basic
    (:scale 1)
  (:scale 2)
  (:scale 2))
