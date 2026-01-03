(fiasco:define-test-package #:mahogany-tests/kmap-modes
  (:use #:mahogany))

(in-package #:mahogany-tests/kmap-modes)

(fiasco:deftest define-kmap-mode-signals-when-name-wrong ()
  (signals simple-error (macroexpand `(mahogany::define-kmap-mode foo)))
  (signals simple-error (macroexpand '(mahogany::define-kmap-mode df)))
  (signals simple-error (macroexpand '(mahogany::define-kmap-mode -mode))))

(fiasco:deftest define-kmap-mode-works-when-name-correct ()
  (is (macroexpand '(mahogany::define-kmap-mode test-mode
                     :prefix-binding *kmap*))))
