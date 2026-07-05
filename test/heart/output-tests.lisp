(fiasco:define-test-package #:mahogany-tests/heart/output
  (:local-nicknames (#:alex #:alexandria))
  (:use #:mahogany/wm-interface
        #:mahogany/test/util))

(in-package #:mahogany-tests/heart/output)

(fiasco:deftest output-config-merge-handles-mode ()
  (let* ((base (hrt:make-output-config
                :scale 2
                :refresh-rate 60
                :custom-mode t
                :dimensions (cons 600 600)
                :position (cons 20 20)))
         (override (hrt:make-output-config
                    :dimensions (cons 1080 960)
                    :refresh-rate 90))
         (merged (hrt:output-config-merge base override)))
    (is (equal (hrt::output-config-dimensions merged)
               (cons 1080 960)))
    (is (equal  (hrt::output-config-refresh-rate merged)
                90))
    (is (eq (hrt::output-config-custom-mode merged)
            nil))))
