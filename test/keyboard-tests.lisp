(fiasco:define-test-package #:mahogany-tests/keyboard
  (:use #:mahogany/keyboard))

(in-package #:mahogany-tests/keyboard)

(defun expand-key-description (code &rest desc)
  (let ((mask 0))
    (declare (type (unsigned-byte 32)))
    (dolist (mod desc)
      (setf mask (logior mod mask)))
    (make-key code mask)))

(defmacro expect-key (kbd &key to-be)
  `(is (equalp (parse-key ,kbd) (expand-key-description ,@to-be))))

(fiasco:deftest test-parse-key ()
  (expect-key "C-l" :to-be (108 mahogany/keyboard::+modifier-ctrl+))
  (expect-key "C-L" :to-be (76 mahogany/keyboard::+modifier-ctrl+))
  (expect-key "C-s-l" :to-be (108 mahogany/keyboard::+modifier-ctrl+
				  mahogany/keyboard::+modifier-super+))
  (expect-key "C-S-F1" :to-be (65470 mahogany/keyboard::+modifier-ctrl+
				     mahogany/keyboard::+modifier-shift+))
  (expect-key "C--" :to-be (45 mahogany/keyboard::+modifier-ctrl+))
  (expect-key "M-RET" :to-be (65293 mahogany/keyboard::+modifier-alt+))
  (expect-key "-" :to-be (45)))

(fiasco:deftest parse-key-signals-errors ()
  (signals kbd-parse-error (parse-key "C-"))
  (signals kbd-parse-error (parse-key "B-")))

(fiasco:deftest define-kmap-returns-kmap ()
  (let ((kmap (define-kmap)))
    (fiasco:is (kmap-p kmap))))

(fiasco:deftest define-key-adds-binding ()
  (let ((map (define-kmap))
	(key (kbd "C-s"))
	(command 'foo))
    (define-key map key command)

    (fiasco:is (equalp command
		       (kmap-lookup map key)))))

(fiasco:deftest define-key-overwrites-binding ()
  (let ((map (define-kmap))
	(key (kbd "M-8")))
    (define-key map key 'foo)
    (define-key map key 'bar)
    (fiasco:is (equalp 'bar (kmap-lookup map key)))))

(fiasco:deftest advance-key-state-returns-correct-finish-found-state ()
  (let* ((key (kbd "C-s"))
	 (command 'foo)
	 (state (make-key-state (list (define-kmap key command)))))
    (multiple-value-bind (matched result) (key-state-advance key state)
      (is (eql matched t))
      (is (eql result command)))))

(fiasco:deftest advance-key-state-returns-correct-finish-not-found-state ()
  (let* ((key (kbd "C-s"))
	 (state (make-key-state (list (define-kmap))))
	 (matched (key-state-advance key state)))
    (is (eql matched nil))))

(fiasco:deftest advance-key-state-returns-correct-continue-state ()
  (let* ((key (kbd "C-s"))
	 (other-kmap (define-kmap))
	 (state (make-key-state (list (define-kmap key other-kmap)))))
    (multiple-value-bind (matched result) (key-state-advance key state)
      (is (eql matched t))
      (is (eql result nil))
      (is (equalp (mahogany/keyboard::key-state-kmaps state) (list other-kmap))))))

(defparameter *test-kmap* (define-kmap))

(fiasco:deftest advance-key-state-dereferences-dynamic-vars ()
  (let* ((key (kbd "C-s"))
	 (state (make-key-state (list (define-kmap key '*test-kmap*)))))
    (multiple-value-bind (matched result) (key-state-advance key state)
      (is (eql matched t))
      (is (eql result nil))
      (is (equalp (mahogany/keyboard::key-state-kmaps state) (list '*test-kmap*))))))

(fiasco:deftest advance-key-state-advances-all-when-not-found ()
  (let* ((key (kbd "M-e"))
	 (kmap-next (define-kmap (kbd "C-a") 'foo))
	 (kmap1 (define-kmap key kmap-next))
	 (kmap2 (define-kmap key *test-kmap*))
	 (state (make-key-state (list kmap1 kmap2))))
    (multiple-value-bind (matched result) (key-state-advance key state)
      (declare (ignore matched))
      (is (eql result nil))
      (is (equalp (mahogany/keyboard::key-state-kmaps state) (list kmap-next *test-kmap*))))))

(fiasco:deftest advance-key-state-progression-works ()
  (let* ((key (kbd "M-e"))
	 (kmap-next (define-kmap (kbd "C-a") 'foo))
	 (kmap1 (define-kmap key kmap-next))
	 (kmap2 (define-kmap key *test-kmap*))
	 (state (make-key-state (list kmap1 kmap2))))
    (multiple-value-bind (matched result) (progn
					    (key-state-advance key state)
					    (key-state-advance (kbd  "C-a") state))
      (is (eql matched t))
      (is (eql result 'foo)))))
