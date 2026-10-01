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

(defun make-test-mode (name &rest top-bindings)
  "Make a kmap-mode whose top kmap contains the key/command pairs in
TOP-BINDINGS."
  (let ((top (mahogany/keyboard:make-kmap)))
    (loop :for (key command) :on top-bindings :by #'cddr
          :do (mahogany/keyboard:define-key top key command))
    (mahogany::make-kmap-mode name top (mahogany/keyboard:make-kmap) nil)))

(defmacro with-test-state ((state &rest modes) &body body)
  "Run BODY with STATE bound to a fresh state that has MODES activated in
order. The prefix passthrough kmap is rebound so that it is not shared
with the running compositor or with other tests."
  (let ((mode (gensym "MODE")))
    `(let ((,state (mahogany::make-mahogany-state))
           (mahogany::*prefix-passthrough-kmap*
             (define-kmap (kbd "C-t") :pass-through)))
       (dolist (,mode (list ,@modes))
         (mahogany::kmap-mode-activate ,state ,mode))
       ,@body)))

(defun lookup-key (state key)
  "Return what pressing KEY does given the current keybindings of STATE."
  (nth-value 1 (mahogany/keyboard:key-state-advance
                key
                (mahogany/keyboard:make-key-state
                 (mahogany::state-keybindings state)))))

(defun prefix-map-bound-p (state key mode)
  "Check if KEY leads to the prefix bindings of MODE in STATE."
  (find (mahogany::kmap-mode-prefix-binding mode)
        (mapcar (lambda (kmap) (mahogany/keyboard:kmap-lookup kmap key))
                (mahogany::state-keybindings state))))

(fiasco:deftest setf-prefix-key-stores-new-key ()
  (with-test-state (state)
    (is (equalp (kbd "C-z")
                (setf (mahogany::state-prefix-key state) (kbd "C-z"))))
    (is (equalp (kbd "C-z") (mahogany::state-prefix-key state)))))

(fiasco:deftest setf-prefix-key-moves-prefix-bindings ()
  (let ((mode (make-test-mode 'a-mode)))
    (with-test-state (state mode)
      (is (prefix-map-bound-p state (kbd "C-t") mode))
      (setf (mahogany::state-prefix-key state) (kbd "C-z"))
      (is (prefix-map-bound-p state (kbd "C-z") mode))
      (is (not (prefix-map-bound-p state (kbd "C-t") mode))))))

(fiasco:deftest setf-prefix-key-updates-key-state ()
  ;; The key state is what key presses are actually matched against.
  (let ((mode (make-test-mode 'a-mode)))
    (with-test-state (state mode)
      (setf (mahogany::state-prefix-key state) (kbd "C-z"))
      (is (eq (mahogany::state-keybindings state)
              (mahogany/keyboard::key-state-kmaps
               (mahogany::state-key-state state)))))))

(fiasco:deftest setf-prefix-key-moves-passthrough-binding ()
  (with-test-state (state)
    (flet ((passthrough (key)
             (mahogany/keyboard:kmap-lookup
              mahogany::*prefix-passthrough-kmap* key)))
      (setf (mahogany::state-prefix-key state) (kbd "C-z"))
      (is (eq :pass-through (passthrough (kbd "C-z"))))
      (is (null (passthrough (kbd "C-t"))))
      ;; The old key can only be removed if the new one was remembered:
      (setf (mahogany::state-prefix-key state) (kbd "C-y"))
      (is (eq :pass-through (passthrough (kbd "C-y"))))
      (is (null (passthrough (kbd "C-z")))))))

(fiasco:deftest setf-prefix-key-to-current-key-changes-nothing ()
  (let ((mode (make-test-mode 'a-mode)))
    (with-test-state (state mode)
      (setf (mahogany::state-prefix-key state) (kbd "C-t"))
      (is (equalp (kbd "C-t") (mahogany::state-prefix-key state)))
      (is (prefix-map-bound-p state (kbd "C-t") mode))
      (is (= 2 (length (mahogany::state-keybindings state))))
      (is (eq :pass-through
              (mahogany/keyboard:kmap-lookup
               mahogany::*prefix-passthrough-kmap* (kbd "C-t")))))))

(fiasco:deftest mode-activated-after-prefix-change-uses-new-prefix ()
  (let ((mode (make-test-mode 'a-mode)))
    (with-test-state (state)
      (setf (mahogany::state-prefix-key state) (kbd "C-z"))
      (mahogany::kmap-mode-activate state mode)
      (is (prefix-map-bound-p state (kbd "C-z") mode))
      (is (not (prefix-map-bound-p state (kbd "C-t") mode))))))

(fiasco:deftest mode-can-be-deactivated-after-prefix-change ()
  (let ((a (make-test-mode 'a-mode))
        (b (make-test-mode 'b-mode)))
    (with-test-state (state a b)
      (setf (mahogany::state-prefix-key state) (kbd "C-z"))
      (mahogany::kmap-mode-deactivate state a)
      (is (not (prefix-map-bound-p state (kbd "C-z") a)))
      (is (prefix-map-bound-p state (kbd "C-z") b))
      (is (= 2 (length (mahogany::state-keybindings state))))
      (mahogany::kmap-mode-deactivate state b)
      (is (null (mahogany::state-keybindings state))))))
