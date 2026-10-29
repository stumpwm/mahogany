(defpackage #:ring-list
  (:use :cl)
  (:export
   #:make-ring-list
   #:ring-list
   #:ring-list-size
   #:clear-list
   #:add-item
   #:add-item-prev
   #:remove-item
   #:pop-item
   #:pop-item-prev
   #:peek-item
   #:contains-item
   #:swap-next
   #:swap-previous
   #:swap-next-find
   #:swap-previous-find
   #:foreach-item))

(in-package #:ring-list)

(defstruct (ring-item (:constructor make-ring-item (item prev next)))
  (next nil :type (or null ring-item))
  (prev nil :type (or null ring-item))
  (item nil))

(defstruct (ring-list (:constructor make-ring-list ()))
  (size 0 :type fixnum)
  (head nil :type (or null ring-item)))

(defun clear-list (ring-list)
  (declare (type ring-list ring-list))
  (setf (ring-list-size ring-list) 0
        (ring-list-head ring-list) nil))

(defun add-item (ring-list item)
  "Add the given item to the head of the list"
  (declare (type ring-list ring-list))
  (with-slots (head size) ring-list
    (if (null head)
        (let ((new-head (make-ring-item item nil nil)))
          (setf (ring-item-next new-head) new-head
                (ring-item-prev new-head) new-head
                head new-head))
        (let* ((prev (ring-item-prev head))
               (new-item (make-ring-item item prev head)))
          (setf (ring-item-prev head) new-item
                (ring-item-next prev) new-item
                head new-item)))
    (incf size)))

(defun add-item-prev (ring-list item)
  "Add the given item to the tail of the list"
  (declare (type ring-list ring-list))
  (with-slots (head size) ring-list
    (if (null head)
        (let ((new-head (make-ring-item item nil nil)))
          (setf (ring-item-next new-head) new-head
                (ring-item-prev new-head) new-head
                head new-head))
        (let* ((tail (ring-item-prev head))
               (new-item (make-ring-item item tail head)))
          (setf (ring-item-prev head) new-item
                (ring-item-next tail) new-item)))
    (incf size)))

(defun %find-item (ring-list item test)
  (declare (type ring-list ring-list)
           (type (or function symbol) test)
           (optimize (speed 3) (safety 1)))
  (with-slots (head) ring-list
    (when head
      (do* ((cur (ring-item-next head) (ring-item-next cur))
            (test-fn (if (functionp test)
                         test
                         (fdefinition test))))
           (nil)
        (cond
          ((funcall test-fn (ring-item-item cur) item)
           (return-from %find-item cur))
          ((eql head cur)
           (return-from %find-item nil)))))))

(defun %remove-item (ring-list ring-item)
  (declare (type ring-list ring-list)
           (type ring-item ring-item)
           (optimize (speed 3) (safety 0)))
  (with-slots (head) ring-list
    (if (= 1 (ring-list-size ring-list))
        (setf head nil)
        (let ((prev (ring-item-prev ring-item))
              (next (ring-item-next ring-item)))
          (setf (ring-item-next prev) next
                (ring-item-prev next) prev)
          (when (eql ring-item head)
            (setf head next))))
    (decf (ring-list-size ring-list)))
  t)

(defun remove-item (ring-list item &key (test #'equalp))
  "Removes the given item from the list. Returns T if the item was
found and removed"
  (declare (type ring-list ring-list)
           (type (or (function (t t) t) symbol) test))
  (alexandria:when-let ((item (%find-item ring-list item test)))
    (%remove-item ring-list item)))

(defun pop-item (ring-list)
  "Remove the item from the top of the list and return it"
  (declare (type ring-list ring-list))
  (let ((head (ring-list-head ring-list)))
    (when head
      (%remove-item ring-list head)
      (ring-item-item head))))

(defun peek-item (ring-list)
  "View the item at the top of the list."
  (declare (type ring-list ring-list))
  (alexandria:when-let ((head (ring-list-head ring-list)))
    (ring-item-item head)))

(defun pop-item-prev (ring-list)
  "Remove the item before the head of the list (the tail) and return it"
  (declare (type ring-list ring-list))
  (alexandria:when-let* ((head (ring-list-head ring-list))
                         (last (ring-item-prev head)))
    (when last
      (let ((prev (ring-item-prev head)))
        (%remove-item ring-list prev)
        (ring-item-item prev)))))

(defun swap-next-find (ring-list find item &key (test #'equalp))
  "Remove FIND from the list, then put ITEM in the list as with add-item"
  (declare (type ring-list ring-list)
           (type (or (function (t t) t) symbol) test)
           (optimize (speed 3)))
  (alexandria:when-let ((node (%find-item ring-list find test)))
    (setf (ring-item-item node) item)
    ;; If there's only one node, we are already done;
    ;; Also, if the node is already the head, there's nothing to do:
    (unless (or (= 1 (ring-list-size ring-list))
                (eql node (ring-list-head ring-list)))
      ;; first remove the node:
      (let ((prev (ring-item-prev node))
            (next (ring-item-next node)))
        (setf (ring-item-next prev) next
              (ring-item-prev next) prev))
      ;; then place in before the head and shift the head:
      (with-slots (head) ring-list
        (let ((prev (ring-item-prev head)))
          (setf (ring-item-next prev) node
                (ring-item-prev head) node
                (ring-item-prev node) prev
                (ring-item-next node) head))
        (setf head node)))
    t))

(defun swap-previous-find (ring-list find item &key (test #'equalp))
  "Remove FIND from the list, then put ITEM in the list as with add-item-prev"
  (declare (type ring-list ring-list)
           (type (or (function (t t) t) symbol) test)
           (optimize (speed 3)))
  (alexandria:when-let ((node (%find-item ring-list find test)))
    (setf (ring-item-item node) item)
    ;; If there's only one node, we are already done;
    (unless (= 1 (ring-list-size ring-list))
      (if (eql node (ring-list-head ring-list))
          ;; If we are at the head, we need to shift it:
          (setf (ring-list-head ring-list) (ring-item-next node))
          (progn
            ;; first remove the node:
            (let ((prev (ring-item-prev node))
                  (next (ring-item-next node)))
              (setf (ring-item-next prev) next
                    (ring-item-prev next) prev))
            ;; place it before the head:
            (with-slots (head) ring-list
              (let ((prev (ring-item-prev head)))
                (setf (ring-item-next prev) node
                      (ring-item-prev head) node
                      (ring-item-prev node) prev
                      (ring-item-next node) head))))))
    t))

(defun swap-next (ring-list item)
  "Replace the item currently at the head of the list with the given item,
and move the head of the list forward one item"
  (declare (type ring-list ring-list) (optimize (speed 3)))
  (with-slots (head) ring-list
    (when (not head)
      (error "The ring list must have an item to swap with"))
    (let ((head-item (ring-item-item head)))
      (setf (ring-item-item head) item
            head (ring-item-next head))
      head-item)))

(defun swap-previous (ring-list item)
  "Move the head of the list backward one item and replace its item for the given one.
Reverses the action that swap-next performs"
  (declare (type ring-list ring-list) (optimize (speed 3)))
  (with-slots (head) ring-list
    (when (not head)
      (error "The ring list must have an item to swap with"))
    (let* ((prev (ring-item-prev head))
           (prev-item (ring-item-item prev)))
      (setf (ring-item-item prev) item
            head prev)
      prev-item)))

(defun contains-item (ring-list item &key (test #'equalp))
  (declare (type ring-list ring-list))
  (let ((found (%find-item ring-list item test)))
    (when found
      (ring-item-item found))))

(defmacro foreach-item ((var lst) &body body)
  (let ((lst-var (gensym))
        (cur (gensym "cur"))
        (top-tag (gensym "top")))
    `(let ((,lst-var ,lst))
       (block nil
         (let ((,cur (ring-list-head ,lst-var)))
           (when ,cur
             (tagbody
                ,top-tag
                (let ((,var (ring-item-item ,cur)))
                  ,@body)
                (setf ,cur (ring-item-next ,cur))
                (when (not (eql (ring-list-head ,lst-var) ,cur))
                  (go ,top-tag)))))))))

;; We need to re-define print-object to prevent infinite recursion
;; when chasing the next and previous pointers:
(defmethod print-object ((ring-item ring-item) stream)
  (format stream "#S(~S :item ~S)" 'ring-item (ring-item-item ring-item)))

(defmethod print-object ((ring-list ring-list) stream)
  (let ((head (ring-list-head ring-list)))
    (format stream "(")
    (when head
      (format stream "*-> ~S" (ring-item-item head))
      (do ((cur (ring-item-next head) (ring-item-next cur)))
          (nil)
        (when (eql head cur)
          (return nil))
        (format stream "-> ~S" (ring-item-item cur))))
    (format stream ")")))

(defun print-backwards (ring-list &optional (stream t))
  (let ((head (ring-list-head ring-list)))
    (format stream "(")
    (when head
      (format stream "*-> ~S" (ring-item-item head))
      (do ((cur (ring-item-prev head) (ring-item-prev cur)))
          (nil)
        (when (eql head cur)
          (return nil))
        (format stream "-> ~S" (ring-item-item cur))))
    (format stream ")")))
