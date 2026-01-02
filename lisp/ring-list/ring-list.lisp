(defpackage #:ring-list
  (:use :cl)
  (:export
   #:make-ring-list
   #:ring-list
   #:ring-list-size
   #:add-item
   #:remove-item
   #:pop-item
   #:pop-item-prev
   #:swap-next
   #:swap-previous
   #:swap-next-find
   #:swap-previous-find))

(in-package #:ring-list)

(defstruct (ring-item (:constructor make-ring-item (item prev next)))
  (next nil :type (or null ring-item))
  (prev nil :type (or null ring-item))
  (item nil))

(defstruct (ring-list (:constructor make-ring-list ()))
  (size 0 :type fixnum)
  (head nil :type (or null ring-item)))

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

(defun %find-item (ring-list item test)
  (declare (type ring-list ring-list)
           (type (or (function (t t) t) symbol) test)
           (optimize (speed 3) (safety 0)))
  (with-slots (head) ring-list
    (when head
      (do* ((cur (ring-item-next head) (ring-item-next cur)))
           (nil)
        (cond
          ((funcall test (ring-item-item cur) item)
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

(defun pop-item-prev (ring-list)
  "Remove the item before the head of the ist and return it"
  (declare (type ring-list ring-list))
  (alexandria:when-let* ((head (ring-list-head ring-list))
                         (last (ring-item-prev head)))
    (when last
      (let ((prev (ring-item-prev head)))
        (%remove-item ring-list prev)
        (ring-item-item prev)))))

(defun %swap-find (ring-list item test swap-fn)
  (declare (type ring-list ring-list)
           (type (function (ring-list t) t) swap-fn)
           (type (or (function (t t) t) symbol) test)
           (optimize (speed 3) (safety 0)))
  (alexandria:when-let ((item (%find-item ring-list item test)))
    ;; remove the ring item from where it was:
    (let ((item-prev (ring-item-prev item))
          (item-next (ring-item-next item)))
      (setf (ring-item-next item-prev) item-next
            (ring-item-prev item-next) item-prev))
    ;; and put it at the head of the list, moving the current head back.
    (with-slots (head) ring-list
      (let ((next (ring-item-next head)))
        (setf (ring-item-next head) item
              (ring-item-prev next) item
              head item)))
    (funcall swap-fn ring-list item)))

(defun swap-next-find (ring-list item &key (test #'equalp))
  "Find the given item in the list and move it to the head of list.
Then swap the found item for the given one like in swap-next"
  (declare (type ring-list ring-list)
           (type (or (function (t t) t) symbol) test))
  (%swap-find ring-list item test #'swap-next))

(defun swap-previous-find (ring-list item &key (test #'equalp))
  "Find the given item in the list and move it to the head of list.
Then swap the found item for the given one like in swap-previous"
  (declare (type ring-list ring-list)
           (type (or (function (t t) t) symbol) test))
  (%swap-find ring-list item test #'swap-previous))

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
