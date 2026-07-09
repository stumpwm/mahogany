(in-package #:mahogany/keyboard)

;; For SBCL to inline the math,
;; the constants here need to be fixnums and somewhat small:
(declaim (type
          fixnum
          +x+ +y+ +z+))
(defconstant +x+ 3499211612)
(defconstant +y+ 581869302)
(defconstant +z+ 3890346734)

;; this is probably the more "correct" hash due to
;; actually being for 32 bit numbers,
;; but it's less efficient on 64 bit systems
;; due to all the bit shifting to keep the results at 32
;; bits each.
#+(or 32-bit 32-bit-target)
(defun sxhash-key (key)
  (declare (type key key)
           (optimize (speed 3) (safety 1) (debug 0)))
  (flet ((hash-32 (a b)
           (declare (type (unsigned-byte 32) a b)
                    (optimize (speed 3)))
           (let ((two-to-32 (expt 2 32)))
             (mod (+ (* +x+ a) (floor (/ (* +y+ a) two-to-32))
                     (* +y+ b) (floor (/ (* +z+ b) two-to-32)))
                  two-to-32))))
    (hash-32 (key-modifier-mask key) (key-keysym key))))

#+(or 64-bit 64-bit-target)
(defun sxhash-key (key)
  (declare (type key key)
           (optimize (speed 3) (safety 1)))
  (flet ((hash-64 (a b)
           (declare (type (unsigned-byte 64) a b)
                    (optimize (speed 3)))
           (let ((two-to-32 (expt 2 64)))
             (mod (+ (* +x+ a) (floor (/ (* +y+ a) two-to-32))
                     (* +y+ b) (floor (/ (* +z+ b) two-to-32)))
                  two-to-32))))
    (logand (hash-64 (key-modifier-mask key) (key-keysym key))
            most-positive-fixnum)))
