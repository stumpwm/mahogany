;; A large part of this file is taken directly from stumpwm:

;; Copyright (C) 2003-2008 Shawn Betts
;;
;;  This file is part of stumpwm.
;;
;; stumpwm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; stumpwm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;; Commentary:
;;
;; This file handles keymaps
;;
;; Code:

(in-package #:mahogany/keyboard)

(defstruct (key (:constructor make-key (keysym modifier-mask)))
  (keysym 0 :read-only t :type (unsigned-byte 32))
  (modifier-mask 0 :read-only t :type (unsigned-byte 32)))

(defun key-equal (a b)
  (declare (type key a b))
  (and (= (key-keysym a)
          (key-keysym b))
       (= (key-modifier-mask a)
          (key-modifier-mask b))))

(declaim (type (unsigned-byte 32)
               +modifier-shift+
               +modifier-caps+
               +modifier-ctrl+
               +modifier-alt+
               +modifier-mod2+
               +modifier-mod3+
               +modifier-super+
               +modifier-mod5+))
(defconstant +modifier-shift+ (ash 1 0))
(defconstant +modifier-caps+ (ash 1 1))
(defconstant +modifier-ctrl+ (ash 1 2))
(defconstant +modifier-alt+ (ash 1 3))
(defconstant +modifier-mod2+ (ash 1 4))
(defconstant +modifier-mod3+ (ash 1  5))
(defconstant +modifier-super+ (ash 1 6))
(defconstant +modifier-mod5+ (ash 1 7))

(defun print-key (key &optional (stream *standard-output*))
  (declare (type key key) (type stream stream)
           (optimize (safety 1)))
  (let ((mod (key-modifier-mask key)))
    (format stream "(Keycode: ~A Modifiers: (" (key-keysym key))
    (when (not (= mod 0))
      (when (/= 0 (logand +modifier-shift+ mod))
        (format stream "SHIFT"))
      (when (/= 0 (logand +modifier-caps+ mod))
        (format stream "CAPS"))
      (when (/= 0 (logand +modifier-ctrl+ mod))
        (format stream "CONTROL"))
      (when (/= 0 (logand +modifier-alt+ mod))
        (format stream "ALT"))
      ;; FIXME: one of these modifiers is probably the hyper key
      (when (/= 0 (logand +modifier-mod2+ mod))
        (format stream "MOD2"))
      (when (/= 0 (logand +modifier-mod3+ mod))
        (format stream "MOD3"))
      (when (/= 0 (logand +modifier-super+ mod))
        (format stream "SUPER"))
      (when (/= 0 (logand +modifier-mod5+ mod))
        (format stream "MOD5"))))
  (format stream "))"))

(defun %report-kbd-parse-error (c stream)
  (format stream "Failed to parse key string: ~s." (kbd-parse-error-string c))
  (when-let ((reason (kbd-parse-error-reason c)))
    (format stream "~%Reason: ~A" reason)))

(define-condition kbd-parse-error (mahogany-error)
  ((string :initarg :string
           :reader kbd-parse-error-string)
   (reason :initarg :reason :reader kbd-parse-error-reason
           :initform nil))
  (:report %report-kbd-parse-error)
  (:documentation "Raised when a kbd string failed to parse."))

(defun %parse-mods (mods end)
  "MODS is a sequence of <MOD CHAR> #\- pairs. Returns a bitfield with
 the appropriate bits set for the given modifiers chars"
  (unless (evenp end)
    (error 'kbd-parse-error :string mods
           :reason "Did you forget to separate modifier characters with '-'?"))
  (let ((mod-mask 0))
    (declare (type (unsigned-byte 32) mod-mask))
    (loop for i from 0 below end by 2
          when (char/= (char mods (1+ i)) #\-)
          do (error 'kbd-parse-error :string mods)
          do (setf mod-mask (logior mod-mask
                                    (case (char mods i)
                                      (#\M +modifier-alt+)
                                      (#\A +modifier-alt+)
                                      (#\C +modifier-ctrl+)
                                      (#\H (error 'kbd-parse-error
                                                  :string mods
                                                  :reason
                                                  "Fixme: don't know which key is the Hyper modifier."))
                                      (#\s +modifier-super+)
                                      (#\S +modifier-shift+)
                                      (t (error 'kbd-parse-error :string mods
                                                :reason (format nil "Unknown modifer character ~A" (char mods i))))))))
    mod-mask))

(defun key-modifier-key-p (key)
  "Check if the given key is a modifier key like Shift or Control"
  (declare (type key key))
  ;; FIXME: don't hardcode these values.
  ;; Using a cleverer datastructure might be good too.
  (find (key-keysym key) #(65515 ; super
                           65507 ; Control_L
                           65508 ; Control_R
                           65513 ; alt
                           65505 ; Shift_L
                           65506 ; Shilf_R
                           )))


(defun parse-key (string)
  "Parse STRING and return a key structure. Raise an error of type
kbd-parse if the key failed to parse."
  (let* ((p (when (> (length string) 2)
              (position #\- string :from-end t :end (- (length string) 1))))
         (mod-mask  (if p (%parse-mods string (1+ p)) 0))
         (key-part (subseq string (if p (1+ p) 0)))
         (keysym (stumpwm-name->keysym key-part)))
    (if keysym
        (make-key keysym mod-mask)
        (error 'kbd-parse-error :string string))))

;; The stumpwm version can take key specs split by a newline,
;; but since only the first value is returned out of those,
;; it should be okay to just accept a single spec here for now:
(defun kbd (key)
  "This compiles a key string into a key structure."
  ;; TODO: make this function accept a list of strings or
  ;; a string of keyspecs separated by spaces
  (parse-key key))
