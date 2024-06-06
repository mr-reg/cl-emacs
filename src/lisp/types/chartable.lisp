;; Copyright (C) 2024 by Gleb Borodulia
;; Author: Gleb Borodulia <mr.reg@mail.ru>

;; This file is part of cl-emacs.

;; cl-emacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; cl-emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with cl-emacs. If not, see <https://www.gnu.org/licenses/>.

(uiop::define-package :cl-emacs/types/chartable
    (:use
     :defstar
     :common-lisp
     :cl-emacs/log
     :fiveam
     :cl-emacs/commons)
  (:export )
  )
(in-package :cl-emacs/types/chartable)
(log-enable :cl-emacs/types/chartable :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(def-suite cl-emacs/types/chartable)
(in-suite cl-emacs/types/chartable)

;;; sparse vector implementation with some business fields
;; leveled structure of the vector is hardcoded and designed
;; to store all unicode character table

(defconstant +chartab-size-bits+ #(6 4 5 7))
;; (defconstant +chartab)
;; 0 . 4194303

;; #^[10 nil test 10
;; 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10
;; 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10
;; 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10
;; 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10]

;; Number of elements in Nth level char-table.
(defconstant +chartab-size+
  (coerce (mapcar #'(lambda (x)
                      (ash 1 x))
                  (coerce +chartab-size-bits+ 'list)) 'vector))

(defstruct (chartable (:copier nil)
                      (:print-function print-chartable)                      )
  ;; This holds the default value, which is used whenever the value
  ;; for a specific character is nil.
  (default nil)
  ;; This points to another char table, from which we inherit when the
  ;; value for a specific character is nil.  The `defalt' slot takes
  ;; precedence over this.
  (parent nil :type (or null chartable))
  ;; This is a symbol which says what kind of use this char-table is
  ;; meant for.
  (purpose nil :type symbol)
  ;;The bottom sub char-table for characters in the range 0..127.  It
  ;;is nil if no ASCII character has a specific value.
  (ascii nil)

  (contents nil :type simple-vector))

(defun* (make-simple-chartable -> chartable) (&key default parent
                                                   purpose)
  (assert (not (null purpose)))
  (make-chartable :default default
                  :parent parent
                  :purpose purpose
                  :ascii default
                  :contents (make-array (ash 1 (aref +chartab-size-bits+ 0))
                                        :initial-element default))
  )

(defun* print-chartable ((ct chartable) (stream stream) depth)
  (declare (ignore depth))
  (with-slots (default parent purpose ascii contents) ct
    (format stream "#^[~s ~s ~s ~s" default parent purpose ascii)
    (loop for sub-table across contents
          do (format stream " ~s" sub-table))
    (format stream "]")))

(defstruct (sub-chartable (:copier nil)
                          (:print-function print-sub-chartable))
  ;; Depth of this sub char-table.  It should be 1, 2, or 3.  A sub
  ;; char-table of depth 1 contains 16 elements, and each element
  ;; covers 4096 (128*32) characters.  A sub char-table of depth 2
  ;; contains 32 elements, and each element covers 128 characters.  A
  ;; sub char-table of depth 3 contains 128 elements, and each element
  ;; is for one character.
  (depth nil :type fixnum)
  ;; Minimum character covered by the sub char-table.
  (min-char nil :type fixnum)

  (contents nil :type simple-vector)
  )

(defun* print-sub-chartable ((sct sub-chartable) (stream stream) depth)
  (declare (ignore depth))
  (with-slots (depth min-char contents) sct
    (format stream "~%#^^[~s ~s" depth min-char)
    (loop for sub-table across contents
          do (format stream " ~s" sub-table))
    (format stream "]")))

(test test-print-chartable
  (let ((ct (make-simple-chartable :purpose 'test-purpose :default 10)))
    (is (string= "#^[10 NIL TEST-PURPOSE 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10]"
                 (format nil "~s" ct)))))

(defun* set-char-table-range ((ct chartable) (from fixnum) (to fixnum) value)

  )

;; #^(8 nil test
;;      #^^(3 0 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3)
;;      #^^(1 0
;;            #^^(2 0
;;                  #^^(3 0 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
;;                        3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3)
;;                  3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3)
;;            3 3 3 3 3 3 3 3 3 3 3 3 3 3 3)
;;      #^^(1 65536
;;            3 3 #^^(2 73728 3
;;                      #^^(3 73856 3 3 3 3 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8)
;;                      8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8)
;;            8 8 8 8 8 8 8 8 8 8 8 8 8)
;;      8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8)

(test test-set-char-table-range
  (is (string= #M"#^[8 nil test
                  #^^[3 0 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3] #^^[1 0 #^^[2 0
                  #^^[3 0 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3] #^^[1 65536 3 3 #^^[2 73728 3
                  #^^[3 73856 3 3 3 3 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] 8 8 8 8 8 8 8 8 8 8 8 8 8] 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8]"
               (let ((ct (make-char-table 'test 8)))
                 (set-char-table-range ct '(80 . 73859) 3)
                 (format nil "~s" ct))))
  )


(defun test-me ()
  (run! 'cl-emacs/types/chartable))
