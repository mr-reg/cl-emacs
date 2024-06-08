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

(cl-emacs/elisp-packages:define-elisp-package :cl-emacs/types/chartable
    (:use
     :defstar
     :cl-emacs/data
     :cl-emacs/log
     :cl-emacs/fns
     :cl-emacs/eval
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

(define-condition invalid-chartable-operation (error-with-description)
  ())

;; 64/16/32/128
(defconstant +chartab-size-bits+ #(6 4 5 7))

;; Number of elements in Nth level chartable.
(defconstant +chartab-size+
  (make-array
   4
   :initial-contents
   (list (ash 1 (aref +chartab-size-bits+ 0))
         (ash 1 (aref +chartab-size-bits+ 1))
         (ash 1 (aref +chartab-size-bits+ 2))
         (ash 1 (aref +chartab-size-bits+ 3)))
   ))

;; Number of characters each element of Nth level chartable covers.
(defconstant +chars-per-element+
  (make-array
   4
   :initial-contents
   (list (ash 1 (+ (aref +chartab-size-bits+ 1)
                   (aref +chartab-size-bits+ 2)
                   (aref +chartab-size-bits+ 3)))
         (ash 1 (+ (aref +chartab-size-bits+ 2)
                   (aref +chartab-size-bits+ 3)))
         (ash 1  (aref +chartab-size-bits+ 3))
         1)))
;; 0 . 4194303

;; #^[10 nil test 10
;; 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10
;; 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10
;; 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10
;; 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10]

(defstruct (chartable (:copier nil)
                      (:print-function print-chartable)                      )
  ;; This holds the default value, which is used whenever the value
  ;; for a specific character is nil.
  (default nil)
  ;; This points to another char table, from which we inherit when the
  ;; value for a specific character is nil.  The `defalt' slot takes
  ;; precedence over this.
  (parent nil :type (or null chartable))
  ;; This is a symbol which says what kind of use this chartable is
  ;; meant for.
  (purpose nil :type symbol)
  ;;The bottom sub chartable for characters in the range 0..127.  It
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
    (cl:format stream "#^[~s ~s ~s ~s" default parent purpose ascii)
    (loop for sub-table across contents
          do (cl:format stream " ~s" sub-table))
    (cl:format stream "]")))

(defstruct (sub-chartable (:copier nil)
                          (:print-function print-sub-chartable))
  ;; Depth of this sub chartable.  It should be 1, 2, or 3.  A sub
  ;; chartable of depth 1 contains 16 elements, and each element
  ;; covers 4096 (128*32) characters.  A sub chartable of depth 2
  ;; contains 32 elements, and each element covers 128 characters.  A
  ;; sub chartable of depth 3 contains 128 elements, and each element
  ;; is for one character.
  (depth nil :type fixnum)
  ;; Minimum character covered by the sub chartable.
  (min-char nil :type fixnum)

  (contents nil :type simple-vector)
  )

(defun* print-sub-chartable ((sct sub-chartable) (stream stream) depth)
  (declare (ignore depth))
  (with-slots (depth min-char contents) sct
    (when (= depth 3)
      (cl:format stream "~%"))
    (cl:format stream "#^^[~s ~s" depth min-char)
    (loop for sub-table across contents
          do (cl:format stream " ~s" sub-table))
    (cl:format stream "]")))

(test test-print-chartable
  (let ((ct (make-simple-chartable :purpose 'test-purpose :default 10)))
    (is (string= "#^[10 NIL TEST-PURPOSE 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10]"
                 (cl:format nil "~s" ct)))))

;; chartables has no equality check, function will always set value without
;; content analysis. only structure matters
(defun* set-sub-chartable-range (&key range-from range-to sub-ct-depth
                                      sub-ct-from sub-ct-to
                                      sub-ct
                                      value)
  "returns new/modified sub-chartable"
  (when (> sub-ct-depth 3)
    (return-from set-sub-chartable-range value))
  (log-debug2 "set-sub-chartable-range range-from:~s range-to:~s sub-ct-depth:~s sub-ct-from:~s sub-ct-to:~s value:~s"
              range-from range-to sub-ct-depth sub-ct-from sub-ct-to value)
  (let* ((sub-ct-element-size (aref +chars-per-element+ sub-ct-depth))
         (is-simple (not (sub-chartable-p sub-ct))))
    ;; (log-debug2 "is-simple:~s" is-simple)
    (cond
      ;; sub-chartable is still one element and can be updated in simple mode
      ((and is-simple (<= range-from sub-ct-from) (>= range-to sub-ct-to))
       value)
      (t
       ;; expand sub-chartable to array if required
       (when is-simple
         (setq sub-ct (make-sub-chartable
                       :depth sub-ct-depth
                       :min-char sub-ct-from
                       :contents (make-array (aref +chartab-size+ sub-ct-depth)
                                             :initial-element sub-ct))))
       (loop for idx from 0
             for sub-sub-ct across (sub-chartable-contents sub-ct)
             for sub-from from sub-ct-from by sub-ct-element-size
             for sub-to = (1- (+ sub-from sub-ct-element-size))
             do (when (and (<= range-from sub-to) (>= range-to sub-from))
                  (setf (aref (sub-chartable-contents sub-ct) idx)
                        (set-sub-chartable-range
                         :range-from range-from :range-to range-to
                         :sub-ct-depth (1+ sub-ct-depth)
                         :sub-ct-from sub-from :sub-ct-to sub-to
                         :sub-ct sub-sub-ct :value value))))
       sub-ct))))

(defun* set-chartable-range ((ct chartable) (range-from fixnum) (range-to fixnum) value)
  "main modification function. Expands sub-chartables if required"
  (with-slots (default parent purpose ascii contents) ct
    (when (< range-from 128)
      (setf ascii
            (set-sub-chartable-range :range-from range-from :range-to range-to :sub-ct-depth 3
                                     :sub-ct-from 0 :sub-ct-to 127 :sub-ct ascii
                                     :value value)))
    (let ((element-size (aref +chars-per-element+ 0)))
      (loop for idx from 0
            for sub-ct across contents
            for sub-from from 0 by element-size
            for sub-to = (1- (+ sub-from element-size))
            do (when (and (<= range-from sub-to) (>= range-to sub-from))
                 (setf (aref contents idx)
                       (set-sub-chartable-range
                        :range-from range-from :range-to range-to
                        :sub-ct-depth 1
                        :sub-ct-from sub-from :sub-ct-to sub-to
                        :sub-ct sub-ct :value value)))))))


(defun* optimize-sub-chartable ((sub-ct sub-chartable) &optional (test 'equal))
  (with-slots (contents) sub-ct
    (let ((optimizable t))
      (loop for idx from 0
            for sub-sub-ct across contents
            do (when (sub-chartable-p sub-sub-ct)
                 (setf (aref contents idx) (optimize-sub-chartable sub-sub-ct test)))
               (when (or (sub-chartable-p (aref contents idx))
                         (not (funcall test (aref contents idx) (aref contents 0))))
                 (setq optimizable nil)))
      (if optimizable
          (aref contents 0)
          sub-ct))))

(defun* optimize-chartable ((ct chartable) &optional (test 'equal))
  (with-slots (ascii contents) ct
    (when (sub-chartable-p ascii)
      (setq ascii (optimize-sub-chartable ascii test)))
    (loop for idx from 0
          for sub-ct across contents
          do (when (sub-chartable-p sub-ct)
               (setf (aref contents idx) (optimize-sub-chartable sub-ct test))))
    ))

(test test-set-chartable-range
  ;; simple set in ascii + big range + useless optimization
  (is (string= (cl:format nil "~a~%~a~%~a"
                          "#^[8 NIL TEST "
                          "#^^[3 0 8 8 8 3 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] #^^[1 0 #^^[2 0 "
                          "#^^[3 0 8 8 8 3 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8]")
               (let ((ct (make-simple-chartable :default 8 :purpose 'test)))
                 (set-chartable-range ct 3 3 3)
                 (optimize-chartable ct)
                 (cl:format nil "~s" ct))))
  ;; simple set with zero-length range, still causes array expansion
  (is (string= (cl:format nil "~a~%~a~%~a"
                          "#^[8 NIL TEST "
                          "#^^[3 0 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] #^^[1 0 #^^[2 0 "
                          "#^^[3 0 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8]")
               (let ((ct (make-simple-chartable :default 8 :purpose 'test)))
                 (set-chartable-range ct 4 3 3)
                 (cl:format nil "~s" ct))))
  ;; simple set in big range only
  (is (string= (cl:format nil "~a~%~a"
                          "#^[8 NIL TEST 8 #^^[1 0 #^^[2 0 8 "
                          "#^^[3 128 3 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8]")
               (let ((ct (make-simple-chartable :default 8 :purpose 'test)))
                 (set-chartable-range ct 128 128 3)
                 (cl:format nil "~s" ct))))
  ;; complex set
  (is (string= (cl:format nil "~a~%~a~%~a~%~a"
                          "#^[8 NIL TEST "
                          "#^^[3 0 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3] #^^[1 0 #^^[2 0 "
                          "#^^[3 0 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3] #^^[1 65536 3 3 #^^[2 73728 3 "
                          "#^^[3 73856 3 3 3 3 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8] 8 8 8 8 8 8 8 8 8 8 8 8 8] 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8]")
               (let ((ct (make-simple-chartable :default 8 :purpose 'test)))
                 (set-chartable-range ct 80 73859 3)
                 (cl:format nil "~s" ct))))


  ;; good optimization case
  (is (string= "#^[8 NIL TEST 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8]"
               (let ((ct (make-simple-chartable :default 8 :purpose 'test)))
                 (set-chartable-range ct 3 3 3)
                 (set-chartable-range ct 3 3 8)
                 (optimize-chartable ct)
                 (cl:format nil "~s" ct))))

  )


(defun* get-sub-chartable-value ((sub-ct sub-chartable) (idx fixnum))
  "we are sure that idx is inside this sub-chartable"
  (with-slots (min-char depth contents) sub-ct
    (let ((element-size (aref +chars-per-element+ depth)))
      (loop for sub-sub-ct across contents
            for sub-from from min-char by element-size
            for sub-to = (1- (+ sub-from element-size))
            do (when (<= sub-from idx sub-to)
                 (return-from get-sub-chartable-value (if (sub-chartable-p sub-sub-ct)
                                                          (get-sub-chartable-value sub-sub-ct idx)
                                                          sub-sub-ct))))))
  ;; impossible case
  (error 'invalid-chartable-operation
         :details (cl:format nil "wrong sub-chartable index ~s" idx))
  )
(defun* get-chartable-value ((ct chartable) (idx fixnum))
  (with-slots (ascii contents parent) ct
    (let* ((element-size (aref +chars-per-element+ 0))
           (local-result
             (if (<= 0 idx 127)
                 ;; fastest access to ascii range
                 (if (sub-chartable-p ascii)
                     (aref (sub-chartable-contents ascii) idx)
                     ascii)
                 ;; deep scan
                 (loop for sub-ct across contents
                       for sub-from from 0 by element-size
                       for sub-to = (1- (+ sub-from element-size))
                       do (when (<= sub-from idx sub-to)
                            (return
                              (if (sub-chartable-p sub-ct)
                                  (get-sub-chartable-value sub-ct idx)
                                  sub-ct)))
                       finally
                          (error 'invalid-chartable-operation :details
                                 (cl:format nil "wrong chartable index ~s" idx))))))
      (return-from get-chartable-value
        (if (and (null local-result) parent)
            (get-chartable-value parent idx)
            local-result)))))

(test test-get-chartable-value
  (let* ((ct1 (make-simple-chartable :purpose 'test :default 1))
         (ct2 (make-simple-chartable :purpose 'test :parent ct1))
         (ct3 (make-simple-chartable :purpose 'test :parent ct2)))

    (set-chartable-range ct2 4 15000 2)
    (set-chartable-range ct3 10 15 3)
    (is (= 3 (get-chartable-value ct3 12)))
    (is (= 2 (get-chartable-value ct3 9)))
    (is (= 1 (get-chartable-value ct3 15001)))
    (setf (chartable-parent ct2) nil)
    (is (null (get-chartable-value ct3 15001)))
    (signals invalid-chartable-operation (get-chartable-value ct3 -1))
    (signals invalid-chartable-operation (get-chartable-value ct3 4194304))
    )
  )

;; TODO: add support for map functions
;; TODO: add support for extra slots

(defun test-me ()
  (run! 'cl-emacs/types/chartable))
