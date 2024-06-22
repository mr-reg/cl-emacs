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

(cl-emacs/elisp-packages:define-elisp-package :cl-emacs/types/chartables
    (:use
     :defstar
     :cl-emacs/data
     :cl-emacs/log
     :cl-emacs/fns
     :cl-emacs/eval
     :fiveam
     :snakes
     :cl-emacs/commons)
  (:import-from #:alexandria
                #:define-constant)
  (:export #:+chartab-size+
           #:chartable-extra-slots
           #:generate-chartable-ranges
           #:make-chartable
           #:make-simple-chartable
           #:make-sub-chartable
           #:sub-chartable-contents
           #:sub-chartable-depth
           #:sub-chartable-min-char
           )
  (:local-nicknames (#:el #:cl-emacs/elisp)))
(in-package :cl-emacs/types/chartables)
(log-enable :cl-emacs/types/chartables :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(def-suite cl-emacs/types/chartables)
(in-suite cl-emacs/types/chartables)

;;; sparse vector implementation with some business fields
;; leveled structure of the vector is hardcoded and designed
;; to store all unicode character table

(define-condition invalid-chartable-operation (error-with-description)
  ())

;; 64/16/32/128
(define-constant +chartab-size-bits+ #(6 4 5 7) :test 'equal)


;; Number of elements in Nth level chartable.
(define-constant +chartab-size+
    (make-array
     4
     :initial-contents
     (list (ash 1 (aref +chartab-size-bits+ 0))
           (ash 1 (aref +chartab-size-bits+ 1))
           (ash 1 (aref +chartab-size-bits+ 2))
           (ash 1 (aref +chartab-size-bits+ 3)))
     )
  :test 'equal)

;; Number of characters each element of Nth level chartable covers.
(define-constant +chars-per-element+
    (make-array
     4
     :initial-contents
     (list (ash 1 (+ (aref +chartab-size-bits+ 1)
                     (aref +chartab-size-bits+ 2)
                     (aref +chartab-size-bits+ 3)))
           (ash 1 (+ (aref +chartab-size-bits+ 2)
                     (aref +chartab-size-bits+ 3)))
           (ash 1  (aref +chartab-size-bits+ 3))
           1))
  :test 'equal)

;; each chartable covers range (0 . 4194303)

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

  (contents nil :type simple-vector)

  (extra-slots nil :type simple-vector)
  )

(defun* (make-simple-chartable -> chartable) (&key default parent
                                                   purpose)
  (assert (not (null purpose)))
  (make-chartable :default default
                  :parent parent
                  :purpose purpose
                  :ascii default
                  :contents (make-array (ash 1 (aref +chartab-size-bits+ 0))
                                        :initial-element default)
                  :extra-slots (make-array (or (cl:get purpose 'el::char-table-extra-slots)
                                               0)
                                           :initial-element default)))

(defun* print-chartable ((ct chartable) (stream stream) depth)
  (declare (ignore depth))
  (with-slots (default parent purpose ascii contents extra-slots) ct
    (cl:format stream "#^[~s ~s ~s ~s" default parent purpose ascii)
    (loop for sub-table across contents
          do (cl:format stream " ~s" sub-table))
    (loop for extra-slot across extra-slots
          do (cl:format stream " ~s" extra-slot))
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
  (setf (cl:get 'el::test-purpose 'el::char-table-extra-slots) 6)
  (let ((ct (make-simple-chartable :purpose 'el::test-purpose :default 10)))
    (setf (aref (chartable-extra-slots ct) 0) 310)
    (setf (aref (chartable-extra-slots ct) 1) 311)
    (is (string= "#^[10 NIL EL::TEST-PURPOSE 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 310 311 10 10 10 10]"
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

;; mapping functions use EQ in emacs code, we use EQUAL for simplicity
(defgenerator generate-chartable-ranges-without-parents (ct)
  #M"generates single list (start end value), returns all values including nil,
     so there is no holes in ranges"
  (with-slots (contents default) ct
    (let ((current-start 0)
          (current-value nil)
          (element-size (aref +chars-per-element+ 0)))
      (labels ((yield-position (position value &optional force)
                 ;; (log-debug2 "yield position:~s value:~s"
                 ;;             position value)
                 (when (or force (not (equal value current-value)))
                   (log-debug2 "start switch current-start:~s current-value:~s"
                               current-start current-value)
                   (let ((current-end (1- position)))
                     (when  (> (- current-end current-start) 0)
                       (yield (list current-start current-end current-value)))
                     (setq current-start position)
                     (setq current-value value))))
               (process-sub-chartable (sub-ct)
                 (with-slots (contents depth min-char) sub-ct
                   (let ((element-size (aref +chars-per-element+ depth)))
                     (loop for sub-sub-ct across contents
                           for sub-from from min-char by element-size
                           for sub-to = (1- (+ sub-from element-size))
                           do (if (sub-chartable-p sub-sub-ct)
                                  (process-sub-chartable sub-sub-ct)
                                  (yield-position sub-to sub-sub-ct)))))))
        (loop for sub-ct across contents
              for sub-from from 0 by element-size
              for sub-to = (1- (+ sub-from element-size))
              do (if (sub-chartable-p sub-ct)
                     (process-sub-chartable sub-ct)
                     (yield-position sub-from sub-ct))
              finally (yield-position (1+ sub-to) nil t))))))

(defgenerator generate-chartable-ranges (ct)
  #M"return ranges in form '(start end value) including
     data from parent table hierarchy"
  ;; code makes generators for all chartable parent tree
  ;; and then does tricky parallel scrolling through all generators

  ;; table with index 0 is the lowest child
  (let* ((tables (loop with tables = (list ct)
                       for parent = (chartable-parent (car tables))
                       while parent
                       do (push parent tables)
                       finally (return (nreverse tables))))
         ;; list of activated range generators
         (generators (loop for ct in tables
                           collect (generate-chartable-ranges-without-parents ct)))
         ;; array of last generated positions (in 3-element lists
         (positions (coerce
                     (loop for gen in generators
                           collect (funcall gen))
                     'cl:vector))
         )
    (let ((current-start -1)
          (current-value nil)
          (last-max-position -1))
      (labels ((yield-position (position value &optional force)
                 (log-debug2 "yield position:~s value:~s"
                             position value)
                 (when (or force (not (equal value current-value)))
                   (log-debug2 "start switch current-start:~s current-value:~s"
                               current-start current-value)
                   (let ((current-end (1- position)))
                     (when (and (> (- current-end current-start) 0) current-value)
                       ;; (log-debug "yield ~s" (list current-start current-end current-value))
                       (yield (list current-start current-end current-value))
                       )
                     (setq current-start position)
                     (setq current-value value)))))
        (loop for minimal-position = nil
              do ;; define minimal position across all tables
                 (loop for position across positions
                       do (unless (eq position 'generator-stop)
                            (when (and
                                   (> (car position) last-max-position)
                                   (or (null minimal-position)
                                       (<= (car position) minimal-position)))
                              (setq minimal-position (car position)))))
                 (log-debug2 "last-max-position ~s" last-max-position)
                 (log-debug2 "minimal position ~s ~s" positions minimal-position)
                 (unless minimal-position
                   ;; no more positions from generators, end main loop
                   (yield-position (1+ last-max-position) nil)
                   (return))

                 ;; define first non-nullvalue for minimal-position
                 ;; according to hierarchy
                 (loop
                   with minimal-value = nil
                   for position across positions
                   do (unless (eq position 'generator-stop)
                        (when (and (<= (car position) minimal-position (nth 1 position))
                                   (nth 2 position))
                          (setq minimal-value (nth 2 position))
                          (yield-position minimal-position minimal-value)
                          (return))))

                 ;; define position intervals with smallest end,  and rotate it,
                 ;; also set new last-max-position to move forward
                 (setq last-max-position nil)
                 (loop for position across positions
                       do (unless (eq position 'generator-stop)
                            (when (or (null last-max-position)
                                      (<= (nth 1 position) last-max-position))
                              (setq last-max-position (nth 1 position)))))
                 (loop for gen in generators
                       for position across positions
                       for idx from 0
                       do (unless (eq position 'generator-stop)
                            (when (= last-max-position (nth 1 position))
                              (log-debug2 "rotating position in table ~s" idx)
                              (setf (aref positions idx) (funcall gen)))))
                 ;; we strongly believe here that the next interval will start without hole
                 (log-debug2 "new last-max-position ~s" last-max-position)
                 (log-debug2 "new-positions ~s" positions)
              )))))

(test test-generate-ranges
  (let* ((ct1 (make-simple-chartable :purpose 'test :default 1))
         (ct2 (make-simple-chartable :purpose 'test :parent ct1))
         (ct3 (make-simple-chartable :purpose 'test :parent ct2))
         (ct4 (make-simple-chartable :purpose 'test :parent ct2 :default 4)))
    (set-chartable-range ct2 4 15000 2)
    (set-chartable-range ct3 10 15 3)
    (set-chartable-range ct4 10 15 3)

    (is (equal '((0 4194303 1))
               (generator->list (generate-chartable-ranges ct1))))
    (is (equal '((0 3 NIL) (4 15000 2) (15001 4194303 NIL))
               (generator->list (generate-chartable-ranges-without-parents ct2))))
    (is (equal '((0 3 1) (4 15000 2) (15001 4194303 1))
               (generator->list (generate-chartable-ranges ct2))))
    (is (equal '((0 9 4)
                 (10 15 3)
                 (16 4194303 4))
               (generator->list (generate-chartable-ranges ct4))))
    (is (equal '((0 3 1)
                 (4 9 2)
                 (10 15 3)
                 (16 15000 2)
                 (15001 4194303 1))
               (generator->list (generate-chartable-ranges ct3))))))

(defun test-me ()
  (run! 'cl-emacs/types/chartables))
