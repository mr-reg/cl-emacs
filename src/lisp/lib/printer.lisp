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

(uiop:define-package :cl-emacs/lib/printer
    (:use
     :defstar
     :common-lisp
     :cl-emacs/lib/log
     :cl-emacs/lib/errors
     :cl-emacs/lib/reader-utils
     :cl-emacs/lib/elisp-packages
     :fiveam
     :cl-emacs/lib/commons)
  (:import-from #:serapeum
                #:memq)
  (:local-nicknames (#:el #:cl-emacs/elisp)
                    (#:pstrings #:cl-emacs/types/pstrings)
                    (#:chartables #:cl-emacs/types/chartables)
                    )
  (:export #:princ-to-cl-stream
           #:prin1-to-cl-stream)
  )

(in-package :cl-emacs/lib/printer)
;; (log-enable :cl-emacs/lib/printer :debug2)
(log-enable :cl-emacs/lib/printer :info)
(def-suite cl-emacs/lib/printer)
(in-suite cl-emacs/lib/printer)
(named-readtables:in-readtable elisp-function-syntax)

(defun check-if-obj-in-circle-refs (obj circle-refs)
  "return nil if obj not in circle refs, otherwise - number"
  (cl:loop for ref in circle-refs
     for ref-counter from 0
     do (log-debug2 "rc:~s ref:~s" ref-counter ref)
        (when (eq ref obj)
          (return-from check-if-obj-in-circle-refs ref-counter))
     )
  nil)

(defun safe-print-to-cl-stream (obj stream raw-mode backquoted circle-refs &optional circle-num)
  (if circle-num
      (progn
        (cl:write-char #\# stream)
        (cl:princ circle-num stream))
      (print-to-cl-stream obj stream raw-mode backquoted circle-refs)))

(defun* print-to-cl-stream (obj stream raw-mode backquoted circle-refs)
  #M"if raw-mode = t, then princ, else prin1,"
  (log-debug2 "print-to-cl-stream obj:~s circle-refs:~s" obj circle-refs)
  (push obj circle-refs)
  (cond
    ((consp obj)
     (log-debug2 "consp")
     (when (and (consp (cdr obj)) (null (cdr (cdr obj))))
       (log-debug2 "special form ~s ~s" (cdr obj) (cdr (cdr obj)))
       ;; it is special form: list of exactly two elements, where first element may be some special symbol
       (let ((processed t))
         (cond
           ((eq (car obj) 'el::function)
            (cl:write-sequence "#'" stream)
            (print-to-cl-stream (car (cdr obj)) stream raw-mode backquoted circle-refs)
            )
           ((eq (car obj) 'el::quote)
            (cl:write-char #\' stream)
            (print-to-cl-stream (car (cdr obj)) stream raw-mode backquoted circle-refs))
           ((eq (car obj) 'el::|`|)
            (cl:write-char #\` stream)
            (print-to-cl-stream (car (cdr obj)) stream raw-mode (1+ backquoted) circle-refs)
            )
           ((and (memq (car obj) '(el::|,| el::|,@|))
                 (> backquoted 0))
            (print-to-cl-stream (@symbol-name (car obj)) stream t (1- backquoted) circle-refs)
            (print-to-cl-stream (car (cdr obj)) stream raw-mode (1- backquoted) circle-refs)
            )
           (t (setq processed nil))
           )
         (when processed
           (return-from print-to-cl-stream)))
       )
     (cl:write-char #\( stream)
     (loop with first = t
           while (consp obj)
           do (if first (setq first nil)
                  (cl:write-char #\space stream))
              (let* ((to-print (car obj))
                     (circle-num (check-if-obj-in-circle-refs to-print circle-refs)))
                (safe-print-to-cl-stream (car obj) stream raw-mode backquoted circle-refs circle-num))
              (setq obj (cdr obj))
              (let ((circle-num (check-if-obj-in-circle-refs obj circle-refs)))
                (when (or circle-num (and obj (not (consp obj))))
                  (cl:write-sequence " . " stream)
                  (safe-print-to-cl-stream obj stream raw-mode backquoted circle-refs circle-num)
                  (when circle-num
                    (return)))))
     (cl:write-char #\) stream))
    ((or (symbolp obj) (null obj))
     (let ((symbol-name (@symbol-name obj)))
       (when (pstrings:emptyp symbol-name)
         (cl:write-sequence "##" stream))
       (if raw-mode
           (pstrings:write-pstring-to-cl-stream symbol-name stream :escaped nil)
           (pstrings:write-pstring-to-cl-stream symbol-name stream :escaped :symbol))))
    ((pstrings:pstring-p obj)
     (if raw-mode
         (pstrings:write-pstring-to-cl-stream obj stream :escaped nil)
         (pstrings:write-pstring-to-cl-stream obj stream :escaped :string)
         ))
    ((and (vectorp obj) (eq (array-element-type obj) 'cl:bit))
     (cl:format stream "#&~d" (cl:length obj))
     (pstrings:write-pstring-to-cl-stream
      (pstrings:build-pstring
       (with-output-to-string (stream)
         (loop for bit across obj
               with code = 0
               with nbits = 0
               do (setf code (logior code (ash bit nbits)))
                  (incf nbits)
                  (when (= nbits 8)
                    (cl:write-char (safe-code-char code) stream)
                    (setq nbits 0)
                    (setq code 0))
               finally (when (> nbits 0)
                         (cl:write-char (safe-code-char code) stream))
               )))
      stream :escaped :string)
     )
    ((@recordp obj)
     (cl:write-sequence "#s(" stream)
     (loop for el across obj
           with first = t
           do (unless first
                (cl:write-char #\space stream))

              (let* ((circle-num (check-if-obj-in-circle-refs el circle-refs)))
                (safe-print-to-cl-stream el stream raw-mode backquoted circle-refs circle-num))
              (setq first nil))
     (cl:write-char #\) stream))
    ((vectorp obj)
     (cl:write-char #\[ stream)
     (loop for el across obj
           with first = t
           do (unless first
                (cl:write-char #\space stream))

              (let* ((circle-num (check-if-obj-in-circle-refs el circle-refs)))
                (safe-print-to-cl-stream el stream raw-mode backquoted circle-refs circle-num))
              (setq first nil))
     (cl:write-char #\] stream))
    ((floatp obj)
     (cond
       ((and (float-features:float-infinity-p obj) (> obj 0))
        (cl:write-sequence "1.0e+INF" stream))
       ((and (float-features:float-infinity-p obj) (< obj 0))
        (cl:write-sequence "-1.0e+INF" stream))
       ((@isnan obj)
        (cl:write-sequence "0.0e+NaN" stream))
       (t (if el::float-output-format
              (cl:format stream el::float-output-format obj)
              (cl:format stream "~f" obj))
          )))
    ((numberp obj)
     (cl:princ obj stream)
     )
    ((hash-table-p obj)
     (cl:write-sequence "#s(hash-table size " stream)
     (print-to-cl-stream (hash-table-size obj) stream raw-mode backquoted circle-refs)
     (cl:write-sequence " test " stream)
     (print-to-cl-stream (hash-table-test obj) stream raw-mode backquoted circle-refs)
     (cl:write-sequence " rehash-size " stream)
     (print-to-cl-stream (hash-table-rehash-size obj) stream raw-mode backquoted circle-refs)
     (cl:write-sequence " rehash-threshold " stream)
     (print-to-cl-stream (hash-table-rehash-threshold obj) stream raw-mode backquoted circle-refs)
     (cl:write-sequence " data (" stream)
     (let ((first t))
       (maphash
        #'(lambda (key value)
            (unless first
              (cl:write-char #\space stream))
            (setq first nil)
            (print-to-cl-stream key stream raw-mode backquoted circle-refs)
            (cl:write-char #\space stream)
            (print-to-cl-stream value stream raw-mode backquoted circle-refs)
            ) obj))
     (cl:write-sequence "))" stream)
     )
    ((chartables:chartable-p obj)
     (cl:write-sequence "#^[" stream)
     (print-to-cl-stream (chartables:chartable-default obj) stream raw-mode backquoted circle-refs)
     (cl:write-char #\space stream)
     (print-to-cl-stream (chartables:chartable-parent obj) stream raw-mode backquoted circle-refs)
     (cl:write-char #\space stream)
     (print-to-cl-stream (chartables:chartable-purpose obj) stream raw-mode backquoted circle-refs)
     (cl:write-char #\space stream)
     (print-to-cl-stream (chartables:chartable-ascii obj) stream raw-mode backquoted circle-refs)
     (loop for sub-table across (chartables:chartable-contents obj)
           do (cl:write-char #\space stream)
              (print-to-cl-stream sub-table stream raw-mode backquoted circle-refs))
     (loop for extra-slot across (chartables:chartable-extra-slots obj)
           do (cl:write-char #\space stream)
              (print-to-cl-stream extra-slot stream raw-mode backquoted circle-refs))
     (cl:write-char #\] stream))
    ((chartables:sub-chartable-p obj)
     (when (= (chartables:sub-chartable-depth obj) 3)
       (cl:write-char #\newline stream))
     (cl:write-sequence "#^^[" stream)
     (print-to-cl-stream (chartables:sub-chartable-depth obj) stream raw-mode backquoted circle-refs)
     (cl:write-char #\space stream)
     (print-to-cl-stream (chartables:sub-chartable-min-char obj) stream raw-mode backquoted circle-refs)
     (loop for sub-table across (chartables:sub-chartable-contents obj)
           do (cl:write-char #\space stream)
              (print-to-cl-stream sub-table stream raw-mode backquoted circle-refs))
     (cl:write-char #\] stream))
    (t (error 'unimplemented-error :details
              (cl:format nil "unsupported object type ~s" (cl:type-of obj)))))
  )
(defun* princ-to-cl-stream (obj stream)
  (print-to-cl-stream obj stream t 0 nil))
(defun* prin1-to-cl-stream (obj stream)
  (print-to-cl-stream obj stream nil 0 nil))

(defun* (princ-to-cl-string -> string) (obj)
  (with-output-to-string (stream)
    (princ-to-cl-stream obj stream)))
(defun* (prin1-to-cl-string -> string) (obj)
  (with-output-to-string (stream)
    (prin1-to-cl-stream obj stream)))

(defmacro prin-test (princ prin1 &optional (obj :skip))
  #M"if 2 arguments passed, it means that print of second argument will produce first one in both cases
     and it is the most common case

     if results should be different, you should use 3 arguments"
  (if (eq obj :skip)
      `(progn (is (string= ,princ (princ-to-cl-string ,prin1)))
              (is (string= ,princ (prin1-to-cl-string ,prin1))))
      `(progn (is (string= ,princ (princ-to-cl-string ,obj)))
              (is (string= ,prin1 (prin1-to-cl-string ,obj))))))

(test test-print-symbol ()
  (prin-test "##" 'el::||)
  (prin-test "test" 'el::test)
  (prin-test ":test" 'el::\:test)
  (prin-test "ab_~!@$%^&:<>{}?c" 'el::|AB__~!@$%^&:<>{}?C|)
  (prin-test "AbCd" 'el::_AB_CD)
  (prin-test "nil" 'el::nil)
  (prin-test "abc_, [/]'`" "abc_\\,\\ \\[/\\]\\'\\`" 'el::|ABC__, [/]'`|)
  (prin-test "non-intern-symbol" (cl:make-symbol "non-intern-symbol"))
  (prin-test " !\"#$%&'()*+,-./09:;<=>?@az[\\]^_`az{|}~"
      "\\\\\\\\\\\\\\\\ !\\\"\\#$%&\\'\\(\\)*+\\,-./09:\\;<=>?@az\\[\\\\\]^_\\`az{|}~"
      (cl:make-symbol " !\"#$%&'()*+,-./09:;<=>?@AZ[\\]^__`az{|}~"))
  )

(test test-print-numbers
  (prin-test "12" 12)
  (prin-test "-1" -1)
  (prin-test "-1.0" -1.0)
  (prin-test "0.0" 0.0)
  (prin-test "0.0e+NaN" (/ 0.0 0.0))
  (prin-test "1.0e+INF" (/ 1.0 0.0))
  (prin-test "-1.0e+INF" (/ -1.0 0.0))
  (prin-test "100000000.0" 1e8)
  (prin-test "0.0001" 1e-4)
  (let ((el::float-output-format "~,8f"))
    (prin-test "0.00010000" 1e-4))
  )

(test test-print-quote
  (prin-test "'test" (quote (el::quote el::test)))
  (prin-test "`(,a,b (,@nil))" "`(,a\\,b (,@nil))"
      '(el::|`| ((el::|,| el::|a,b|) ((el::|,@| el::nil)))))
  (prin-test "'((, a,b) ((,@ nil)))" "'((\\, a\\,b) ((\\,@ nil)))"
      '(el::quote ((el::|,| el::|a,b|) ((el::|,@| el::nil)))))
  ;; (princ (read "`(a . '_)"))
  (prin-test "`(a quote _)"
      '(el::|`| (el::a . (el::quote el::__))))
  (prin-test "`(quote , _)" "`(quote \\, _)"
      '(el::|`| (el::quote . (el::|,| el::__))))

  (prin-test "(, (, a))" "(\\, (\\, a))"
      '(el::|,| (el::|,| el::a)))
  (prin-test "`,(, a)" "`,(\\, a)"
      '(el::|`| (el::|,| (el::|,| el::a))))
  (prin-test "``,,a"
      '(el::|`| (el::|`| (el::|,| (el::|,| el::a)))))
  )

(test test-print-string
  (prin-test " " "\" \"" (pstrings:build-pstring " "))
  (prin-test "test" "\"test\"" (pstrings:build-pstring "test"))
  (prin-test "test" "\"test\"" (pstrings:build-pstring "test"))
  (prin-test #M"multiline
                string"
      #M"\"multiline
                string\""
      (pstrings:build-pstring #M"multiline
                                        string"))
  (prin-test " " "#(\" \" 0 1 (invisible t))"
      (pstrings:build-pstring " " '((el::invisible . t))))
  (prin-test (cl:format nil "~c, \"\"~c" #\tab #\soh)
      (cl:format nil "\"~c, \\\"\\\"~c\"" #\tab #\soh)
      (pstrings:build-pstring (cl:format nil "~c, \"\"~c" #\tab #\soh)))
  (prin-test " !\"#$%&'()*+,-./09:;<=>?@AZ[\\]^_`az{|}~"
      "\" !\\\"#$%&'()*+,-./09:;<=>?@AZ[\\\\]^_`az{|}~\""
      (pstrings:build-pstring " !\"#$%&'()*+,-./09:;<=>?@AZ[\\]^_`az{|}~"))
  (prin-test "abc" "#(\"abc\" 0 3 (\"ab\\\"c\" ab\\ c))"
      (pstrings:build-pstring
       "abc"
       (list
        (cons (pstrings:build-pstring "ab\"c") 'el::|AB C|))))

  (prin-test "\\235\\353" "\"\\235\\353\""
      (pstrings:build-pstring (cl:format nil "~c~c" (safe-code-char #o235) (safe-code-char #o353))))
  (prin-test "\\235ëЖ" "\"\\235ëЖ\""
      (pstrings:build-pstring (cl:format nil "~c~cЖ" (safe-code-char #o235) (safe-code-char #o353))))

  (let ((pstr (pstrings:build-pstring " ")))
    (setf (pstrings:pstring-multibyte pstr) t)
    (princ-to-cl-string pstr)
    (prin-test " " "\" \"" pstr))

  ;; emacs supports this, but we do not:
  ;; it makes no sense, because strings are for characters, not for any hex numbers.
  ;; To properly support this, we need convert pstrings to numeric/fixnum arrays,
  ;; which will take a lot of memory in future
  ;; (prin-test "\x303000" "\"\x303000\"" "\x303000")

  )
(test test-print-lists
  (prin-test "(a b)" '(el::a el::b))
  (prin-test "(a . b)" '(el::a . el::b))
  (prin-test "(a)" '(el::a . nil))
  (prin-test "(function . test)" '(el::function . el::test))
  (prin-test "(function test a)" '(el::function el::test el::a))
  (prin-test "(function)" '(el::function))

  )

(test test-print-vectors
  (prin-test "[a b]" "[a b]" #(el::a el::b))
  (prin-test "[]" "[]" #())
  )

(test test-print-functions
  (prin-test "#'test" "#'test" '(el::function el::test))
  (prin-test "#'nil" "#'nil" '(el::function nil))
  )

(test test-print-bool-vector
  (prin-test "#&0\"\"" #*)
  (prin-test "#&8\"z\"" #*01011110)
  (prin-test "#&8\"\\377\"" #*11111111)
  (prin-test "#&14\"z#\"" #*01011110110001)
  )

(test test-print-hash-table
  (let ((hash (@make-hash-table :test 'eq)))
    (@puthash 'el::a (pstrings:build-pstring "2") hash)
    (@puthash 'el::b 3 hash)
    (prin-test
        "#s(hash-table size 65 test eq rehash-size 1.5 rehash-threshold 0.8125 data (a 2 b 3))"
        "#s(hash-table size 65 test eq rehash-size 1.5 rehash-threshold 0.8125 data (a \"2\" b 3))"
        hash)))

(test test-print-chartable
  (let ((ct (chartables:make-simple-chartable :purpose 'purpose :default nil)))
    (chartables:set-chartable-range ct 4 15000 2)
    (chartables:set-chartable-range ct 10 15 (pstrings:build-pstring "3"))
    (prin-test
        (cl:format nil "~a~%~a~%~a~%~a"
                   "#^[nil nil purpose "
                   "#^^[3 0 nil nil nil nil 2 2 2 2 2 2 3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2] #^^[1 0 #^^[2 0 "
                   "#^^[3 0 nil nil nil nil 2 2 2 2 2 2 3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2] 2 2 #^^[2 12288 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 "
                   "#^^[3 14976 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] nil nil nil nil nil nil nil nil nil nil] nil nil nil nil nil nil nil nil nil nil nil nil] nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil]"
                   )
        (cl:format nil "~a~%~a~%~a~%~a"
                   "#^[nil nil purpose "
                   "#^^[3 0 nil nil nil nil 2 2 2 2 2 2 \"3\" \"3\" \"3\" \"3\" \"3\" \"3\" 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2] #^^[1 0 #^^[2 0 "
                   "#^^[3 0 nil nil nil nil 2 2 2 2 2 2 \"3\" \"3\" \"3\" \"3\" \"3\" \"3\" 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2] 2 2 #^^[2 12288 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 "
                   "#^^[3 14976 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] nil nil nil nil nil nil nil nil nil nil] nil nil nil nil nil nil nil nil nil nil nil nil] nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil]"
                   )
        ct))
  )

(test test-print-circle
  ;;; common lisp reader is not stable when we are messing with so deep circles
  ;;; so we should construct everything programmatically....
  (let ((val (make-array 2)))
    (setf (aref val 0) 'el::a)
    (setf (aref val 1) val)
    (prin-test "[a #0]" val))

  (let ((val (make-array 3)))
    (setf (aref val 0) 'el::a)
    (setf (aref val 1) val)
    (setf (aref val 2) 'el::b)
    (prin-test "[a #0 b]" val))

  (let* ((ref nil)
         (val (cons 'el::a ref)))
    (setf (cdr val) val)
    (prin-test "(a . #0)" val))

  (let* ((ref nil)
         (val `(el::a ,ref el::b . ,ref)))
    (setf (nth 1 val) val)
    (setf (cdr (cl:nthcdr 2 val)) val)
    (prin-test "(a #0 b . #0)" val)
    )

  (let ((val (make-array 4)))
    (setf (aref val 0) 'el::a)
    (setf (aref val 1) val)
    (setf (aref val 2) 'el::b)
    (setf (aref val 3) val)
    (prin-test "[a #0 b #0]" val))

  )

(test test-print-record
  (let ((record #(el::test-rec el::abc 123 (1 2 3))))
    (setf (cl:get 'el::test-rec'el::type) 'el::record)
    (prin-test "#s(test-rec abc 123 (1 2 3))" record)))


(defun test-me ()
  (run! 'cl-emacs/lib/printer))
;; (loop for code from 1 to 127
;;       do (cl:format t "~c" (code-char code)))
