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

(cl-emacs/lib/elisp-packages:define-elisp-package :cl-emacs/lib/printer
    (:use
     :defstar
     :cl-emacs/alloc
     :cl-emacs/data
     :cl-emacs/eval
     :cl-emacs/fns
     :cl-emacs/lib/log
     :cl-emacs/lib/reader-utils
     :snakes
     :fiveam
     :cl-emacs/lib/commons)
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
(named-readtables:in-readtable mstrings:mstring-syntax)

(defstruct printer

  )

(defun* print-to-cl-stream (printer obj stream raw-mode backquoted)
  #M"if raw-mode = t, then princ, else prin1"
  (log-debug2 "print-to-cl-stream ~s" obj)
  (cond
    ((consp obj)
     (log-debug2 "consp")
     (when (and (consp (cdr obj)) (null (cdr (cdr obj))))
       (log-debug2 "special form ~s ~s" (cdr obj) (cdr (cdr obj)))
       ;; it is special form: list of exactly two elements, where first element may be some special symbol
       (let ((processed t))
         (cond
           ((eq (car obj) 'el::function)
            (write-sequence "#'" stream)
            (print-to-cl-stream printer (car (cdr obj)) stream raw-mode backquoted)
            )
           ((eq (car obj) 'el::quote)
            (write-char #\' stream)
            (print-to-cl-stream printer (car (cdr obj)) stream raw-mode backquoted))
           ((eq (car obj) 'el::|`|)
            (write-char #\` stream)
            (print-to-cl-stream printer (car (cdr obj)) stream raw-mode (1+ backquoted))
            )
           ((and (memq (car obj) '(el::|,| el::|,@|))
                 (> backquoted 0))
            (print-to-cl-stream printer (symbol-name (car obj)) stream t (1- backquoted))
            (print-to-cl-stream printer (car (cdr obj)) stream raw-mode (1- backquoted))
            )
           (t (setq processed nil))
           )
         (when processed
           (return-from print-to-cl-stream)))
       )
     (write-char #\( stream)
     (loop with first = t
           while (consp obj)
           do (if first (setq first nil)
                  (write-char #\space stream))
              (print-to-cl-stream printer (car obj) stream raw-mode backquoted)
              (setq obj (cdr obj))
              (when (and obj (not (consp obj)))
                (write-sequence " . " stream)
                (print-to-cl-stream printer obj stream raw-mode backquoted)))
     (write-char #\) stream))
    ((or (symbolp obj) (null obj))
     (let ((symbol-name (symbol-name obj)))
       (when (pstrings:emptyp symbol-name)
         (write-sequence "##" stream))
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
                    (write-char (safe-code-char code) stream)
                    (setq nbits 0)
                    (setq code 0))
               finally (when (> nbits 0)
                         (write-char (safe-code-char code) stream))
               )))
      stream :escaped :string)
     )
    ((vectorp obj)
     (write-char #\[ stream)
     (loop for el across obj
           with first = t
           do (unless first
                (write-char #\space stream))
              (print-to-cl-stream printer el stream raw-mode backquoted)
              (setq first nil))
     (write-char #\] stream))
    ((floatp obj)
     (cond
       ((and (float-features:float-infinity-p obj) (> obj 0))
        (write-sequence "1.0e+INF" stream))
       ((and (float-features:float-infinity-p obj) (< obj 0))
        (write-sequence "-1.0e+INF" stream))
       ((isnan obj)
        (write-sequence "0.0e+NaN" stream))
       (t (if el::float-output-format
              (cl:format stream el::float-output-format obj)
              (cl:format stream "~f" obj))
          )))
    ((numberp obj)
     (cl:princ obj stream)
     )
    ((hash-table-p obj)
     (write-sequence "#s(hash-table size " stream)
     (print-to-cl-stream printer (hash-table-size obj) stream raw-mode backquoted)
     (write-sequence " test " stream)
     (print-to-cl-stream printer (hash-table-test obj) stream raw-mode backquoted)
     (write-sequence " rehash-size " stream)
     (print-to-cl-stream printer (hash-table-rehash-size obj) stream raw-mode backquoted)
     (write-sequence " rehash-threshold " stream)
     (print-to-cl-stream printer (hash-table-rehash-threshold obj) stream raw-mode backquoted)
     (write-sequence " data (" stream)
     (let ((first t))
       (maphash
        #'(lambda (key value)
            (unless first
              (write-char #\space stream))
            (setq first nil)
            (print-to-cl-stream printer key stream raw-mode backquoted)
            (write-char #\space stream)
            (print-to-cl-stream printer value stream raw-mode backquoted)
            ) obj))
     (write-sequence "))" stream)
     )
    ((chartables:chartable-p obj)
     (write-sequence "#^[" stream)
     (print-to-cl-stream printer (chartables:chartable-default obj) stream raw-mode backquoted)
     (write-char #\space stream)
     (print-to-cl-stream printer (chartables:chartable-parent obj) stream raw-mode backquoted)
     (write-char #\space stream)
     (print-to-cl-stream printer (chartables:chartable-purpose obj) stream raw-mode backquoted)
     (write-char #\space stream)
     (print-to-cl-stream printer (chartables:chartable-ascii obj) stream raw-mode backquoted)
     (loop for sub-table across (chartables:chartable-contents obj)
           do (write-char #\space stream)
              (print-to-cl-stream printer sub-table stream raw-mode backquoted))
     (loop for extra-slot across (chartables:chartable-extra-slots obj)
           do (write-char #\space stream)
              (print-to-cl-stream printer extra-slot stream raw-mode backquoted))
     (write-char #\] stream))
    ((chartables:sub-chartable-p obj)
     (when (= (chartables:sub-chartable-depth obj) 3)
       (write-char #\newline stream))
     (write-sequence "#^^[" stream)
     (print-to-cl-stream printer (chartables:sub-chartable-depth obj) stream raw-mode backquoted)
     (write-char #\space stream)
     (print-to-cl-stream printer (chartables:sub-chartable-min-char obj) stream raw-mode backquoted)
     (loop for sub-table across (chartables:sub-chartable-contents obj)
           do (write-char #\space stream)
              (print-to-cl-stream printer sub-table stream raw-mode backquoted))
     (write-char #\] stream))
    (t (error 'unimplemented-error :details
              (cl:format nil "unsupported object type ~s" (cl:type-of obj)))))
  )
(defun* princ-to-cl-stream (obj stream)
  (let ((printer (make-printer)))
    (print-to-cl-stream printer obj stream t 0)))
(defun* prin1-to-cl-stream (obj stream)
  (let ((printer (make-printer)))
    (print-to-cl-stream printer obj stream nil 0)))

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
  (let ((hash (make-hash-table :test 'eq)))
    (puthash 'el::a (pstrings:build-pstring "2") hash)
    (puthash 'el::b 3 hash)
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

(defun test-me ()
  (run! 'cl-emacs/lib/printer))
;; (loop for code from 1 to 127
;;       do (cl:format t "~c" (code-char code)))
