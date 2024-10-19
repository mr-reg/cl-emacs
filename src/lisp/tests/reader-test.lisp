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

(cl-emacs/lib/elisp-packages:define-elisp-test-package :cl-emacs/tests/reader-test
    (:use cl-emacs/lib/reader)
  )
(in-package :cl-emacs/tests/reader-test)
(log-enable :cl-emacs/tests/reader-test :debug2)
(def-suite cl-emacs/tests/reader-test)
(in-suite cl-emacs/tests/reader-test)
(named-readtables:in-readtable elisp-function-syntax)

(test test-read-symbols
  (is (@equal (cl:quote el::||)
              (read-simple "##")))
  (is (@equal (cl:quote (el::test . 4))
              (read-cl-string "test")))
  (is (@equal (cl:quote (el::test . 5))
              (read-cl-string "test two")))
  (is (@equal (cl:quote (el::\:test))
              (read-simple "(:test))")))
  (is (@equal (cl:quote el::|AB123-+=*/C|)
              (read-simple "ab123-+=*/c")))
  (is (@equal (cl:quote el::|AB__~!@$%^&:<>{}?C|)
              (read-simple "ab_~!@$%^&:<>{}?c")))
  (is-false (@equal (read-simple "FOO")
                    (read-simple "foo")))
  (is (@equal (cl:quote el::_AB_CD)
              (read-simple "AbCd")))
  (is (@equal (cl:quote el::nil)
              (read-simple "nil")))
  (is (@equal (cl:quote el::|ABC, [/]'`|)
              (read-simple "a\\b\\c\\,\\ \\[\\/\\]\\'\\`")))
  (is (@equal (cl:quote el::\,)
              (read-simple "\\,")))
  (is (@equal (cl:quote el::+1)
              (read-simple "\\+1")))
  (is (@equal (pstrings:build-pstring "non-intern-symbol")
              (@symbol-name (read-simple "#:non-intern-symbol"))))
  (is-false (cl:find-symbol "non-intern-symbol" :el))
  (is (@equal (pstrings:build-pstring "+1")
              (@symbol-name (read-simple "#:+1"))))
  (is (@equal (pstrings:build-pstring "")
              (@symbol-name (read-simple "#:,"))))
  (is (@equal 'el::test
              (read-simple "#_test")))
  (is (@equal (cl:quote el::||)
              (read-simple "#_")))
  (is (@equal (cl:quote el::|-|)
              (read-simple "-")))
  (is (@equal (cl:quote (el::quote el::_na_n))
              (read-simple "'NaN")))
  (is (@equal (cl:quote (el::a el::b))
              (read-simple (cl:format cl:nil "(a~cb)" #\return))))
  (is (@equal (cl:quote (el::a el::b))
              (read-simple (cl:format cl:nil "(a~cb ~c)" #\tab #\tab))))
  (is (@equal (cl:quote (el::a el::b))
              (read-simple (cl:format cl:nil "(a~cb ~c)" #\dc4 #\dc4))))
  (is (@equal (cl:quote (el::a el::b el::nil))
              (read-simple "(a b #$)")))
  )

(test test-read-shorthands
  ;; TODO: add support for shorthands in reader
  ;; (let ((read-symbol-shorthands
  ;;         '(("s-" . "shorthand-longhand-"))))
  ;; (read "#_s-test")
  ;;   (read "#_s-test"))
  )

(test test-read-numbers
  (is (@= 1 (read-simple "1")))
  (is (@= 1 (read-simple "1.")))
  (is (@= 1 (read-simple "+1")))
  (is (@= -1 (read-simple "-1")))
  (is (@= 0 (read-simple "+0")))
  (is (@= 0 (read-simple "-0")))
  (is (@= 0 (read-simple "-0.0")))

  (is (@= 1500 (read-simple "1500.0")))
  (is (@= 1500 (read-simple "+15E2")))
  (is (@= 1600 (read-simple "+160000e-2")))
  (is (@= 1500 (read-simple "15.0e+2")))
  (is (@= 12.34d0 (read-simple "1.234e+01")))
  (is (@= 1500 (read-simple ".15e4")))
  (is (@equal @*nan* (read-simple "0.0e+NaN")))
  (is (@equal @*nan* (read-simple "-4.5E+NaN")))
  (is (@equal @*positive-infinity* (read-simple "1.0e+INF")))
  (is (@equal @*negative-infinity* (read-simple "-4.0e+INF")))
  (is (@= 425.19685d0 (read-simple "425.19685")))
  ;;; does not work for Android SBCL:
  ;; (is (@equal @*positive-infinity* (read-simple "1.0e+309")))
  ;; (is (@equal @*negative-infinity* (read-simple "-4.0e+309")))
  )
(test test-read-quotes
  (is (@equal (cl:quote (el::quote el::test-symbol))
              (read-simple "'test-symbol")))
  (is (@equal (cl:quote (el::quote (el::quote el::test-sym)))
              (read-simple "''test-sym")))
  (is (@equal (cl:quote (el::quote (el::test-symbol)))
              (read-simple "'(test-symbol)")))
  (is (@equal (cl:quote ((el::quote 3)))
              (read-simple "(' 3)")))
  (signals eof-reader-error (read-cl-string "';; test comment"))
  (is (@equal `(el::quote ,(pstrings:build-pstring "abc ("))
              (read-simple "'\"abc (\"")))
  (is (@equal (cl:quote (el::quote 66))
              (read-simple "'?B")))

  (is (@equal (cl:quote (el::quote
                      (el::quote
                       (1 (el::quote
                           ((el::quote
                             (el::quote
                              (el::quote
                               (2 (el::quote (el::quote (el::quote (el::quote 3)))))))) 4)) 5))))
              (read-simple "''(1 '('''(2 ''''3) 4) 5)")))
  (is (@equal (cl:quote (el::quote (el::a (el::quote el::b))))
              (read-simple "'(a'b)")))
  (is (@equal `(el::quote (,(pstrings:build-pstring "a") (el::quote el::b)))
              (read-simple "'(\"a\"'b)")))
  (is (@equal (cl:quote (el::quote (65 (el::quote el::b))))
              (read-simple "'(?A'b)")))
  (is (@equal (cl:quote (el::\` el::test-symbol))
              (read-simple "`test-symbol")))
  (is (@equal (cl:quote (el::\` (el::test-symbol)))
              (read-simple "`(test-symbol)")))
  (is (@equal (cl:quote (el::\` (el::\` el::test-symbol)))
              (read-simple "``test-symbol")))
  (is (@equal (cl:quote ((el::\` el::test-symbol) (el::\, 3)))
              (read-simple "(`test-symbol ,3)")))
  ;; real emacs test (@equal (cl:quote (\` (form ((\, a) (\, ((\` (b (\, c))))))))) (read-simple "`(form (,a ,(`(b ,c))))"))
  (is (@equal (cl:quote (el::\` (el::form ((el::\, el::a) (el::\, ((el::\` (el::b (el::\, el::c)))))))))
              (read-simple "`(form (,a ,(`(b ,c))))")))
  (signals eof-reader-error (read-cl-string "'"))
  (signals eof-reader-error (read-cl-string "`,"))

  (is (@equal (cl:quote (el::quote el::\,))
              (read-simple "'\\,")))
  (is (@equal (cl:quote (el::\` (el::a (el::|,@| el::b))))
              (read-simple "`(a ,@b)")))
  (is (@equal (cl:quote (el::|,| (el::|,| el::a)))
              (read-simple ",,a")))
  (is (@equal (cl:quote (el::|`| el::|,| el::a))
              (read-simple "(\\` . ,a)")))
  (is (@equal (cl:quote (el::|`| (el::quote (el::|,| (el::and)))))
              (read-simple "`',(and)")))
  )

(test test-read-strings
  (is (@equal `(,(pstrings:build-pstring "test") . 6)
              (read-cl-string "\"test\" ")))
  (is (@equal `(el::defvar el::test-var el::nil
                 ,(pstrings:build-pstring #M"some
                                            multiline docstring `with symbols'."))
              (read-simple #M"(defvar test-var nil \"some
                                     multiline docstring `with symbols'.\")")))
  ;; TODO: add better pstring property validation after more complicated reads
  (is (@equal
       (pstrings:build-pstring " ")
       (read-simple "#(\" \" 0 1 (invisible t))")))
  (is (@equal
       (pstrings:build-pstring "foo bar")
       (read-simple "#(\"foo bar\" 0 3 (face bold) 3 4 nil 4 7 (face italic))")))
  (is (@equal (pstrings:build-pstring "abc\\z\"")
              (read-simple "\"abc\\\\\\x7a\\\"\"" )))
  (is (@equal (pstrings:build-pstring "sample string without newline.")
              (read-simple
               #M"#(\"sampl\\ e \\
                      string without newline.\" 0 4 (face bold))")))
  (is (@equal (pstrings:build-pstring (cl:format cl:nil "~c, \"\"~c" #\tab #\soh))
              (read-simple "\"\\t, \\\"\\\"\\C-a\"")))
  (is (@equal  (pstrings:build-pstring "")
               (read-simple "#(\"\" 0 0 (invisible t))")))
  (is (@equal
       (pstrings:build-pstring "fooA0bar")
       (read-simple "\"foo\\u00410bar\")")))
  ;; (read-cl-string "\"\\x103000\")")
  (is (read-cl-string "\"\\x3FFF7F\""))

  (cl:let ((el::string-multibyte-flag-emacs-compatible el::t))
    (is-false (pstrings:pstring-multibyte (read-simple "\"\\M-k\"")))
    (is (pstrings:pstring-multibyte (read-simple "\"\\±\"")))
    (is-false (pstrings:pstring-multibyte (read-simple "\"\\3757zXZ\\0\"")))
    (is (pstrings:pstring-multibyte (read-simple "\"[1-9][0-9][0-9]\\u2044[0-9]+\"")))
    (is (pstrings:pstring-multibyte (read-simple "\"\\u0080\"")))
    (is-false (pstrings:pstring-multibyte (read-simple "\"\\u007F\"")))
    ;; ( (read-simple "\"[1-9][0-9][0-9]\\u2044[0-9]+\""))
    (is (pstrings:pstring-multibyte (read-simple "\"\\\\(\\u00A0+\\\\)\""))))
  )
(test test-read-lists
  (is (@equal `(el::a ,(pstrings:build-pstring "b"))
              (read-simple "(a\"b\") ")))
  (is (@equal `(el::test 2 3 ,(pstrings:build-pstring "A ( B"))
              (read-simple "(test 2 3 \"A ( B\")")))
  (is (@equal (cl:quote (el::test (2 3) 4 ()))
              (read-simple "(test (2 3) 4 ())")))
  (is (@equal (cl:quote (el::.))
              (read-simple "(.)")))
  (is (@equal (cl:quote (el::a el::.))
              (read-simple "(a .)")))
  (is (@equal (cl:quote (el::a el::.b))
              (read-simple "(a .b)")))
  (is (@equal (cl:quote (3 . 4))
              (read-simple "(3 .  4)")))
  (is (@equal (cl:quote 4)
              (read-simple "(. 4)")))
  (signals invalid-reader-input-error (read-cl-string "(a b . c d)"))
  (is (@equal (cl:quote (65 . 66))
              (read-simple "(?A.?B)")))
  (is (@equal (cl:quote (65 . 66))
              (read-simple "(?A. ?B))")))
  (is (@equal 'el::nil (read-simple "()")))
  (signals eof-reader-error (read-cl-string "("))
  (is (@equal (cl:quote (1 2 3))
              (read-simple "(1 . (2 . (3 . nil)))")))
  (is (@equal (cl:quote (el::quote el::nil))
              (read-simple "'()")))
  )
(test test-read-comments
  (is (@equal 'el::test
              (read-simple #M";;; comment line
                                        test symbol")))
  (signals eof-reader-error (read-cl-string ";; just comment"))
  )
(test test-read-characters
  (is (@equal '(65 66)
              (read-simple "(?A?B))")))
  (is (@equal '(el::a 92 65 66 65 32 . 3)
              (read-simple "(a ?\\\\ ?A?B ?A?\\s. 3)")))
  (is (@equal 32
              (read-simple "?\\ ")))
  (is (@equal #x202a
              (read-simple "?\\x202a")))
  (is (@equal '(#x202a #x202a)
              (read-simple "(?\\x202a ?\\x202a)")))
  (is (@equal #o202
              (read-simple "?\\202")))
  (is (@equal 8206
              (read-simple "?\\N{left-to-right mark}")))
  (is (@equal 123
              (read-simple "?\\{")))
  )

(test test-read-special-cases
  (signals eof-reader-error (read-cl-string ""))
  (is (@equal '(el::quote el::test)
              (read-simple #M" ' #!some-stuff
                                        #!some-stuff
                                        test")))
  (is (@equal '(el::quote (el::|.| el::|.| el::e))
              (read-simple "'(\\. \\. e)")))
  )

(test test-read-vectors
  (is (@equal (cl:quote #(1 el::a))
              (read-simple "[1 a]")))
  (is (@equal (cl:quote #(el::a el::b el::c el::.))
              (read-simple "[a b c .]")))
  (signals invalid-reader-input-error (read-cl-string "[1 . a]"))
  )


(test test-reader-functions
  (is (@equal (cl:quote (el::function el::a))
              (read-simple "#'a")))
  )

(test test-read-circles
  (cl:let ((sample (read-simple "#1=(a #1#)")))
    (is (@eq sample (second sample))))
  (cl:let ((sample (read-simple "#1=[a #1#]")))
    (is (@eq sample (cl:aref sample 1))))
  (cl:let ((sample (read-simple "#1=[#1# a #1#]")))
    (is (@eq sample (cl:aref sample 0)))
    (is (@eq sample (cl:aref sample 2))))
  (cl:let ((sample (read-simple "#1=(a . #1#)")))
    (is (@eq sample (cl:cdr sample))))
  (cl:let ((sample (read-simple "#1=(#2=[#1# #2#] . #1#)")))
    (is (@eq sample (cl:cdr sample)))
    (is (@eq sample (cl:aref (cl:car sample) 0)))
    (is (@eq (cl:car sample) (cl:aref (cl:car sample) 1))))
  (cl:let ((sample (read-simple "#1=(#2=[#1# #2#] . #2#)")))
    (is (@eq sample (cl:aref (cl:car sample) 0)))
    (is (@eq (cl:car sample) (cl:cdr sample)))
    (is (@eq (cl:car sample) (cl:aref (cl:car sample) 1))))
  (cl:let ((sample (read-simple "#1=[#2=(#1# . #2#)]")))
    (is (@eq sample (cl:car (cl:aref sample 0))))
    (is (@eq (cl:aref sample 0) (cl:cdr (cl:aref sample 0)))))
  (cl:let ((sample (read-simple "#1=(#2=[#3=(#1# . #2#) #4=(#3# . #4#)])")))
    (is (@eq sample (cl:car (cl:aref (cl:car sample) 0))))
    (is (@eq (cl:car sample) (cl:cdr (cl:aref (cl:car sample) 0))))
    (is (@eq (cl:aref (cl:car sample) 0) (cl:car (cl:aref (cl:car sample) 1))))
    (is (@eq (cl:aref (cl:car sample) 1) (cl:cdr (cl:aref (cl:car sample) 1)))))
  (is (@equal (cl:quote ((el::quote (el::quote el::a)) (el::quote el::a)))
              (read-simple "('#1='a #1#)")))
  (signals invalid-reader-input-error (read-cl-string "#1=(#1# #2#)"))
  (signals invalid-reader-input-error (read-cl-string "#1=#1#"))

  (is (@equal (cl:quote ((el::a el::b el::b) el::b))
              (read-simple "(#1=(a #1=b #1#) #1#)")))

  (cl:let* ((sample (read-simple "(#1=(a . #1#) #1=(b . #1#))"))
            (first (cl:first sample))
            (second (cl:second sample)))
    (is (@eq first (cl:cdr first)))
    (is (@eq second (cl:cdr second)))
    (is-false (@eq first second)))

  (cl:let* ((sample (read-simple "(#1=(a #1# #1=b #1#) #1#)"))
            (first (cl:first sample))
            (second (cl:second sample)))
    (is (@eq first (cl:second first)))
    (is (@eq 'el::b (nth 2 first)))
    (is (@eq 'el::b (nth 3 first)))
    (is (@eq 'el::b second))
    )

  (cl:let* ((sample (read-simple "#1=(a #1# b #1#)"))
            (first (cl:first sample))
            (second (cl:second sample))
            (fourth (cl:fourth sample)))
    (is (@eq second sample))
    (is (@eq fourth sample))
    )

  )

(test test-read-radix
  (signals invalid-reader-input-error (read-cl-string "#b"))
  (is (@equal (cl:quote #b10)
              (read-simple "#b010")))
  (is (@equal (cl:quote #b10)
              (read-simple "#b+010")))
  (is (@equal (cl:quote #b-10)
              (read-simple "#b-010")))
  (is (@equal (cl:quote (0 -10))
              (read-simple "(#b0-10)")))

  (is (@equal (cl:quote #b1010)
              (read-simple "#B01010")))
  (is (@equal (cl:quote #b-1010)
              (read-simple "#B-01010")))
  (signals invalid-reader-input-error (read-cl-string "#b013"))

  (signals invalid-reader-input-error (read-cl-string "#o"))
  (is (@equal (cl:quote #o10)
              (read-simple "#o010")))
  (is (@equal (cl:quote #o-10)
              (read-simple "#o-010")))
  (is (@equal (cl:quote #o1010)
              (read-simple "#O01010")))
  (signals invalid-reader-input-error (read-cl-string "#o013a"))

  (signals invalid-reader-input-error (read-cl-string "#x"))
  (is (@equal (cl:quote #x010aaf)
              (read-simple "#x010aAF")))
  (is (@equal (cl:quote #x-010aaf)
              (read-simple "#x-010aAF")))
  (is (@equal (cl:quote #x010aaf)
              (read-simple "#X010aAF")))
  (signals invalid-reader-input-error (read-cl-string "#x013aw"))
  (is (@equal (cl:quote (el::test #x123))
              (read-simple "(test #x123)")))
  )

(test test-read-hash-tables
  (cl:let ((ht (read-simple
                #M"#s(hash-table
                     size 1000 test eq
                     rehash-size 1.3
                     rehash-threshold 0.6
                     data (a 2 b 3))")))
    (is (@= 1000 (hash-table-size ht)))
    (is (@eq 'el::eq (hash-table-test ht)))
    (is (@= 1.3 (hash-table-rehash-size ht)))
    (is (@= 0.6 (hash-table-rehash-threshold ht)))
    (is (@= 2 (gethash 'el::a ht)))
    (is (@= 3 (gethash 'el::b ht))))
  (cl:let ((ht (read-simple
                "#s(hash-table bad-parameter some-value test equal data)")))
    (is (@eq 'el::equal (hash-table-test ht)))
    (is (@= 0 (hash-table-count ht)))))


(test test-read-records
  (cl:let ((record (read-simple
                    #M"#s(test-rec abc 123 (1 2 3))")))
    (is (@eq 'el::test-rec (type-of record)))
    (is (@equal record #(el::test-rec el::abc 123 (1 2 3))))


    ))
;; (defun* real-file-test ()
;;   (with-open-file (stream "../emacs/lisp/master.el" :direction :input)
;;     (read stream)
;;     (read stream)))


(test test-read-chartables ()
  (cl:let ((sub-ct (read-simple "#^^[3 12 8 8 8 3 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8]")))
    (is (@= 3 (chartables:sub-chartable-depth sub-ct)))
    (is (@= 12 (chartables:sub-chartable-min-char sub-ct)))
    (is (@= 8 (cl:aref (chartables:sub-chartable-contents sub-ct) 0)))
    (is (@= 3 (cl:aref (chartables:sub-chartable-contents sub-ct) 3)))
    )
  (signals invalid-reader-input-error (read-cl-string "#^^[3 12 8 8 8 3 8]"))
  (cl:let ((ct (read-simple
                #M"#^[8 nil test
                     #^^[3 0 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3]
                     #^^[1 0 #^^[2 0 #^^[3 0 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3]
                     3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3]
                     3 3 3 3 3 3 3 3 3 3 3 3 3 3 3]
                     #^^[1 65536 3 3 #^^[2 73728 3 #^^[3 73856 3 3 3 3 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8]
                     8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8]
                     8 8 8 8 8 8 8 8 8 8 8 8 8]
                     8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8 8
                     slot0 slot1 slot2]"
                )))
    (is (@equal
         '((0 79 8) (80 73859 3) (73860 4194303 8))
         (chartables:get-chartable-ranges ct)))
    (is (@equal '#(el::slot0 el::slot1 el::slot2)
                (chartables:chartable-extra-slots ct)))
    ))

(test test-read-bool-vector ()
  (is (@equal #*1010
              (read-simple "#&4\"\"")))
  (is (@equal #*10
              (read-simple "#&2\"a\"")))
  (is (@equal #*
              (read-simple "#&0\"\"")))
  (is (@equal #*01011110110001
              (read-simple "#&14\"z#\"")))
  )

(test test-read-cl-string
  (read-cl-string "(defcustom double-map '((?\; \"\\346\" \";\")))")
;;;###autoload
  ;; #&8"\0"
  ;; #'test
  ;; #'(lambda (x) (use-package-require-after-load x body))
  ;; #'*
  ;; #'.
  ;; #'gnus-article-jump-to-part
  ;; #+A invalid syntax
  ;; '#'test - function
  ;; #'test - symbol
  ;; #[1 2] - bytecode?
  ;; #\" eof?
  ;; lisp_file_lexically_bound_p supports some lexical env while loading using lexical-binding variable
  ;; #&N bool vector?
  ;; #@number skip. #@00 - skip to eof/eob
  ;; strings:
  ;;  \s \  \\n ?\C-SPC ?\^SPC \C-SPC' and `\^SPC
  ;; #s( record
  ;; #^[ char table
  ;; #^^[ sub-char table
  ;; #[ byte code
  ;; #$ current file name
  ;; #NrDIGITS -- radix-N number, any radix 0-36, r or R
  ;; obarrays?
  )

(defun test-me ()
  (run! 'cl-emacs/tests/reader-test))
