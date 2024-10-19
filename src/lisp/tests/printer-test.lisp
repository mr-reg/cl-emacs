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

(cl-emacs/lib/elisp-packages:define-elisp-test-package :cl-emacs/tests/printer-test
    (:use :cl-emacs/lib/printer)
  )
(in-package :cl-emacs/tests/printer-test)
(log-enable :cl-emacs/tests/printer-test :debug2)
(def-suite cl-emacs/tests/printer-test)
(in-suite cl-emacs/tests/printer-test)
(named-readtables:in-readtable elisp-function-syntax)

(cl:defmacro prin-test (princ prin1 cl:&optional (obj :skip))
  #M"if 2 arguments passed, it means that print of second argument will produce first one in both cases
     and it is the most common case

     if results should be different, you should use 3 arguments"
  (cl:if (cl:eq obj :skip)
         `(cl:progn (is (cl:string= ,princ (princ-to-cl-string ,prin1)))
                    (is (cl:string= ,princ (prin1-to-cl-string ,prin1))))
         `(cl:progn (is (cl:string= ,princ (princ-to-cl-string ,obj)))
                    (is (cl:string= ,prin1 (prin1-to-cl-string ,obj))))))

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
  (prin-test "0.0e+NaN" (@/ 0.0 0.0))
  (prin-test "1.0e+INF" (@/ 1.0 0.0))
  (prin-test "-1.0e+INF" (@/ -1.0 0.0))
  (prin-test "100000000.0" 1e8)
  (prin-test "0.0001" 1e-4)
  (cl:let ((el::float-output-format "~,8f"))
    (prin-test "0.00010000" 1e-4))
  )

(test test-print-quote
  (prin-test "'test" (cl:quote (el::quote el::test)))
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
      (pstrings:build-pstring " " '((el::invisible . el::t))))
  (prin-test (cl:format cl:nil "~c, \"\"~c" #\tab #\soh)
      (cl:format cl:nil "\"~c, \\\"\\\"~c\"" #\tab #\soh)
      (pstrings:build-pstring (cl:format cl:nil "~c, \"\"~c" #\tab #\soh)))
  (prin-test " !\"#$%&'()*+,-./09:;<=>?@AZ[\\]^_`az{|}~"
      "\" !\\\"#$%&'()*+,-./09:;<=>?@AZ[\\\\]^_`az{|}~\""
      (pstrings:build-pstring " !\"#$%&'()*+,-./09:;<=>?@AZ[\\]^_`az{|}~"))
  (prin-test "abc" "#(\"abc\" 0 3 (\"ab\\\"c\" ab\\ c))"
      (pstrings:build-pstring
       "abc"
       (@list
        (@cons (pstrings:build-pstring "ab\"c") 'el::|AB C|))))

  (prin-test "\\235\\353" "\"\\235\\353\""
      (pstrings:build-pstring (cl:format cl:nil "~c~c" (safe-code-char #o235) (safe-code-char #o353))))
  (prin-test "\\235ëЖ" "\"\\235ëЖ\""
      (pstrings:build-pstring (cl:format cl:nil "~c~cЖ" (safe-code-char #o235) (safe-code-char #o353))))

  (cl:let ((pstr (pstrings:build-pstring " ")))
    (cl:setf (pstrings:pstring-multibyte pstr) cl:t)
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
  (prin-test "(a)" '(el::a . el::nil))
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
  (cl:let ((hash (@make-hash-table :test 'el::eq)))
    (@puthash 'el::a (pstrings:build-pstring "2") hash)
    (@puthash 'el::b 3 hash)
    (prin-test
        "#s(hash-table size 65 test eq rehash-size 1.5 rehash-threshold 0.8125 data (a 2 b 3))"
        "#s(hash-table size 65 test eq rehash-size 1.5 rehash-threshold 0.8125 data (a \"2\" b 3))"
        hash)))

(test test-print-chartable
  (cl:let ((ct (chartables:make-simple-chartable :purpose 'purpose :default 'el::nil)))
    (chartables:set-chartable-range ct 4 15000 2)
    (chartables:set-chartable-range ct 10 15 (pstrings:build-pstring "3"))
    (prin-test
        (cl:format cl:nil "~a~%~a~%~a~%~a"
                   "#^[nil nil purpose "
                   "#^^[3 0 nil nil nil nil 2 2 2 2 2 2 3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2] #^^[1 0 #^^[2 0 "
                   "#^^[3 0 nil nil nil nil 2 2 2 2 2 2 3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2] 2 2 #^^[2 12288 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 "
                   "#^^[3 14976 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil] nil nil nil nil nil nil nil nil nil nil] nil nil nil nil nil nil nil nil nil nil nil nil] nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil]"
                   )
        (cl:format cl:nil "~a~%~a~%~a~%~a"
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
  (cl:let ((val (cl:make-array 2)))
    (cl:setf (cl:aref val 0) 'el::a)
    (cl:setf (cl:aref val 1) val)
    (prin-test "[a #0]" val))

  (cl:let ((val (cl:make-array 3)))
    (cl:setf (cl:aref val 0) 'el::a)
    (cl:setf (cl:aref val 1) val)
    (cl:setf (cl:aref val 2) 'el::b)
    (prin-test "[a #0 b]" val))

  (cl:let* ((ref 'el::nil)
            (val (cl:cons 'el::a ref)))
    (cl:setf (cl:cdr val) val)
    (prin-test "(a . #0)" val))

  (cl:let* ((ref 'el::nil)
            (val `(el::a ,ref el::b . ,ref)))
    (cl:setf (cl:nth 1 val) val)
    (cl:setf (cl:cdr (cl:nthcdr 2 val)) val)
    (prin-test "(a #0 b . #0)" val)
    )

  (cl:let ((val (cl:make-array 4)))
    (cl:setf (cl:aref val 0) 'el::a)
    (cl:setf (cl:aref val 1) val)
    (cl:setf (cl:aref val 2) 'el::b)
    (cl:setf (cl:aref val 3) val)
    (prin-test "[a #0 b #0]" val))

  )

(test test-print-record
  (cl:let ((record #(el::test-rec el::abc 123 (1 2 3))))
    (cl:setf (cl:get 'el::test-rec 'el::type) 'el::record)
    (prin-test "#s(test-rec abc 123 (1 2 3))" record)))


(defun test-me ()
  (run! 'cl-emacs/tests/printer-test))
