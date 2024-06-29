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
  ;; (:export #:prin1-to-cl-stream
  ;;          #:princ-to-cl-stream)
  )

(in-package :cl-emacs/lib/printer)
(log-enable :cl-emacs/lib/printer :debug2)
(def-suite cl-emacs/lib/printer)
(in-suite cl-emacs/lib/printer)
(named-readtables:in-readtable mstrings:mstring-syntax)

(defun* write-pstring-to-cl-stream ((pstr pstrings:pstring) stream &key escaped)
  (do-generator (char (pstrings:generate-chars pstr))
    (when (and escaped (or (cl:char<= char #\space)
                           (char-end-of-statement-p char)))
      (write-char #\\ stream))
    (write-char char stream)))
(defun* print-to-cl-stream (obj stream raw-mode)
  #M"if raw-mode = t, then princ, else prin1"
  (cond
    ((and (consp obj) (eq (car obj) 'el::quote))
     (write-char #\' stream)
     (print-to-cl-stream (car (cdr obj)) stream raw-mode))
    ((consp obj)
     (write-char #\( stream)
     (loop with first = t
           while (consp obj)
           do (if first (setq first nil)
                  (write-char #\space stream))
              (print-to-cl-stream (car obj) stream raw-mode)
              (setq obj (cdr obj))
              (when (and obj (not (consp obj)))
                (write-sequence " . " stream)
                (print-to-cl-stream obj stream raw-mode)))
     (write-char #\) stream))
    ((or (symbolp obj) (null obj))
     (let ((symbol-name (symbol-name obj)))
       (when (pstrings:emptyp symbol-name)
         (write-sequence "##" stream))
       (if raw-mode
           (write-pstring-to-cl-stream symbol-name stream :escaped nil)
           (write-pstring-to-cl-stream symbol-name stream :escaped t))))
    ((pstrings:pstring-p obj)
     (pstrings:write-pstring-chunks obj stream nil))
    ((numberp obj)
     (cond 
       ((and (floatp obj) (float-features:float-infinity-p obj))
        (write-sequence "1.0e+INF" stream))
       ((and (floatp obj) (isnan obj))
        (write-sequence "0.0e+NaN" stream))
       (t (cl:princ obj stream))))
    (t (error 'unimplemented-error :details
              (cl:format nil "unsupported object type ~s" (cl:type-of obj)))))
  )
(defun* (princ-to-cl-string -> string) (obj)
  (with-output-to-string (stream)
    (print-to-cl-stream obj stream t)))
(defun* (prin1-to-cl-string -> string) (obj)
  (with-output-to-string (stream)
    (print-to-cl-stream obj stream nil)))

(defmacro prin-test (princ prin1 obj)
  `(progn (is (string= ,princ (princ-to-cl-string ,obj)))
          (is (string= ,prin1 (prin1-to-cl-string ,obj)))))

(test test-print-symbol ()
  (prin-test "##" "##" 'el::||)
  (prin-test "test" "test" 'el::test)
  (prin-test ":test" ":test" 'el::\:test)
  (prin-test "ab_~!@$%^&:<>{}?c" "ab_~!@$%^&:<>{}?c" 'el::|AB__~!@$%^&:<>{}?C|)
  (prin-test "AbCd" "AbCd" 'el::_AB_CD)
  (prin-test "nil" "nil" 'el::nil)
  (prin-test "abc_, [/]'`" "abc_\\,\\ \\[/\\]\\'\\`" 'el::|ABC__, [/]'`|)
  (prin-test "non-intern-symbol" "non-intern-symbol" (cl:make-symbol "non-intern-symbol"))
  (prin-test " !\"#$%&'()*+,-./09:;<=>?@az[\\]^_`az{|}~"
             "\\\\\\\\\\\\\\\\ !\\\"\\#$%&\\'\\(\\)*+\\,-./09:\\;<=>?@az\\[\\\\\]^_\\`az{|}~"
             (cl:make-symbol " !\"#$%&'()*+,-./09:;<=>?@AZ[\\]^__`az{|}~"))
  )

(test test-print-numbers 
  (prin-test "12" "12" 12)
  (prin-test "-1" "-1" -1)
  (prin-test "-1.0" "-1.0" -1.0)
  (prin-test "0.0" "0.0" 0.0)
  (prin-test "0.0e+NaN" "0.0e+NaN" (/ 0.0 0.0))
  (prin-test "1.0e+INF" "1.0e+INF" (/ 1.0 0.0))
  (prin-test "1.0e+INF" "1.0e+INF" (/ -1.0 0.0))
  )

(test test-print-quote 
  (prin-test "'test" "'test" (quote (el::quote el::test))))

(defun test-me ()
  (run! 'cl-emacs/lib/printer))
;; (loop for code from 1 to 127
;;       do (cl:format t "~c~%" (code-char code)))
