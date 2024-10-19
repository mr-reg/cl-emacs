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

(uiop:define-package :cl-emacs/lib/reader-utils
    (:use
     :cl-emacs/lib/log
     :alexandria
     :fiveam
     :common-lisp
     :defstar)
  (:export
   #:char-end-of-statement-p
   #:char-list-to-cl-string
   #:char-list-to-pstring
   #:char-whitespace-p
   #:parse-elisp-number
   #:reversed-list-to-number
   #:simple-digit-char-p
   )
  (:import-from #:serapeum
                #:memq)
  (:local-nicknames (#:el #:cl-emacs/elisp)
                    (#:pstrings #:cl-emacs/types/pstrings)))
(in-package :cl-emacs/lib/reader-utils)
(log-enable :cl-emacs/lib/reader-utils :info)
;; (log-enable :cl-emacs/lib/reader-utils :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(def-suite cl-emacs/lib/reader-utils)
(in-suite cl-emacs/lib/reader-utils)

(defun* parse-elisp-number (chardata)
  (log-debug2 "parse-elisp-number ~s" chardata)
  (let ((has-nan (str:ends-with-p "NaN" chardata))
        (has-inf (str:ends-with-p "INF" chardata))
        (to-parse chardata))
    (when (or has-nan has-inf)
      (setq to-parse (str:substring 0 -3 chardata))
      (when (or (str:ends-with-p "e+" to-parse)
                (str:ends-with-p "e-" to-parse)
                (str:ends-with-p "E-" to-parse)
                (str:ends-with-p "E+" to-parse)
                )
        (setq to-parse (str:concat to-parse "0")))
      )
    ;; (log-debug2 "parsing number from chardata ~s" to-parse)
    (when (emptyp to-parse)
      (return-from parse-elisp-number nil))
    (handler-case
        (let ((parsed (handler-case
                          (parse-number:parse-real-number to-parse :float-format 'double-float)
                        (common-lisp:floating-point-overflow ()
                          (setq has-inf t)
                          (if (str:starts-with-p "-" to-parse) -1 1)))))
          ;; here we know that number notation is correct
          (cond
            (has-nan cl-emacs/data::*nan*)
            ((and has-inf (> parsed 0)) cl-emacs/data::*positive-infinity*)
            ((and has-inf (< parsed 0)) cl-emacs/data::*negative-infinity*)
            (t parsed)))
      (error ()
        nil)
      )))

(defun* reversed-list-to-number ((digits list) (radix-bits fixnum))
  #M"internal function for readers
     Inputs:
     digits - reversed list of digits in integer form,
     radix - radix in power of 2, like 3 means 2^3 octal, 4 - 2^4 hex
     result - one big number"
  (loop for digit in digits
        for shift from 0 by radix-bits
        sum (ash digit shift)))

;; (defun octals-to-code (octals)
;;   (declare (list octals))
;;   (loop for oct in octals
;;         for shift from 0 by 3
;;         sum (ash oct shift)))
;; (defun hex-to-code (hex)
;;   (declare (list hex))
;;   (loop for h in hex
;;         for shift from 0 by 4
;;         sum (ash h shift)))

(defun* (char-list-to-cl-string -> string) (char-list)
  (with-output-to-string (stream)
    (dolist (char char-list)
      (cl:write-char char stream))))

(defun* (char-list-to-pstring -> pstrings:pstring) (char-list)
  (pstrings:build-pstring (with-output-to-string (stream)
                            (dolist (char char-list)
                              (cl:write-char char stream)))))

(test parse-elisp-number
  (is (= (parse-elisp-number "+1") 1))
  (is (= (parse-elisp-number "24") 24))
  (is (= (parse-elisp-number "-3") -3))
  (is (= (parse-elisp-number "0") 0))
  (is (= (parse-elisp-number "0.0") 0.0))
  (is (= (parse-elisp-number "-4.5") -4.5))
  (is (= (parse-elisp-number "2.71828") 2.71828d0))
  (is (= (parse-elisp-number "1.5e2") 150.0d0))
  )
(test radix-list
  (is (= #o321 (reversed-list-to-number '(1 2 3) 3)))
  (is (= #x1fa (reversed-list-to-number '(10 15 1) 4)))
  )

(defun* char-whitespace-p ((char character))
  #M"return t if character is whitespace or #\null.
     #\nullnil used for EOF"
  (memq char '(#\null #\space #\tab #\newline #\page #\return #\dc4)))

(defun* char-end-of-statement-p ((char character))
  #M"return t if character is whitespace or nil or any
     special symbol that signals about new statement. "
  (or (char-whitespace-p char) (memq char '(#\( #\) #\[ #\] #\" #\' #\` #\, #\# #\;))))

(defun* (simple-digit-char-p -> (or integer null)) ((char character) &optional (radix 10))
  #M"If char is a digit in the specified radix, returns the fixnum for which
     that digit stands, else returns NIL.

     We can't use CL standard function digit-char-p, because it is too clever 
     and it understands some non-ascii symbols as digits"
  (let* ((code (char-code char))
         (digit (cond
                  ((<= (char-code #\0) code (char-code #\9))
                   (- code (char-code #\0)))
                  ((<= (char-code #\a) code (char-code #\f))
                   (+ 10 (- code (char-code #\a))))
                  ((<= (char-code #\A) code (char-code #\F))
                   (+ 10 (- code (char-code #\A))))
                  (t radix))))
    (when (< digit radix) digit)
    ))
