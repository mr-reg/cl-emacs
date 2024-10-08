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

(cl-emacs/lib/elisp-packages:define-elisp-package :cl-emacs/tests/read-print-test
    (:use
     :defstar
     :cl-emacs/data
     :cl-emacs/eval
     :cl-emacs/fns
     :cl-emacs/lib/commons
     :cl-emacs/lib/log
     :fiveam
     )
  (:local-nicknames (#:el #:cl-emacs/elisp)
                    (#:reader #:cl-emacs/lib/reader)
                    (#:printer #:cl-emacs/lib/printer)
                    )

  )

(in-package :cl-emacs/tests/read-print-test)
;; (log-enable :cl-emacs/tests/read-print-test :debug2)
(log-enable :cl-emacs/tests/read-print-test :debug)
(def-suite cl-emacs/lib/read-print-test)
(in-suite cl-emacs/lib/read-print-test)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defparameter *test-cache-enabled* t)
(defparameter *parse-errors-enabled* nil)
(defun* read-emacs-file ())

(defparameter *emacs-source-folder* "~/github/emacs/")
(defvar *good-tested-files* (make-hash-table :test 'equal))
(defparameter *excluded-files* '(
                                 "lisp/pcomplete.el"
                                 "lisp/international/titdic-cnv.el"
                                 "lisp/international/uni-bidi.el"
                                 "lisp/international/uni-brackets.el"
                                 "lisp/international/uni-category.el"
                                 "lisp/international/uni-combining.el"
                                 "lisp/international/uni-comment.el"
                                 "lisp/international/uni-decimal.el"
                                 "lisp/international/uni-decomposition.el"
                                 "lisp/international/uni-digit.el"
                                 "lisp/international/uni-mirrored.el"
                                 "lisp/international/uni-name.el"
                                 "lisp/international/uni-numeric.el"
                                 "lisp/international/uni-old-name.el"
                                 "lisp/language/ethio-util.el"
                                 "lisp/language/ethiopic.el"
                                 "lisp/language/ind-util.el"
                                 "lisp/language/tibet-util.el"
                                 "lisp/language/tibetan.el"
                                 "lisp/leim/leim-list.el"
                                 "lisp/leim/quail/ARRAY30.el"
                                 "leim/quail/ECDICT.el"
                                 "leim/quail/Punct-b5.el"
                                 "leim/quail/Punct.el"
                                 "leim/quail/ethiopic.el"
                                 "leim/quail/quick-cns.el"
                                 "leim/quail/tibetan.el"
                                 "leim/quail/tsang-cns.el"
                                 "org/org-entities.el"
                                 "progmodes/ebnf-abn.el"
                                 "progmodes/ebnf-bnf.el"
                                 "progmodes/ebnf-dtd.el"
                                 "progmodes/ebnf-ebx.el"
                                 "progmodes/ebnf-iso.el"
                                 "progmodes/ebnf-yac.el"
                                 "json-tests.el"
                                 "subr-tests.el"
                                 "xml-tests.el"
                                 "emacs-lisp/bindat-tests.el"
                                 "emacs-lisp/cl-lib-tests.el"
                                 "emacs-lisp/macroexp-tests.el"
                                 "emacs-lisp/rx-tests.el"
                                 "international/textsec-tests.el"
                                 "net/tramp-tests.el"
                                 "compat/compat-tests.el"
                                 ))
;; (clrhash *good-tested-files*)
(define-condition test-error (error-with-description)
  ())

(defun* cl-read-princ-file ((file pathname))
  (let ((raw-string (with-output-to-string (out-stream)
                      (with-open-file (in-stream file)
                        (handler-case
                            (loop do (write-char (read-char in-stream) out-stream))
                          (end-of-file ())))))
        (position 0)
        (el::float-output-format "~,6f")
        (el::string-multibyte-flag-emacs-compatible t))
    (log-debug2 "raw-string: ~s" raw-string)
    (with-output-to-string (out-stream)
      (handler-case
          (handler-bind ((reader:incomplete-reader-error #'(lambda (condition)
                                                             (when *parse-errors-enabled*
                                                               (error condition)))))
            (loop do (let ((read-result (reader:read-cl-string raw-string position)))
                       (log-debug2 "one read result ~s" read-result)
                       (printer:princ-to-cl-stream (car read-result) out-stream)
                       (write-char #\newline out-stream)
                       (incf position (cdr read-result))
                       (log-debug2 "new read position ~s" position)))

            )
        (reader:empty-reader-error ())))))

(defun* el-read-princ-file ((file pathname))
  (let* ((filename (cl:namestring file))
         (cmd (cl:format nil "emacs --batch -Q --load src/elisp/read.el --eval '(read-princ-el-file \"~a\" \"raw-emacs.log\")'" filename))
         (process-info (uiop:launch-program cmd :output :stream ))
         (in-stream (uiop:process-info-output process-info)))
    (with-output-to-string (out-stream)
      (handler-case
          (loop for char = (read-char in-stream)
                do (write-char char out-stream))
        (end-of-file ()))
      (cl:close in-stream))
    )


  )

(defun* one-test ((filename pathname))
  ;; ~/github/emacs/lisp/calendar/cal-x.el
  (cl:format t "filename ~s " filename)
  (when (and *test-cache-enabled*
             (gethash (cl:namestring filename) *good-tested-files*))
    (cl:format t "previously tested~%")
    (return-from one-test))
  (dolist (suffix *excluded-files*)
    (when (str:ends-with-p suffix (cl:namestring filename))
      (cl:format t "excluded~%")
      (return-from one-test)))
  (let* (
         ;; (filename "/root/github/cl-emacs/src/elisp/test.el")
         ;; (filename "/root/github/emacs/lisp/calendar/cal-x.el")
         (cl-str (cl-read-princ-file filename))
         (el-str (el-read-princ-file filename))
         )
    (if (not (string= cl-str el-str))
        (progn
          (cl:format t "has differences~%")
          (with-open-file (stream "cl.txt" :direction :output
                                           :if-does-not-exist :create
                                           :if-exists :supersede)
            (write-sequence cl-str stream))
          (with-open-file (stream "el.txt" :direction :output
                                           :if-does-not-exist :create
                                           :if-exists :supersede)
            (write-sequence el-str stream))

          (let* ((process-info (uiop:launch-program
                                "wdiff -3 cl.txt el.txt" :output :stream))
                 (in-stream (uiop:process-info-output process-info)))
            (handler-case
                (loop for char = (read-char in-stream)
                      do (write-char char t))
              (end-of-file ()))
            )
          (error 'test-error))
        (progn
          (setf (gethash (cl:namestring filename) *good-tested-files*) t)
          (cl:format t "OK~%")))
    ))

(defun* run-test-for-subdirectories ((dir pathname))
  (dolist (el-file (uiop/filesystem:directory-files dir "*.el"))
    (one-test el-file)
    ;; (log-debug "file ~s" el-file)
    ;; (full-read-file el-file)
    )
  (dolist (sub-dir (uiop/filesystem:subdirectories dir))
    (run-test-for-subdirectories sub-dir)))

(defun run-test ()
  ;; (let ((roo)))
  ;; (uiop/filesystem:directory-files )
  ;; (clrhash *good-tested-files*)
  (handler-case
      (run-test-for-subdirectories
       (truename 
        "~/.emacs.d/straight/repos/"
        ;; (concatenate 'string
        ;;              *emacs-source-folder* "lisp/"
        ;;              ;; "test/lisp/"
        ;;              ;; "lisp/calendar/"
        ;;              )
        ))
    (test-error ()
      (log-debug "error")))
  )
(defun debug-one-test ()
  (handler-case
      (let ((*test-cache-enabled* nil)
            (*parse-errors-enabled* t))
        (one-test (truename (concatenate 'string
                                         ;; "lisp/"
                                         ;; "double.el"
                                         ;; "/home/re9/github/emacs/test/lisp/emacs-lisp/macroexp-tests.el"
                                         "/root/.emacs.d/straight/repos/dap-mode/dap-magik.el"
                                         
                                         ;; "../cl-emacs/.tmp.el"
                                         ;; "lisp/calendar/"
                                         ))))
    ;; (test-error ()
    ;;   (log-debug "error"))
    )
  )
