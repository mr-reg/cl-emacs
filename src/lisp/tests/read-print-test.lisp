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

(defun* read-emacs-file ())

(defparameter *emacs-source-folder* "~/github/emacs/")
(defvar *good-tested-files* (make-hash-table :test 'equal))
(defparameter *excluded-files* '(
                                 "lisp/pcomplete.el"
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
        (el::print-escape-multibyte t)
        (el::string-multibyte-flag-emacs-compatible t))
    (log-debug2 "raw-string: ~s" raw-string)
    (with-output-to-string (out-stream)
      (handler-case
          (loop do (let ((read-result (reader:read-cl-string raw-string position)))
                     (log-debug2 "one read result ~s" read-result)
                     (printer:princ-to-cl-stream (car read-result) out-stream)
                     (write-char #\newline out-stream)
                     (incf position (cdr read-result))
                     (log-debug2 "new read position ~s" position)))
        (reader:eof-reader-error ()
          ))))

  )
(defun* el-read-princ-file ((file pathname))
  (let* ((filename (cl:namestring file))
         (cmd (cl:format nil "emacs --batch -Q --load src/elisp/read.el --eval '(read-princ-el-file \"~a\" \"raw-emacs.log\")'" filename))
         (process-info (uiop:launch-program cmd :output :stream ))
         (in-stream (uiop:process-info-output process-info)))
    (with-output-to-string (out-stream)
      (handler-case
          (loop for char = (read-char in-stream)
                do (write-char char out-stream))
        (end-of-file ())))
    )


  )

(defun* one-test ((filename pathname))
  ;; ~/github/emacs/lisp/calendar/cal-x.el
  (cl:format t "filename ~s " filename)
  (when (gethash (cl:namestring filename) *good-tested-files*)
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
  (handler-case
      (run-test-for-subdirectories
       (truename (concatenate 'string
                              *emacs-source-folder*
                              "lisp/"
                              ;; "lisp/calendar/"
                              )))
    (test-error ()
      (log-debug "error")))
  )
(defun debug-one-test ()
  (handler-case
      (one-test #P"/root/github/emacs/lisp/pcomplete.el")
    (test-error ()
      (log-debug "error")))
  )
