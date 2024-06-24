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
                    )

  )

(in-package :cl-emacs/tests/read-print-test)
(log-enable :cl-emacs/tests/read-print-test :debug2)
(def-suite cl-emacs/lib/read-print-test)
(in-suite cl-emacs/lib/read-print-test)
(named-readtables:in-readtable mstrings:mstring-syntax)

(defun* read-emacs-file ())

(defparameter *emacs-source-folder* "~/github/emacs/")

(define-condition test-error (error-with-description)
  ())

(defun* full-read-file ((file pathname))
  (let ((raw-string (with-output-to-string (out-stream)
                      (with-open-file (in-stream file)
                        (handler-case
                            (loop do (write-char (read-char in-stream) out-stream))
                          (end-of-file ()
                            ))
                        )))
        (position 0)
        result)
    (handler-case
        (loop do (let ((read-result (reader:read-cl-string raw-string position)))
                   (log-debug2 "one read result ~s" read-result)
                   (push (car read-result) result)
                   (setq position (cdr read-result))))
      (reader:eof-reader-error ()
        ))
    (setq result (nreverse result))
    (log-debug "~s" result)
    )
  (error 'test-error)
  )

(defun* run-test-for-subdirectories ((dir pathname))
  (dolist (el-file (uiop/filesystem:directory-files dir "*.el"))
    (log-debug "file ~s" el-file)
    (full-read-file el-file))
  (dolist (sub-dir (uiop/filesystem:subdirectories dir))
    (run-test-for-subdirectories sub-dir)))

(defun run-test ()
  ;; (let ((roo)))
  ;; (uiop/filesystem:directory-files )
  (handler-case
      (run-test-for-subdirectories (truename *emacs-source-folder*))
    (test-error ()
      (log-debug "error")))
  )
