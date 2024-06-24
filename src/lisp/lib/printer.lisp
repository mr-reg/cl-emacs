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
     :cl-emacs/lib/log
     :fiveam
     :cl-emacs/lib/commons)
  (:local-nicknames (#:el #:cl-emacs/elisp)
                    (#:pstrings #:cl-emacs/types/pstrings)
                    (#:chartables #:cl-emacs/types/chartables)
                    )
  (:export #:prin1-to-cl-stream
           #:princ-to-cl-stream)
  )

(in-package :cl-emacs/lib/printer)
(log-enable :cl-emacs/lib/printer :debug2)
(def-suite cl-emacs/lib/printer)
(in-suite cl-emacs/lib/printer)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* princ-to-cl-stream (obj stream)
  (cond
    ((consp obj)
     (write-char #\( stream)
     (loop with first = t
           while (consp obj)
           do (if first (setq first nil)
                  (write-char #\space stream))
              (princ-to-cl-stream (car obj) stream)
              (setq obj (cdr obj))
              (when (and obj (not (consp obj)))
                (write-sequence " . " stream)
                (princ-to-cl-stream obj stream)))
     (write-char #\) stream))
    ((symbolp obj)
     (princ-to-cl-stream (symbol-name obj) stream))
    ((pstrings:pstring-p obj)
     (pstrings:write-pstring-chunks obj stream nil))
    ((null obj)
     (princ-to-cl-stream (symbol-name nil) stream))
    ((numberp obj)
     (cl:princ obj stream)
     )
    (t (error 'unimplemented-error :details
              (cl:format nil "unsupported object type ~s" (cl:type-of obj)))))
  )
