#|
Copyright (C) 2024 by Gleb Borodulia
Author: Gleb Borodulia <mr.reg@mail.ru>

This file is part of cl-emacs.

cl-emacs is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

cl-emacs is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with cl-emacs. If not, see <https://www.gnu.org/licenses/>.
|#
(uiop:define-package :cl-emacs/elisp/internals
    (:use :common-lisp :cl-emacs/log)
  (:export #:wrong-type-argument
           #:check-string
           #:check-string-null-bytes
           #:*context*)
  )
(in-package :cl-emacs/elisp/internals)
(log-enable :cl-emacs/elisp/internals)

(defvar *context* nil)

(defclass emacs-signal (error)
  ())

(defmethod print-object ((condition emacs-signal) stream)
  "(cons 'wrong-type-argument (list 'stringp a))"
  (format stream "(cons '~a (list" (str:downcase (class-name (class-of condition))))
  (dolist (slot-def (ccl:class-direct-slots (class-of condition)))
    (let ((raw (slot-value condition (ccl:slot-definition-name slot-def))))
      (cond
        ((or (numberp raw) (stringp raw))
         (format stream " ~s" raw))
        ((symbolp raw)
         (format stream " '~a" (str:downcase raw)))
        (t (format stream " \"unsupported type ~a\"" (type-of raw)))))
    )
  (format stream "))")

  )

(define-condition wrong-type-argument (emacs-signal)
  ((predicate :initarg :predicate
              :type symbol
              )
   (value :initarg :value)))


(defun check-string (arg)
  (unless (stringp arg)
    (error 'wrong-type-argument :predicate 'stringp :value arg))
  )


(defun check-string-null-bytes (arg)
  (when (find #\NULL arg)
    (error 'wrong-type-argument :predicate 'filenamep :value arg)))


