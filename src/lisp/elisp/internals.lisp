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
           #:defun-elisp
           #:eval-intercomm-expr
           #:get-elisp-alias
           #:check-string
           #:check-string-null-bytes
           #:condition-to-elisp-signal
           #:*context*)
  (:import-from :common-lisp-user
                #:class-direct-slots
                #:slot-definition-name
                )
  )
(in-package :cl-emacs/elisp/internals)
(log-enable :cl-emacs/elisp/internals)

(defvar *context* nil)

(defclass emacs-signal (error)
  ())



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


(defun condition-to-elisp-signal (condition)
  "(cons 'wrong-type-argument (list 'stringp a))"
  (declaim (condition condition))
  (with-output-to-string (stream)
    (format stream "(cons '~a (list" (str:downcase (class-name (class-of condition))))
    (dolist (slot-def (class-direct-slots (class-of condition)))
      (let ((raw (slot-value condition (slot-definition-name slot-def))))
        (cond
          ((or (numberp raw) (stringp raw))
           (format stream " ~s" raw))
          ((symbolp raw)
           (format stream " '~a" (str:downcase raw)))
          (t (format stream " \"unsupported type ~a\"" (type-of raw)))))
      )
    (format stream "))")))

;; key = common lisp symbol
;; value = elisp string. 
(defvar *elisp-aliases* (make-hash-table))

(defmacro defun-elisp (function-name elisp-alias args &body body)
  (declare (string elisp-alias))
  `(progn
     (setf (gethash ',function-name *elisp-aliases*) ,elisp-alias)
     ,(append (list 'defun function-name args) body)
     (export ',function-name)))


(defun get-elisp-alias (symbol)
  (gethash symbol *elisp-aliases*)
  )

(defmacro eval-intercomm-expr (function-name &rest args)
  (cons function-name (mapcar #'(lambda (x)
                                  (cond
                                    ((or (symbolp x)
                                         (listp x)) (list 'quote x) )
                                    (t x)))
                              args))
  )
