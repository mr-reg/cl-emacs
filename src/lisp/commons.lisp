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

(uiop:define-package :cl-emacs/commons
    (:use :common-lisp :cl-emacs/log :alexandria :fiveam
          :defstar)

  (:export
   #:reexport-symbols
   #:unimplemented-error
   #:error-with-description
   #:simple-print-condition-with-slots))
(in-package :cl-emacs/commons)
(log-enable :cl-emacs/commons :debug1)

(defvar *timer* 0)
(defun set-timer ()
  (let* ((new-time (get-internal-real-time))
         (time (- new-time *timer*)))
    (setq *timer* new-time)
    (* 1.0 (/ time internal-time-units-per-second))))

(defun* reexport-symbols (package-from)
  (do-external-symbols (sym package-from)
    (shadowing-import sym)
    (export sym)))

(defun* simple-print-condition-with-slots (obj (stream stream))
  "emulate emacs error formatting syntax"
  (format stream "#<~a" (class-name (class-of obj)))
  (let ((cls (class-of obj)))
    (dolist (slot (cl-user::class-slots cls))
      (let ((slot-sym (cl-user::slot-definition-name slot)))
        (format stream " ~a:~s" slot-sym
                (slot-value obj slot-sym)))))
  (format stream ">"))

(define-condition base-error ()
  ()
  (:report simple-print-condition-with-slots))


(define-condition unimplemented-error (base-error)
  ())

(define-condition error-with-description (base-error)
  ((details :initarg :details
            :initform ""
            :type string)))


(defmacro import-cl-basics ()
  )

;; (defmacro define-elisp-package (&rest form)
;;   (cons 'uiop:define-package
;;         (append form '((:import-from #:cl
;;                         #:and
;;                         #:aref
;;                         #:ash
;;                         #:caar
;;                         #:case
;;                         #:cdar
;;                         #:char-upcase
;;                         #:character
;;                         #:characterp
;;                         #:class-name
;;                         #:class-of
;;                         #:concatenate
;;                         #:cond
;;                         #:copy-alist
;;                         #:copy-seq
;;                         #:char-downcase
;;                         #:declaim
;;                         #:declare
;;                         #:defstruct
;;                         #:defclass
;;                         #:define-condition
;;                         #:defmethod
;;                         #:defparameter
;;                         #:defun
;;                         #:defvar
;;                         #:digit-char-p
;;                         #:dolist
;;                         #:dotimes
;;                         #:error
;;                         #:find-symbol
;;                         #:fixnum
;;                         #:funcall
;;                         #:gethash
;;                         #:handler-case
;;                         #:if
;;                         #:in-package
;;                         #:incf
;;                         #:inline
;;                         #:labels
;;                         #:lambda
;;                         #:let
;;                         #:let*
;;                         #:loop
;;                         #:make-array
;;                         #:make-load-form-saving-slots
;;                         #:multiple-value-bind
;;                         #:nil
;;                         #:nth
;;                         #:null
;;                         #:or
;;                         #:pop
;;                         #:push
;;                         #:progn
;;                         #:read-char
;;                         #:return
;;                         #:return-from
;;                         #:symbol
;;                         #:setq
;;                         #:setf
;;                         #:sort
;;                         #:string
;;                         #:string<
;;                         #:string>
;;                         #:string=
;;                         #:symbol-function
;;                         #:t
;;                         #:type-of
;;                         #:unless
;;                         #:upper-case-p
;;                         #:values
;;                         #:when
;;                         #:with-input-from-string
;;                         #:with-open-file
;;                         #:with-output-to-string
;;                         #:with-slots
;;                         #:write-char
;;                         #:&key
;;                         #:&optional
;;                         #:&rest)))))

;; (defmacro setup-elisp-package (package)
;;   `(progn
;;      (cl:unuse-package :common-lisp ,package)
;;      (cl:do-symbols (symbol ,package)
;;        (when (find-symbol (symbol-name symbol) :common-lisp)
;;          (log-debug "unintern ~s" symbol)
;;          (cl:unintern symbol)))
;;      (cl:import '(
;;                   cl:error
;;                   cl:in-package
;;                   cl:*package*
;;                   )
;;                 ,package)
;;      (cl:in-package ,package)))
