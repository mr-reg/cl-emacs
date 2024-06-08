(uiop:define-package :cl-emacs/elisp-packages
    (:use :common-lisp)
  (:export
   #:define-elisp-package))
(in-package :cl-emacs/elisp-packages)
(defmacro define-elisp-package (&rest form)
  (cons 'uiop:define-package
        (append form '((:import-from #:serapeum
                        #:fixnump)
                       (:import-from #:cl
                        #:aref
                        #:ash
                        #:assert
                        #:boolean
                        #:break
                        #:caar
                        #:case
                        #:cdar
                        #:char-upcase
                        #:character
                        #:characterp
                        #:class-name
                        #:class-of
                        #:concatenate
                        #:copy-alist
                        #:copy-seq
                        #:char-downcase
                        #:declaim
                        #:declare
                        #:defclass
                        #:defconstant
                        #:define-condition
                        #:defstruct
                        #:defmethod
                        #:defparameter
                        #:defun
                        #:digit-char-p
                        #:dolist
                        #:dotimes
                        #:end-of-file
                        #:error
                        #:find-symbol
                        #:fixnum
                        #:hash-table
                        #:handler-case
                        #:ignore
                        #:ignore-errors
                        #:in-package
                        #:incf
                        #:inline
                        #:labels
                        #:lambda
                        #:list
                        #:loop
                        #:make-array
                        #:make-load-form-saving-slots
                        #:multiple-value-bind
                        #:nil
                        #:not
                        #:pop
                        #:position
                        #:push
                        #:read-char
                        #:return
                        #:return-from
                        #:second
                        #:setf
                        #:sort
                        #:stream
                        #:string
                        #:string<
                        #:string>
                        #:string=
                        #:symbol
                        #:t
                        #:unless
                        #:upper-case-p
                        #:values
                        #:when
                        #:with-input-from-string
                        #:with-open-file
                        #:with-output-to-string
                        #:with-slots
                        #:write-char
                        #:&key
                        #:&optional
                        #:&rest)))))
(pushnew 'define-elisp-package asdf/package-inferred-system::*defpackage-forms*)
