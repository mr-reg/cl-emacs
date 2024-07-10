(uiop:define-package :cl-emacs/lib/elisp-packages
    (:use :common-lisp)
  (:export
   #:define-elisp-package))
(in-package :cl-emacs/lib/elisp-packages)
(defmacro define-elisp-package (&rest form)
  (cons 'uiop:define-package
        (append form '((:import-from #:serapeum
                        #:fixnump)
                       (:import-from #:cl
                        #:array-element-type
                        #:aref
                        #:ash
                        #:assert
                        #:block
                        #:boolean
                        #:break
                        #:caar
                        #:case
                        #:cdar
                        #:char-code
                        #:char-upcase
                        #:character
                        #:characterp
                        #:class-name
                        #:class-of
                        #:coerce
                        #:concatenate
                        #:copy-alist
                        #:copy-seq
                        #:char-downcase
                        #:decf
                        #:declaim
                        #:declare
                        #:defclass
                        #:defconstant
                        #:define-condition
                        #:defstruct
                        #:defmacro
                        #:defmethod
                        #:defparameter
                        #:defun
                        #:digit-char-p
                        #:dolist
                        #:dotimes
                        #:double-float
                        #:ecase
                        #:end-of-file
                        #:error
                        #:find-symbol
                        #:fixnum
                        #:float
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
                        #:parse-integer
                        #:pathname
                        #:pop
                        #:position
                        #:push
                        #:read-char
                        #:read-sequence
                        #:return
                        #:return-from
                        #:second
                        #:setf
                        #:simple-vector
                        #:sort
                        #:stream
                        #:string
                        #:string<
                        #:string>
                        #:string=
                        #:symbol
                        #:t
                        #:truename
                        #:unless
                        #:upper-case-p
                        #:values
                        #:when
                        #:with-input-from-string
                        #:with-open-file
                        #:with-output-to-string
                        #:with-slots
                        #:write-char
                        #:write-sequence
                        #:zerop
                        #:&key
                        #:&optional
                        #:&rest)))))
(pushnew 'define-elisp-package asdf/package-inferred-system::*defpackage-forms*)
