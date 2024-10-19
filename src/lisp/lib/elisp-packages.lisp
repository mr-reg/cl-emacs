(uiop:define-package :cl-emacs/lib/elisp-packages
    (:use :common-lisp)
  (:export
   #:define-elisp-package
   #:define-elisp-test-package
   #:*elisp-exports*))
(in-package :cl-emacs/lib/elisp-packages)

(defparameter *elisp-exports*
  '((alloc (cons
            list
            intern
            make-symbol
            make-list
            make-string
            vector
            ))
    (data 
     (+ * - / /= 1+ 1- < <= = > >=
      aref arrayp ash atom boundp eq car cdr consp fboundp 
      floatp fmakunbound integerp intern
      isnan keywordp
      listp logand logcount lognot logior logxor min makunbound 
      max mod null numberp recordp set stringp
      symbolp symbol-function symbol-name symbol-plist
      symbol-value type-of vectorp))
    (editfns (char-equal format propertize))
    (eval 
     (and catch cond defvar funcall function functionp
      if let let* macroexpand or prog1 progn
      quote setq signal throw unwind-protect))
    (floatfns 
     (abs acos asin atan ceiling cos exp expt fceiling ffloor
      float floor fround ftruncate log round sin sqrt tan
      truncate))
    (fn-apply (apply))
    (fn-eval (eval))
    (fn-substitute-in-file-name (substitute-in-file-name))
    (fns 
     (append assoc clrhash copy-alist copy-hash-table delete elt eql equal 
      equal-including-properties get gethash hash-table-count 
      hash-table-p hash-table-rehash-size hash-table-rehash-threshold
      hash-table-size hash-table-test identity length 
      make-hash-table mapc mapcan mapcar maphash member memq 
      nconc nreverse nth nthcdr provide puthash random rassoc remhash
      require reverse string-equal string-lessp
      sxhash-eq sxhash-eql sxhash-equal sxhash-equal-including-properties
      sort yes-or-no-p))
    (lread (read))
    (load (load load-suffixes load-path))
    (textprop (set-text-properties add-text-properties))
    (types 
     (string make-string string-chardata  string-properties
      build-string buffer))
    
    ))

(defmacro define-elisp-package (&rest form)
  "wise? automatic exports and imports in all this cross-lisp mess"
  (let* ((raw-package-name (symbol-name (car form)))
         (sub-package-name 
           ;;; emulate (str:replace-first "CL-EMACS/" "" raw-package-name)
           ;;; in native CL...
           (when (and (> (length raw-package-name) 9)
                      (equal (subseq raw-package-name 0 9) "CL-EMACS/"))
             (subseq raw-package-name 9)))
         (new-form form)
         (exports nil)
         (shadows nil))
    (loop 
      for pkg in *elisp-exports*
      for pkg-name = (car pkg)
      for def-exports = (cadr pkg)
      do (when (string= sub-package-name (symbol-name pkg-name))
           (loop 
             for sym in def-exports
             for sym-name = (symbol-name sym)
             do (push (make-symbol sym-name) exports)
                (when (find-symbol sym-name :cl)
                  (push (make-symbol sym-name) shadows))
             )))
    (setq exports (cons :export (reverse exports)))
    (setq shadows (cons :shadow (reverse shadows)))
    (setq new-form (append new-form 
                           '(;; (:import-from #:serapeum
                             ;;  #:fixnump)
                             (:use 
                              #:common-lisp
                              #:defstar
                              #:cl-emacs/lib/log
                              #:cl-emacs/lib/commons
                              #:cl-emacs/lib/errors
                              )
                             ;; (:import-from #:cl
                             ;;  #:array-element-type
                             ;;  #:aref
                             ;;  #:ash
                             ;;  #:assert
                             ;;  #:block
                             ;;  #:boolean
                             ;;  #:break
                             ;;  #:caar
                             ;;  #:case
                             ;;  #:cdar
                             ;;  #:char-code
                             ;;  #:char-upcase
                             ;;  #:character
                             ;;  #:characterp
                             ;;  #:class-name
                             ;;  #:class-of
                             ;;  #:coerce
                             ;;  #:concatenate
                             ;;  #:copy-alist
                             ;;  #:copy-seq
                             ;;  #:char-downcase
                             ;;  #:decf
                             ;;  #:declaim
                             ;;  #:declare
                             ;;  #:defclass
                             ;;  #:defconstant
                             ;;  #:define-condition
                             ;;  #:defstruct
                             ;;  #:defmacro
                             ;;  #:defmethod
                             ;;  #:defparameter
                             ;;  #:defun
                             ;;  #:dolist
                             ;;  #:dotimes
                             ;;  #:double-float
                             ;;  #:ecase
                             ;;  #:end-of-file
                             ;;  #:error
                             ;;  #:find-package
                             ;;  #:find-symbol
                             ;;  #:fixnum
                             ;;  #:float
                             ;;  #:floating-point-overflow
                             ;;  #:hash-table
                             ;;  #:handler-bind
                             ;;  #:handler-case
                             ;;  #:ignore
                             ;;  #:ignore-errors
                             ;;  #:in-package
                             ;;  #:incf
                             ;;  #:inline
                             ;;  #:integer
                             ;;  #:labels
                             ;;  #:lambda
                             ;;  #:list
                             ;;  #:loop
                             ;;  #:make-array
                             ;;  #:make-load-form-saving-slots
                             ;;  #:multiple-value-bind
                             ;;  #:nil
                             ;;  #:not
                             ;;  #:parse-integer
                             ;;  #:pathname
                             ;;  #:pop
                             ;;  #:position
                             ;;  #:push
                             ;;  #:return
                             ;;  #:return-from
                             ;;  #:second
                             ;;  #:setf
                             ;;  #:simple-vector
                             ;;  #:sort
                             ;;  #:stream
                             ;;  #:string
                             ;;  #:string<
                             ;;  #:string>
                             ;;  #:string=
                             ;;  #:symbol
                             ;;  #:t
                             ;;  #:truename
                             ;;  #:unless
                             ;;  #:upper-case-p
                             ;;  #:values
                             ;;  #:when
                             ;;  #:with-input-from-string
                             ;;  #:with-open-file
                             ;;  #:with-output-to-string
                             ;;  #:with-slots
                             ;;  #:zerop
                             ;;  #:&key
                             ;;  #:&optional
                             ;;  #:&rest)
                             )))
    (setq new-form (append new-form 
                           (list exports shadows)
                           ))
    (cons 'uiop:define-package new-form)))

(defmacro define-elisp-test-package (&rest form)
  "test package with minimal interference with :CL to force use of elisp functions "
  (let* ((new-form form))
    (setq new-form (append new-form 
                           '(
                             (:use
                              :defstar
                              :cl-emacs/lib/log
                              :fiveam
                              :cl-emacs/lib/commons
                              :cl-emacs/lib/errors
                              )
                             (:import-from #:cl
                              ;;  #:array-element-type
                              ;;  #:aref
                              ;;  #:ash
                              ;;  #:assert
                              ;;  #:block
                              ;;  #:boolean
                              ;;  #:break
                              ;;  #:caar
                              ;;  #:case
                              ;;  #:cdar
                              ;;  #:char-code
                              ;;  #:char-upcase
                              ;;  #:character
                              ;;  #:characterp
                              ;;  #:class-name
                              ;;  #:class-of
                              ;;  #:coerce
                              ;;  #:concatenate
                              ;;  #:copy-alist
                              ;;  #:copy-seq
                              ;;  #:char-downcase
                              ;;  #:decf
                              ;;  #:declaim
                              ;;  #:declare
                              ;;  #:defclass
                              ;;  #:defconstant
                              ;;  #:define-condition
                              ;;  #:defstruct
                              ;;  #:defmacro
                              ;;  #:defmethod
                              ;;  #:defparameter
                              #:defun
                              ;;  #:dolist
                              ;;  #:dotimes
                              ;;  #:double-float
                              ;;  #:ecase
                              ;;  #:end-of-file
                              ;;  #:error
                              ;;  #:find-package
                              ;;  #:find-symbol
                              ;;  #:fixnum
                              ;;  #:float
                              ;;  #:floating-point-overflow
                              ;;  #:hash-table
                              ;;  #:handler-bind
                              ;;  #:handler-case
                              ;;  #:ignore
                              ;;  #:ignore-errors
                              #:in-package
                              ;;  #:incf
                              ;;  #:inline
                              ;;  #:integer
                              ;;  #:labels
                              ;;  #:lambda
                              ;;  #:list
                              ;;  #:loop
                              ;;  #:make-array
                              ;;  #:make-load-form-saving-slots
                              ;;  #:multiple-value-bind
                              ;;  #:nil
                              ;;  #:not
                              ;;  #:parse-integer
                              ;;  #:pathname
                              ;;  #:pop
                              ;;  #:position
                              ;;  #:push
                              ;;  #:return
                              ;;  #:return-from
                              ;;  #:second
                              ;;  #:setf
                              ;;  #:simple-vector
                              ;;  #:sort
                              ;;  #:stream
                              ;;  #:string
                              ;;  #:string<
                              ;;  #:string>
                              ;;  #:string=
                              ;;  #:symbol
                              ;;  #:t
                              ;;  #:truename
                              ;;  #:unless
                              ;;  #:upper-case-p
                              ;;  #:values
                              ;;  #:when
                              ;;  #:with-input-from-string
                              ;;  #:with-open-file
                              ;;  #:with-output-to-string
                              ;;  #:with-slots
                              ;;  #:zerop
                              ;;  #:&key
                              )
                             (:local-nicknames (#:el #:cl-emacs/elisp)
                              (#:reader #:cl-emacs/lib/reader)
                              (#:pstrings #:cl-emacs/types/pstrings)
                              )
                             )))
    
    (cons 'uiop:define-package new-form)))
(pushnew 'define-elisp-package asdf/package-inferred-system::*defpackage-forms*)
(pushnew 'define-elisp-test-package asdf/package-inferred-system::*defpackage-forms*)
