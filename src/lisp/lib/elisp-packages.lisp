(uiop:define-package :cl-emacs/lib/elisp-packages
    (:use :common-lisp)
  (:export
   #:define-elisp-package
   #:define-elisp-test-package
   #:*elisp-exports*
   #:cl-emacs-package-system
   #:elisp-function-syntax
   ))
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
      symbol-value type-of vectorp *nan* *positive-infinity*
      *negative-infinity*))
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
         ;; (shadows nil)
         )
    (loop 
      for pkg in *elisp-exports*
      for pkg-name = (car pkg)
      for def-exports = (cadr pkg)
      do (when (string= sub-package-name (symbol-name pkg-name))
           (loop 
             for sym in def-exports
             for sym-name = (symbol-name sym)
             do (push (make-symbol sym-name) exports)
                ;; (when (find-symbol sym-name :cl)
                ;;   (push (make-symbol sym-name) shadows))
             )))
    (setq exports (cons :export (reverse exports)))
    ;; (setq shadows (cons :shadow (reverse shadows)))
    (setq new-form (append new-form 
                           '((:use 
                              #:common-lisp
                              #:defstar
                              #:cl-emacs/lib/log
                              #:cl-emacs/lib/commons
                              #:cl-emacs/lib/errors
                              #:cl-emacs/lib/elisp-packages
                              )
                             )))
    (setq new-form (append new-form 
                           (list exports ;; shadows
                                 )
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
                              #:cl-emacs/lib/elisp-packages
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



(defparameter *defpackage-macro-forms* nil)
(pushnew 'define-elisp-package asdf/package-inferred-system::*defpackage-forms*)
(pushnew 'define-elisp-test-package asdf/package-inferred-system::*defpackage-forms*)
(pushnew 'define-elisp-package *defpackage-macro-forms*)
(pushnew 'define-elisp-test-package *defpackage-macro-forms*)
;; (defparameter *defpackage-forms* '(cl:defpackage 
;;                                    uiop:define-package
;;                                    define-elisp-package
;;                                    define-elisp-test-package))

(defvar *dependent-elisp-packages* nil)

(defun elisp-function-reader (stream arg)
  (declare (ignore arg))
  (let ((parsed
          (string-upcase
           (with-output-to-string (str-stream)
             (handler-case
                 (loop for next-char = (peek-char nil stream)
                       for next-code = (char-code next-char)
                       ;; for x = (log-info "next-code ~s" next-code)
                       until (not (or (<= (char-code #\A) next-code (char-code #\Z))
                                      (<= (char-code #\a) next-code (char-code #\z))
                                      (<= (char-code #\0) next-code (char-code #\9))
                                      (member next-code 
                                              (list (char-code #\-)
                                                    (char-code #\.)
                                                    (char-code #\>)
                                                    (char-code #\<)
                                                    (char-code #\+)
                                                    (char-code #\*)
                                                    (char-code #\=)
                                                    (char-code #\:)
                                                    (char-code #\/)))))
                       do (write-char (read-char stream) str-stream))
               (end-of-file ()))))))
    ;; (format t "parsed '~s'" parsed)
    (let (fn-package fn-sym)
      (loop 
        for pkg in cl-emacs/lib/elisp-packages:*elisp-exports*
        for pkg-name = (format nil "CL-EMACS/~a" (symbol-name (car pkg)))
        for exports = (cadr pkg)
        do (loop 
             for sym in exports
             do (when (string= (symbol-name sym) parsed)
                  ;; (log-debug2 "pkg-name:'~s'" pkg-name)
                  (pushnew pkg-name *dependent-elisp-packages* :test #'string=)
                  (setq fn-package (find-package pkg-name))
                  ;; (log-debug2 "fn-package:'~s'" fn-package)
                  (unless fn-package
                    (error "can't find package ~s" pkg-name))
                  (setq fn-sym (find-symbol parsed fn-package))
                  ;; (log-debug2 "fn-sym:'~s'" fn-sym)
                  (unless fn-sym
                    (error "can't find external symbol ~s in package ~s"
                           parsed pkg-name))
                  (return-from elisp-function-reader fn-sym)
                  )))
      (error "can't find symbol ~s definition in elisp-packages"
             parsed)
      
      )))


(named-readtables:defreadtable elisp-function-syntax
  (:merge mstrings:mstring-syntax)
  (:macro-char #\@ #'elisp-function-reader))

(defclass cl-emacs-package-system (asdf:system)
  ())

(defun defpackage-form-p (form)
  (and (consp form)
       (member (car form) asdf/package-inferred-system::*defpackage-forms*)))

(defun file-dependencies (file)
  "Return the first DEFPACKAGE form in FILE."
  (let (forms
        (*readtable* (copy-readtable *readtable*))
        (*dependent-elisp-packages* nil)
        result
        )
    (named-readtables:in-readtable elisp-function-syntax)
    (uiop:with-input-file (stream file)
      ;; (format t "forms:~s" (uiop:slurp-stream-forms stream))
      (loop for form = (ignore-errors
                        (handler-case
                            (read stream t)
                          (end-of-file ()
                            ;; (format t "eof~%")
                            (return))))
            do ;; (format t "read ~s~%" form)
               (when (defpackage-form-p form)
                 ;; (format t "processing form ~s~%" form)
                 (if (member (car form) *defpackage-macro-forms*)
                     (push (macroexpand-1 form) forms)
                     (push form forms))
                 ;; (format t "pushed~%")
                 ))
      (setq result *dependent-elisp-packages*)
      ;; (format t "dep-packages:~s~%" *dependent-elisp-packages*)
      )
    (dolist (form forms)
      (dolist (dep (asdf/package-inferred-system::package-dependencies form))
        (pushnew dep result :test #'string=))
      ;; (format t "deps:~s~%" )
      )
    result))

(defun cl-emacs-package-file-dependencies (file &optional system)
  (remove system 
          (remove t (mapcar 'asdf/package-inferred-system::package-name-system (file-dependencies file)))
          :test #'string=)
  )

;; ;; Given package-inferred-system object, check whether its specification matches
;; ;; the provided parameters
;; (defun same-package-inferred-system-p (system name directory subpath around-compile dependencies)
;;   (and (eq (type-of system) 'package-inferred-system)
;;        (equal (component-name system) name)
;;        (pathname-equal directory (component-pathname system))
;;        (equal dependencies (component-sideway-dependencies system))
;;        (equal around-compile (around-compile-hook system))
;;        (let ((children (component-children system)))
;;          (and (length=n-p children 1)
;;               (let ((child (first children)))
;;                 (and (eq (type-of child) 'cl-source-file)
;;                      (equal (component-name child) "lisp")
;;                      (and (slot-boundp child 'relative-pathname)
;;                           (equal (slot-value child 'relative-pathname) subpath))))))))

;; sysdef search function to push into *system-definition-search-functions*
(defun cl-emacs-package-system-search (system-name)
  "Made to be added to *SYSTEM-DEFINITION-SEARCH-FUNCTIONS*."
  ;; (format t "system-name:~s~%" system-name)
  ;;; For main case system-name is name of package, like "cl-emacs/lib/commons"
  (let ((primary (asdf:primary-system-name system-name)))
    ;;; primary is name of really loading asd system. We care only about "cl-emacs"
    ;;; keep original universal asdf check here
    (unless (equal primary system-name)
      (let ((system (asdf:find-system primary nil))
            dir)
        ;;; check that in asd file :class is equal to our custom type
        (when (and (typep system'cl-emacs-package-system)
                   (setq dir (asdf:component-pathname system)))
          (let* ((sub (subseq system-name (1+ (length primary))))
                 (component-type (asdf::class-for-type system :file))
                 (file-type (asdf:file-type (make-instance component-type)))
                 (f (uiop:probe-file* (uiop:subpathname dir sub :type file-type)
                                      :truename asdf:*resolve-symlinks*)))
            (when (uiop:file-pathname-p f)
              (let ((dependencies (cl-emacs-package-file-dependencies f system-name))
                    (previous (asdf:registered-system system-name))
                    (around-compile (asdf::around-compile-hook system))
                    )
                (if (asdf/package-inferred-system::same-package-inferred-system-p previous system-name dir sub around-compile dependencies)
                    previous
                    (eval `(asdf:defsystem ,system-name
                             :class cl-emacs-package-system
                             :default-component-class ,component-type
                             :source-file ,(asdf:system-source-file system)
                             :pathname ,dir
                             :depends-on ,dependencies
                             :around-compile ,around-compile
                             :components ((,component-type file-type :pathname ,sub)))))
                ))
            )
          )
        ))
    )
  )

(pushnew 'cl-emacs-package-system-search asdf:*system-definition-search-functions*)
;; (setf *system-definition-search-functions*
;;       (remove (find-symbol* :sysdef-package-system-search :asdf/package-system nil)
;;               *system-definition-search-functions*))
