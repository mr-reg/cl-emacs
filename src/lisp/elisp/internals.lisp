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
  (:export
   #:*context*
   #:*defun-flags*
   #:check-string
   #:check-string-null-bytes
   #:condition-to-elisp-signal
   #:defun-elisp
   #:defvar-elisp
   #:elisp-symbol-to-string
   #:eval-intercomm-expr
   #:generate-c-block
   #:generate-h-block
   #:get-elisp-alias
   #:read-lisp-binary-object
   #:string-to-elisp-symbol
   #:write-lisp-binary-object
   #:*defvar-defaults*
   ;; #:wrong-type-argument
   )
  (:import-from :common-lisp-user
                #:class-direct-slots
                #:slot-definition-name
                #:function-args
                )
  )
(in-package :cl-emacs/elisp/internals)
(log-enable :cl-emacs/elisp/internals)

(defvar *context* nil)

(defclass emacs-signal (error)
  ())

(defconstant +vector-type/normal-vector+ 0)
(defconstant +vector-type/free+ 1)
(defconstant +vector-type/bignum+ 2)
(defconstant +vector-type/marker+ 3)
(defconstant +vector-type/overlay+ 4)
(defconstant +vector-type/finalizer+ 5)
(defconstant +vector-type/symbol_with_pos+ 6)
(defconstant +vector-type/misc_ptr+ 7)
(defconstant +vector-type/user_ptr+ 8)
(defconstant +vector-type/process+ 9)
(defconstant +vector-type/frame+ 10)
(defconstant +vector-type/window+ 11)
(defconstant +vector-type/bool_vector+ 12)
(defconstant +vector-type/buffer+ 13)
(defconstant +vector-type/hash_table+ 14)
(defconstant +vector-type/terminal+ 15)
(defconstant +vector-type/window_configuration+ 16)
(defconstant +vector-type/subr+ 17)
(defconstant +vector-type/other+ 18)           
(defconstant +vector-type/xwidget+ 19)
(defconstant +vector-type/xwidget_view+ 20)
(defconstant +vector-type/thread+ 21)
(defconstant +vector-type/mutex+ 22)
(defconstant +vector-type/condvar+ 23)
(defconstant +vector-type/module_function+ 24)
(defconstant +vector-type/native_comp_unit+ 25)
(defconstant +vector-type/ts_parser+ 26)
(defconstant +vector-type/ts_node+ 27)
(defconstant +vector-type/ts_compiled_query+ 28)
(defconstant +vector-type/sqlite+ 29)
(defconstant +vector-type/compiled+ 30)
(defconstant +vector-type/char_table+ 31)
(defconstant +vector-type/sub_char_table+ 32)
(defconstant +vector-type/record+ 33)
(defconstant +vector-type/font+ 34)

;; (define-condition wrong-type-argument (emacs-signal)
;;   ((predicate :initarg :predicate
;;               :type symbol
;;               )
;;    (value :initarg :value)))


;; (defun check-string (arg)
;;   (unless (stringp arg)
;;     (error 'wrong-type-argument :predicate 'stringp :value arg))
;;   )


;; (defun check-string-null-bytes (arg)
;;   (when (find #\NULL arg)
;;     (error 'wrong-type-argument :predicate 'filenamep :value arg)))


;; (defun serialize-to-elisp (obj &optional toplevel)
;;   (with-output-to-string (stream)
;;     (cond
;;       ((or (numberp obj) (stringp obj))
;;        (format stream "~s" obj))
;;       ((symbolp obj)
;;        (when toplevel
;;          (format stream "'"))
;;        (loop for c across (symbol-name obj)
;;              with upcase = nil
;;              do (cond
;;                   (upcase
;;                    (format stream "~a" (str:upcase c))
;;                    (setq upcase nil))
;;                   ((eq #\_ c)
;;                    (setq upcase t))
;;                   (t
;;                    (format stream "~a" (str:downcase c)))))
;;        )
;;       ((consp obj)
;;        (format stream "(cons ~a ~a)"
;;                (serialize-to-elisp (car obj) toplevel)
;;                (serialize-to-elisp (cdr obj) toplevel)))
;;       (t (format stream "~s" "unsupported")))))

(defun condition-to-elisp-signal (condition)
  "(cons 'wrong-type-argument (list 'stringp a))"
  (declaim (condition condition))
  (cons (class-name (class-of condition))
        (mapcar #'(lambda (slot-def)
                    (slot-value condition (slot-definition-name slot-def)))
                (class-direct-slots (class-of condition)))))

;; key = common lisp symbol
;; value = elisp string. 
(defvar *defun-flags* (make-hash-table))

(defmacro defun-elisp (function-name flags args &body body)
  "flags: 
:internal - do not make function visible in elisp 
:c-native - generate c function interface, only for &restp
:rpc-debug - show debug RPC log
"
  ;; (declare (string elisp-alias))
  `(progn
     ,(append (list 'defun function-name args) body)
     (setf (gethash ',function-name *defun-flags*) ,flags)
     (export ',function-name)))


(defun get-elisp-alias (symbol)
  (let ((str (elisp-symbol-to-string symbol)))
    (cl-ppcre:regex-replace "^elisp/|^arg/" str ""))
  ;; (gethash symbol *elisp-aliases*)
  )
(defun get-c-alias (symbol)
  (str:replace-all "-" "_" (get-elisp-alias symbol))
  )

(defmacro eval-intercomm-expr (function-name &rest args)
  (cons function-name (mapcar #'(lambda (x)
                                  (cond
                                    ((or (symbolp x)
                                         (listp x)) (list 'quote x) )
                                    (t x)))
                              args))
  )

(defun generate-elisp-c-fun (stream symbol)  
;;; elisp
  (let* ((function (symbol-function symbol))
         (func-name (string-downcase (symbol-name symbol)))
         (elisp-alias (get-elisp-alias symbol))
         (c-alias (get-c-alias symbol))
         (args (delete '&rest (delete '&optional (ccl:arglist function))))
         (docstring (documentation symbol 'function))
         n-req-args
         n-opt-args
         restp keys
         array-args
         n-total-args)
    (multiple-value-setq (n-req-args n-opt-args restp keys) (function-args function))
    (setq array-args (or restp keys))
    (setq n-total-args (+ n-req-args n-opt-args))
    (format stream "DEFUN (\"~a\", F~a, S~a, ~d, ~a, 0,~%"
            elisp-alias c-alias c-alias n-req-args (if array-args "MANY" n-total-args))

    (format stream "       doc: /* ~a */)~%" docstring)
    (format stream "  (")
    (if array-args
        (format stream "ptrdiff_t argc, Lisp_Object *argv")
        (loop for argi from 0 below n-total-args
              for arg in args
              with comma = nil
              do (progn
                   (when comma (format stream ", "))
                   (setq comma t)
                   (format stream "Lisp_Object ~a" (get-c-alias arg)))))
    (format stream ")~%")
    (format stream "{~%")
    (unless array-args
      (format stream "  Lisp_Object alien_data[] = {")
      (loop for argi from 0 below n-total-args
            for arg in args
            with comma = nil
            do (progn
                 (when comma (format stream ", "))
                 (setq comma t)
                 (format stream "~a" (get-c-alias arg))))
      (format stream "};~%")
      )
    (format stream "  return alien_rpc((char*)\"cl-emacs/elisp:~a\"" func-name)
    (unless array-args
      (format stream ", ~d, alien_data);~%" n-total-args))
    (when array-args
      (format stream ", argc, argv);~%"))
    (format stream "}~%~%")))

(defun generate-native-c-fun (stream symbol)
;;; c-native
  (let* ((function (symbol-function symbol))
         (c-alias (get-c-alias symbol))
         (args (delete '&rest (delete '&optional (ccl:arglist function))))
         n-req-args
         n-opt-args
         restp
         n-total-args)
    (multiple-value-setq (n-req-args n-opt-args restp) (function-args function))
    (unless restp
      (return-from generate-native-c-fun))
    (setq n-total-args (+ n-req-args n-opt-args))

    (format stream "Lisp_Object ~a~a~a ("
            c-alias n-total-args (if restp "n" ""))
    (loop for argi from 0 below n-total-args
          for arg in args
          with comma = nil
          do (progn
               (when comma (format stream ", "))
               (setq comma t)
               (format stream "Lisp_Object ~a" (get-c-alias arg)))
          finally (when restp
                    (when comma (format stream ", "))
                    (format stream "ptrdiff_t argc, Lisp_Object *argv")))
    (format stream ")~%")
    (format stream "{~%")
    (format stream "  Lisp_Object *alien_data = malloc (sizeof(Lisp_Object) * ~a);~%"
            (if restp
                (format nil "(argc + ~d)" n-total-args)
                n-total-args))
    (loop for argi from 0 below n-total-args
          for arg in args
          do (format stream "  alien_data[~d] = ~a;~%"
                     argi (get-c-alias arg)))
    (when restp
      (format stream "  memcpy(alien_data + ~d, argv, argc * sizeof(Lisp_Object));~%"
              n-total-args))
    (format stream "  Lisp_Object result = F~a(~a, alien_data);~%"
            c-alias
            (if restp
                (format nil "(argc + ~d)" n-total-args)
                n-total-args))
    (format stream "  free(alien_data);~%")
    (format stream "  return result;~%")
    (format stream "}~%~%")
    ))
(defun generate-native-c-header (stream symbol)
;;; c-native
  (let* ((function (symbol-function symbol))
         (c-alias (get-c-alias symbol))
         (args (delete '&rest (delete '&optional (ccl:arglist function))))
         n-req-args
         n-opt-args
         restp
         n-total-args)
    (multiple-value-setq (n-req-args n-opt-args restp) (function-args function))
    (unless restp
      (return-from generate-native-c-header))
    (setq n-total-args (+ n-req-args n-opt-args))

    (format stream "Lisp_Object ~a~a~a ("
            c-alias n-total-args (if restp "n" ""))
    (loop for argi from 0 below n-total-args
          for arg in args
          with comma = nil
          do (progn
               (when comma (format stream ", "))
               (setq comma t)
               (format stream "Lisp_Object ~a" (get-c-alias arg)))
          finally (when restp
                    (when comma (format stream ", "))
                    (format stream "ptrdiff_t argc, Lisp_Object *argv")))
    (format stream ");~%")
    ))


(defun generate-c-block ()
  (with-output-to-string (stream)
    (format stream "#include <stdlib.h>~%")
    (format stream "#include \"config.h\"~%")
    (format stream "#include \"lisp.h\"~%")
    (format stream "#include \"alien-intercomm.h\"~%")
    (format stream "#include \"alien-injection.h\"~%")
    (let (func-c-aliases function-symbols var-symbols)
      (do-external-symbols (symbol :cl-emacs/elisp)
        (handler-case
            (when (symbol-function symbol)
              (push symbol function-symbols))
          (undefined-function ()
            (push symbol var-symbols)
            )
          ))

      
      (dolist (symbol function-symbols)
        (let* ((c-alias (get-c-alias symbol))
               (flags (gethash symbol *defun-flags*)))
          (unless (find :internal flags)
            (push c-alias func-c-aliases))

          (generate-elisp-c-fun stream symbol)
          (generate-native-c-fun stream symbol)

          ))
      (dolist (symbol var-symbols)
        (format stream "Lisp_Object A~a;~%" (get-c-alias symbol))
        )
      (format stream "void visit_alien_roots (struct gc_root_visitor visitor) {~%")
      (dolist (symbol var-symbols)
        (format stream "  visitor.visit(&A~a, GC_ROOT_C_SYMBOL, visitor.data);~%" (get-c-alias symbol)))
      (format stream "}~%")

      (format stream "void init_alien_injection (void) {~%")
      (dolist (c-alias func-c-aliases)
        (format stream "  defsubr (&S~a);~%" c-alias))
      (loop for symbol in var-symbols
            for symbol-idx from 0
            do (let ((c-alias (get-c-alias symbol)))
                 (format stream "  A~a = intern(\"~a\");~%"
                         c-alias (str:downcase (symbol-name symbol)))
                 (format stream "  static struct Lisp_Objfwd const o_fwd~a = {Lisp_Fwd_Alien, &A~a};~%"
                         symbol-idx c-alias)
                 (format stream "  defvar_lisp (&o_fwd~a, \"~a\");~%"
                         symbol-idx (str:downcase (symbol-name symbol)))
                 ))
      (format stream "}~%")
      )
    ))

(defun generate-h-block ()
  (with-output-to-string (stream)
    (format stream "#ifndef ALIEN_INJECTION_H~%")
    (format stream "#define ALIEN_INJECTION_H~%")
    (format stream "#include \"lisp.h\"~%")
    
    (let (function-symbols var-symbols)
      (do-external-symbols (symbol :cl-emacs/elisp)
        (handler-case
            (when (symbol-function symbol)
              (push symbol function-symbols))
          (undefined-function ()
            (push symbol var-symbols)
            )
          ))
      (dolist (symbol function-symbols)
        (let* ((function (symbol-function symbol))
               (c-alias (get-c-alias symbol))
               n-req-args n-opt-args restp keys
               array-args)
          (multiple-value-setq (n-req-args n-opt-args restp keys) (function-args function))
          (setq array-args (or restp keys))
          (format stream "EXFUN (F~a, ~a);~%"
                  c-alias (if array-args
                              "MANY"
                              (+ n-req-args n-opt-args)))))
      (dolist (symbol function-symbols)
        (generate-native-c-header stream symbol)
        )
      (dolist (symbol var-symbols)
        (format stream "extern Lisp_Object A~a;~%" (get-c-alias symbol))
        )
      (format stream "void visit_alien_roots (struct gc_root_visitor visitor);~%")
      (format stream "void init_alien_injection (void);~%")
      )
    (format stream "#endif")
    ))

;; ;; unused
;; (defun generate-elisp-block ()
;;   (with-output-to-string (stream)
;;     (format stream "(progn~%")
;;     (do-external-symbols (symbol :cl-emacs/elisp)
;;       (handler-case
;;           (let* ((function (symbol-function symbol))
;;                  (name (string-downcase (symbol-name symbol)))
;;                  (alias (cl-emacs/elisp/internals:get-elisp-alias symbol))
;;                  (args (mapcar #'string-downcase (ccl:arglist function)))
;;                  (docstring (documentation symbol 'function))
;;                  )
;;             (when alias
;;               (format stream "(defalias '~a #'(lambda (" alias)
;;               (dolist (arg args)
;;                 (format stream "~a " arg))
;;               (format stream ")~%")
;;               (format stream "~s~%" docstring)
;;               (format stream "  (common-lisp-apply 'cl-emacs/elisp:~a (list" name)
;;               (dolist (arg args)
;;                 (unless (string= arg "&optional")
;;                   (format stream " ~a" arg)))
;;               (format stream "))))~%~%")))
;;         (undefined-function ()
;;           ;; skip non-function exports
;;           )))
;;     (format stream ")~%")))

(defun string-to-elisp-symbol(str)
  (let* ((cl-str (with-output-to-string (stream)
                   (loop for c across str
                         do (when (or (<= (char-code #\A) (char-code c) (char-code #\Z)) (eq c #\_))
                              (format stream "_"))
                            (format stream "~a" (str:upcase c)))))
         (package (find-package 'cl-emacs/elisp)))
    (let ((pos (position #\: cl-str)))
      (when pos
        (setq package (str:substring 0 pos cl-str))
        (setq cl-str (str:substring (1+ pos) (length cl-str) cl-str))
        (when (string= package "")
          (setq package (find-package 'keyword)))))
    (intern cl-str package)))
(defun elisp-symbol-to-string (sym)
  (with-output-to-string (stream)
    (loop for c across (symbol-name sym)
          with upcase = nil
          do (cond
               (upcase
                (format stream "~a" (str:upcase c))
                (setq upcase nil))
               ((eq #\_ c)
                (setq upcase t))
               (t
                (format stream "~a" (str:downcase c)))))))



(defun read-lisp-binary-object (stream stack)
  (declare (hash-table stack))
  (let ((type (code-char (read-byte stream))))
    (cond
      ((eq type #\I)
       (lisp-binary:read-integer 8 stream))
      ((eq type #\D)
       (lisp-binary:read-float :double :stream stream))
      ((eq type #\A)
       (let* ((len (lisp-binary:read-integer 8 stream))
              (bytes (lisp-binary:read-bytes len stream)))
         (babel:octets-to-string bytes)))
      ((eq type #\S)
       (let* ((len (lisp-binary:read-integer 8 stream))
              (bytes (lisp-binary:read-bytes len stream))
              (str (babel:octets-to-string bytes)))
         (string-to-elisp-symbol str)))
      ((eq type #\C)
       (let ((addr (lisp-binary:read-integer 8 stream))
             (cons (cons nil nil)))
         (setf (gethash addr stack) cons)
         (setf (car cons) (read-lisp-binary-object stream stack))
         (setf (cdr cons) (read-lisp-binary-object stream stack))
         cons))
      ((eq type #\R)
       (let ((addr (lisp-binary:read-integer 8 stream)))
         (gethash addr stack)))
      ;; ((eq type #\H)
      ;;  (let* ((test-sym (read-lisp-binary-object stream stack))
      ;;         (rehash-size (read-lisp-binary-object stream stack))
      ;;         (rehash-threshold (read-lisp-binary-object stream stack))
      ;;         (len (lisp-binary:read-integer 8 stream))
      ;;         (test-fun (cond
      ;;                     ((or (eq test-sym 'eq))
      ;;                      (symbol-function test-sym))
      ;;                     (t (error "unsupported hash test func"))))
      ;;         (hash (make-hash-table :test test-fun :size len :rehash-size rehash-size :rehash-threshold rehash-threshold))
      ;;         )
      ;;    (loop for idx from 0 below len
      ;;          do (let ((key (read-lisp-binary-object stream stack))
      ;;                   (value (read-lisp-binary-object stream stack)))
      ;;               ;; (log-debug "~s->~s" key value)
      ;;               (setf (gethash key hash) value)))
      ;;    hash))
      ;; ((eq type #\V)
      ;;  (let ((vector-type (lisp-binary:read-integer 8 stream))
      ;;        (len (lisp-binary:read-integer 8 stream))
      ;;        result)
      ;;    (cond
      ;;      ((eq vector-type +vector-type/hash_table+)
      ;;       (setq result (make-hash-table))
      ;;       (loop for idx from 0 below len
      ;;             do (let ((key (read-lisp-binary-object stream stack))
      ;;                      (value (read-lisp-binary-object stream stack)))
      ;;                  (setf (gethash key result) value))))
      ;;      (t
      ;;       (setq result (make-array len))
      ;;       (loop for idx from 0 below len
      ;;             do (setf (aref result idx) (read-lisp-binary-object stream stack))))
      ;;      )
      ;;    (if (eq vector-type +vector-type/normal-vector+)
      ;;        result
      ;;        (cons vector-type result)))
      ;;  )
      (t (format nil "unknown elisp type ~a" type)))))



(defun write-lisp-binary-object (obj stream stack)
  (declare (hash-table stack))
  ;; (break)
  (cond
    ((integerp obj)
     (write-byte (char-code #\I) stream)
     (lisp-binary:write-integer obj 8 stream))
    ((floatp obj)
     (write-byte (char-code #\D) stream)
     (lisp-binary:write-float :double obj :stream stream))
    ((stringp obj)
     (write-byte (char-code #\A) stream)
     (let ((bytes (babel:string-to-octets obj)))
       (lisp-binary:write-integer (length bytes) 8 stream)
       (lisp-binary:write-bytes bytes stream)))
    ((symbolp obj)
     ;; (handler-case
     ;;     (progn (symbol-value obj)
     ;;            (write-byte (char-code #\L) stream)
     ;;            )
     ;;   (unbound-variable ()
     ;;     )
     ;;   )
     (write-byte (char-code #\S) stream)
     (let ((bytes (babel:string-to-octets (elisp-symbol-to-string obj))))
       (lisp-binary:write-integer (length bytes) 8 stream)
       (lisp-binary:write-bytes bytes stream)))
    ((hash-table-p obj)
     (write-lisp-binary-object (cl-emacs/elisp::elisp/make-unique-alien-var obj) stream stack)
     )
    ((consp obj)
     (let ((id (gethash obj stack)))
       (if id
           (progn
             (write-byte (char-code #\R) stream)
             (lisp-binary:write-integer id 8 stream))
           (progn
             (setq id (hash-table-count stack))
             (setf (gethash obj stack) id)
             (write-byte (char-code #\C) stream)
             (lisp-binary:write-integer id 8 stream)
             (write-lisp-binary-object (car obj) stream stack)
             (write-lisp-binary-object (cdr obj) stream stack))))

     )
    (t (write-lisp-binary-object "unsupported" stream stack))))

;; key - var symbol
;; value - default value
(defvar *defvar-defaults* (make-hash-table))

(defmacro defvar-elisp (var-name type init-value docstring)
  `(progn
     (declaim (,type ,var-name))
     (setf (gethash ',var-name *defvar-defaults*) ,init-value)
     ,(append (list 'defvar var-name init-value docstring))
     (export ',var-name)))
