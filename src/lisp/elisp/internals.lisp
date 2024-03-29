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
           #:generate-c-block
           #:string-to-elisp-symbol
           #:check-string
           #:check-string-null-bytes
           #:condition-to-elisp-signal
           #:*context*
           #:read-lisp-binary-object
           #:write-lisp-binary-object
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
;; (defvar *elisp-aliases* (make-hash-table))

(defmacro defun-elisp (function-name args &body body)
  ;; (setf (gethash ',function-name *elisp-aliases*) ,elisp-alias)
  ;; (declare (string elisp-alias))
  `(progn
     ,(append (list 'defun function-name args) body)
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

(defun generate-c-block ()
  (with-output-to-string (stream)
    (format stream "#ifndef ALIEN_INJECTION~%")
    (format stream "#define ALIEN_INJECTION~%")
    (format stream "#include \"lisp.h\"~%")
    (format stream "#include \"alien-intercomm.h\"~%")
    (let (c-aliases)
      (do-external-symbols (symbol :cl-emacs/elisp)
        (handler-case
            (let* ((function (symbol-function symbol))
                   (name (string-downcase (symbol-name symbol)))
                   (elisp-alias (get-elisp-alias symbol))
                   (c-alias (get-c-alias symbol))
                   (args (delete '&optional (ccl:arglist function)))
                   (docstring (documentation symbol 'function))
                   (n-req-args) (n-opt-args))

              (multiple-value-setq (n-req-args n-opt-args) (function-args function))

              (format stream "EXFUN (F~a, ~d);~%"
                      c-alias (+ n-req-args n-opt-args))
              
              (format stream "DEFUN (\"~a\", F~a, S~a, ~d, ~d, 0,~%"
                      elisp-alias c-alias c-alias n-req-args (+ n-req-args n-opt-args))
              (push c-alias c-aliases)
              (format stream "       doc: /* ~a */)~%" docstring)
              (format stream "  (")
              (let (comma) (dolist (arg args)
                             (when comma (format stream ", "))
                             (setq comma t)
                             (format stream "Lisp_Object ~a" (get-c-alias arg))))
              (format stream ")~%")
              (format stream "{~%")
              (format stream "  Lisp_Object alien_data[] = {")
              (let (comma) (dolist (arg args)
                             (when comma (format stream ", "))
                             (setq comma t)
                             (format stream "~a" (get-c-alias arg))))
              (format stream "};~%")
              (format stream "  return alien_rpc(\"cl-emacs/elisp:~a\", ~d, alien_data);~%" name (length args))
              (format stream "}~%~%")
              )
          (undefined-function ()
            ;; skip non-function exports
            ;; (break)
            )
          ))
      (format stream "void init_alien_injection (void) {~%")
      (dolist (c-alias c-aliases)
        (format stream "  defsubr (&S~a);~%" c-alias))
      (format stream "}~%")
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



(defun read-lisp-binary-object (stream)
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
       (cons (read-lisp-binary-object stream)
             (read-lisp-binary-object stream)))
      (t (format nil "unsupported type ~a" type)))))

(defun write-lisp-binary-object (obj stream)
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
     (write-byte (char-code #\S) stream)
     (let ((bytes (babel:string-to-octets (elisp-symbol-to-string obj))))
       (lisp-binary:write-integer (length bytes) 8 stream)
       (lisp-binary:write-bytes bytes stream)))
    ((consp obj)
     (write-byte (char-code #\C) stream)
     (write-lisp-binary-object (car obj) stream)
     (write-lisp-binary-object (cdr obj) stream)
     )
    (t (write-lisp-binary-object "unsupported" stream))))
