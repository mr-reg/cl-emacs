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
(uiop:define-package :cl-emacs/elisp/globals
    (:use :common-lisp :cl-emacs/log
          :cl-emacs/elisp/internals)
  (:export
   #:init-globals
   )
  )
(in-package :cl-emacs/elisp/globals)
(log-enable :cl-emacs/elisp/globals)


;; key - var symbol
;; value - default value
(defvar *defvar-defaults* (make-hash-table))
(defun init-globals ()
  "set emacs global-vars to default values"
  (loop for var-sym being each hash-key of *defvar-defaults*
        do (setf (symbol-value var-sym) (gethash var-sym *defvar-defaults*))))

(defmacro defvar-elisp (var-name init-value docstring)
  `(progn
     (setf (gethash ',var-name *defvar-defaults*) ,init-value)
     ,(append (list 'defvar var-name init-value docstring))
     (export ',var-name)))

;; (defvar invocation-directory nil )
(declaim (fixnum gcs-done))
(defvar-elisp gcs-done 0 "Accumulated number of garbage collections done.")

(defvar *var-naming-lock* (bt:make-lock "var-naming-lock"))
(defvar *var-uid* 1)
(defun-elisp elisp/make-alien-var () (arg/name arg/value)
  (declare (string arg/name))
  (let* ((uid (bt:with-lock-held (*var-naming-lock*)
                (incf *var-uid*)))
         (sym-str (format nil "~a-~a" arg/name uid))
         (sym (string-to-elisp-symbol sym-str)))
    (defvar sym)
    (setf (symbol-value sym) arg/value)
    sym))

(defun-elisp elisp/delete-alien-var () (arg/sym)
  (declare (symbol arg/sym))
  (makunbound arg/sym))

(defun-elisp elisp/increment '(:internal :c-native) (arg/sym)
  (declare (symbol arg/sym))
  (incf (symbol-value arg/sym)))
