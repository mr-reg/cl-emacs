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
(uiop:define-package :cl-emacs/elisp/alien-vars
    (:use :common-lisp :cl-emacs/log
          :cl-emacs/elisp/internals)
  )
(in-package :cl-emacs/elisp/alien-vars)
(log-enable :cl-emacs/elisp/alien-vars)

(defvar *var-naming-lock* (bt:make-lock "var-naming-lock"))
(defvar *var-uid* 1)
;; (defvar *alien-sym-map* (make-hash-table))

(defun-elisp elisp/make-unique-alien-var '(:rpc-debug :internal) (arg/value)
  (let* ((uid (bt:with-lock-held (*var-naming-lock*)
                (incf *var-uid*)))
         (sym-str (format nil "uniq-var-~a" uid))
         (sym (string-to-elisp-symbol sym-str)))
    (defvar sym)
    (setf (symbol-value sym) arg/value)
    ;; (setf (gethash arg/sym *alien-sym-map*) sym)
    sym))

;; (defun-elisp elisp/delete-alien-var '(:rpc-debug) (arg/sym)
;;   (declare (symbol arg/sym))
;;   (makunbound arg/sym))

(defun-elisp elisp/alien-increment '(:internal :rpc-debug) (arg/sym)
  (declare (symbol arg/sym))
  (incf (symbol-value arg/sym)))

(defun-elisp elisp/find-alien-symbol-value '(:internal :rpc-debug) (arg/sym)
  (declare (symbol arg/sym))
  (symbol-value arg/sym))

(defun-elisp elisp/alien-boundp '(:internal :rpc-debug) (arg/var-name)
  "return NIL if there is no such alien variable. T otherwise"
  (declare (string arg/var-name))
  (when (find-symbol arg/var-name 'cl-emacs/elisp) t)
  ;; (gethash arg/sym *alien-sym-map*)
  )
