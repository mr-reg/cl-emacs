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
(uiop:define-package :cl-emacs/elisp
    (:use :common-lisp :cl-emacs/log :cl-emacs/elisp/internals)
  (:use-reexport
   :cl-emacs/elisp/alien-vars
   :cl-emacs/elisp/alloc
   :cl-emacs/elisp/data
   :cl-emacs/elisp/editfns
   :cl-emacs/elisp/fileio
   :cl-emacs/elisp/fns
   :cl-emacs/elisp/font
   :cl-emacs/elisp/xfns
   )
  (:export #:rpc-apply))
(in-package :cl-emacs/elisp)
(log-enable :cl-emacs/elisp)

#|       
IMPORTANT NOTE 

If elisp function argument name has the same name as variable in
lexical scope, you will have PROBLEMS. So all function arguments
should have kinda unique name, so I always use prefix arg_
|#

(defun rpc-apply (argv)
  (setq *context* (third argv))
  (let ((func (string-to-elisp-symbol (first argv)))
        (func-args (second argv)))
    ;; (log-info (cons func func-args))
    ;; (eval (cons func func-args))
    ;; (log-info (macroexpand (cons func func-args)))
    (apply (symbol-function func) func-args)
    ))

(defun-elisp elisp/init-globals '(:internal :rpc-debug) ()
  "set emacs global-vars to default values"
  (loop for var-sym being each hash-key of *defvar-defaults*
        do (setf (symbol-value var-sym) (gethash var-sym *defvar-defaults*))))

(defvar-elisp test-alien-var fixnum 10
  "")

(let ((c-name "alien-injection.c"))
  (with-open-file (stream c-name
                          :if-exists :supersede
                          :direction :output)
    (format stream "~a" (generate-c-block)))
  (push :other-write (osicat:file-permissions c-name))
  (push :group-write (osicat:file-permissions c-name))
  )

(let ((h-name "alien-injection.h"))
  (with-open-file (stream h-name
                          :if-exists :supersede
                          :direction :output)
    (format stream "~a" (generate-h-block)))
  (push :other-write (osicat:file-permissions h-name))
  (push :group-write (osicat:file-permissions h-name))
  )

