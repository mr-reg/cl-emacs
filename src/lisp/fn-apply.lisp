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

(cl-emacs/lib/elisp-packages:define-elisp-package :cl-emacs/fn-apply
    (:use
     :defstar
     :cl-emacs/lib/log
     :cl-emacs/data
     :cl-emacs/eval
     :cl-emacs/fns
     :cl-emacs/lib/commons
     :cl-emacs/lib/errors
     )
  (:export #:apply)
  (:local-nicknames (#:el #:cl-emacs/elisp))
  )
(in-package :cl-emacs/fn-apply)
(log-enable :cl-emacs/fn-apply :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)

(defun* apply (func &rest args)
  #M"Call FUNCTION with our remaining args, using our last arg as list of args.
     Then return the value FUNCTION returns.
     With a single argument, call the argument's first element using the
     other elements as args.
     Thus, (apply \\='+ 1 2 \\='(3 4)) returns 10."
  (when (null args)
    (error 'wrong-type-argument :details "apply function should at least one invocation argument"))
  (let ((cl-func (ignore-errors 
                  (cl:symbol-function func)))
        (raw-args args)
        (processed-args nil))
    (loop while raw-args
          do (if (null (cdr raw-args))
                 ;;; if we are on the last argument
                 (progn (unless (listp (car raw-args))
                          (error 'wrong-type-argument :details "apply function should receive last argument as list"))
                        (setq processed-args (cl:nconc (nreverse processed-args) (car raw-args))))
                 (push (car raw-args) processed-args))
             (setq raw-args (cdr raw-args)))
    (log-debug2 "args:~s" processed-args)
    (if cl-func
        (cl:apply cl-func processed-args)
        (error 'void-function :details (cl:format nil "unknown function ~s" func)))
    )
  
  
  )
