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

(cl-emacs/lib/elisp-packages:define-elisp-package :cl-emacs/fn-eval
    (:shadow #:eval)
  (:local-nicknames (#:el #:cl-emacs/elisp))
  )
(in-package :cl-emacs/fn-eval)
(log-enable :cl-emacs/fn-eval :debug2)
(named-readtables:in-readtable elisp-function-syntax)


(defconstant +eval-max-depth+ 1600)

(defun* eval-impl (form &key lexical (depth 0))
  (when (@> depth +eval-max-depth+)
    (error 'evaluation-error :details (cl:format nil "evaluation depth can't be more than ~s" +eval-max-depth+)))
  (cl:cond
    ((@consp form)
     (@let ((func (@car form))
            (args (@cdr form))
            (ev-args nil))

       (loop while args
             do (unless (@listp args)
                  (error 'wrong-type-argument :details (cl:format nil "~s should be a list" args)))
                (push (eval-impl (@car args) :lexical lexical :depth (@1+ depth)) ev-args)
                (@setq args (@cdr args)))
       (@cond 
         ((@eq (@type-of func) 'symbol)
          (@apply func ev-args)
          ;; (let ((cl-function (ignore-errors 
          ;;                     (cl:symbol-function func))))
          ;;   (if cl-function
          ;;       (c))
          ;;   (log-info "symbol found ~s" cl-function))
          
          )
         (t 
          (log-info "unknown function type:~s function:~s" (@type-of func) func))
         )
       
       ))
    ((@symbolp form)
     (error "unimplemented")
     )
    (t form)
    )
  )

(defun* eval (form &optional lexical)
  #M"Evaluate FORM and return its value.
     If LEXICAL is t, evaluate using lexical scoping.
     LEXICAL can also be an actual lexical environment, in the form of an
     alist mapping symbols to their value."
  (eval-impl form :lexical lexical)
  )


