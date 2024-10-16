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
    (:use
     :defstar
     :cl-emacs/lib/log
     :alexandria
     :fiveam
     :cl-emacs/lib/commons
     )
  (:export #:eval)
  (:local-nicknames (#:el #:cl-emacs/elisp)
                    (#:reader #:cl-emacs/lib/reader))
  )
(in-package :cl-emacs/fn-eval)
(log-enable :cl-emacs/fn-eval :debug2)
(def-suite cl-emacs/fn-eval)
(in-suite cl-emacs/fn-eval)
(named-readtables:in-readtable mstrings:mstring-syntax)

(defun* eval (form &optional lexical)
  #M"Evaluate FORM and return its value.
     If LEXICAL is t, evaluate using lexical scoping.
     LEXICAL can also be an actual lexical environment, in the form of an
     alist mapping symbols to their value."
  (cl:cond
    (t form)
    )
  (error 'unimplemented-error))

(test test-read-symbols
  (is (equal 3 (eval (reader:read-simple "3"))))
  )


(defun test-me ()
  (run! 'cl-emacs/fn-eval))
