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
(uiop:define-package :cl-emacs/types
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons)
  (:shadow #:string #:make-string)
  (:export #:string
           #:make-string
           #:string-chardata
           #:string-properties
           #:build-string)
  )
(in-package :cl-emacs/types)
(log-enable :cl-emacs/types :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)

(defstruct string
  (chardata "" :type cl:string)
  (properties nil :type list))

(defun* (build-string -> string) ((cl-string cl:string))
  (make-string :chardata cl-string))

;; (in-package :cl-emacs/elisp)
;; (reexport-symbols :cl-emacs/types)
