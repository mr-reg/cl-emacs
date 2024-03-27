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
(uiop:define-package :cl-emacs/elisp/fns
    (:use :common-lisp :alexandria :cl-emacs/log
     :cl-emacs/elisp/internals)
  (:import-from :common-lisp-user
                #:memq)
  )
(in-package :cl-emacs/elisp/fns)

(defun-elisp memq-elisp "memq" (arg/elt arg/list)
  "Return non-nil if ELT is an element of LIST.  Comparison done with `eq'.
The value is actually the tail of LIST whose car is ELT."
  ;; memq is macros in common lisp, but in elisp we need function
  (memq arg/elt arg/list))
