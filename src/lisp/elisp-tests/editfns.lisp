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
(uiop:define-package :cl-emacs/elisp-tests/editfns
    (:use :common-lisp :cl-emacs/log :fiveam
     :cl-emacs/elisp/internals :cl-emacs/elisp/editfns ))
(in-package :cl-emacs/elisp-tests/editfns)
(def-suite elisp/editfns)
(in-suite elisp/editfns)

(test styled-format
  (is (string= " $ TAG"
               (elisp/styled-format 0 " %c TAG" 36)))
  (is (string= ")〈"
               (elisp/styled-format 0 ")%c" 12296)))
  (is (string= "[?\\C-9]"
               (elisp/styled-format 0 "[?\\C-%c]" 57)))
  (is (string= "malayalam-cdac"
               (elisp/styled-format 0 "%s-cdac" 'malayalam)))
  (is (string= "nil-35"
               (elisp/styled-format 0 "%s-%d" "nil" 35)))
  )


(defun run-all-tests ()
  (run! 'elisp/editfns))

