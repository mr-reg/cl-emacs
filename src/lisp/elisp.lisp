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

(uiop:define-package :cl-emacs/elisp)
(cl:unuse-package :common-lisp :cl-emacs/elisp)
(cl:do-symbols (symbol :cl-emacs/elisp)
  (cl:unintern symbol))
(cl:in-package :cl-emacs/elisp)
;; (cl-user::package-use-list :cl-emacs/elisp)
(named-readtables:in-readtable mstrings:mstring-syntax)
;; (log-enable :cl-emacs/elisp :debug1)
;; (def-suite cl-emacs/elisp)
;; (in-suite cl-emacs/elisp)

;; (in-package :cl-emacs/elisp)
(cl:defun reexport-symbols (package)
  (cl-emacs/lib/commons:reexport-symbols package))

;; (unless t
;;   (do-symbols (symbol :cl-emacs/elisp)
;;     (format t "~s~%" symbol)))

(cl:defparameter float-output-format cl:nil)
;; (cl:defparameter print-escape-multibyte cl:nil)

(cl:defparameter string-multibyte-flag-emacs-compatible cl:nil
  "if t, string multibyte flag computation will take longer, but will produce same weird results as emacs do. Needed only for hard compatibility testing")

