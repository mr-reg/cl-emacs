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
;; (defpackage :cl-emacs/elisp)
(uiop:define-package :cl-emacs/elisp
  ;; (:import-from #:cl
  ;;               #:in-package)
  ;; (:import-from #:cl-emacs/commons
  ;;               #:reexport-symbols)
  ;; (:use-reexport
  ;;  :cl-emacs/reader
  ;;  ;; :cl-emacs/elisp/alloc
  ;;  ;; :cl-emacs/elisp/data
  ;;  ;; :cl-emacs/elisp/editfns
  ;;  ;; :cl-emacs/elisp/fileio
  ;;  ;; :cl-emacs/elisp/fns
  ;;  ;; :cl-emacs/elisp/font
  ;;  ;; :cl-emacs/elisp/xfns
  ;;  )
  ;; (:export #:rpc-apply)
  )
;; (delete-package :cl-emacs/elisp)
(in-package :cl-emacs/elisp)
(named-readtables:in-readtable mstrings:mstring-syntax)
;; (log-enable :cl-emacs/elisp :debug1)
;; (def-suite cl-emacs/elisp)
;; (in-suite cl-emacs/elisp)


;; (in-package :cl-emacs/elisp)
(defun reexport-symbols (package)
  (cl-emacs/commons:reexport-symbols package))

