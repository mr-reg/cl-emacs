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

(uiop:define-package :cl-emacs/utils
    (:use :common-lisp :cl-emacs/lib/log)
  )
(in-package :cl-emacs/utils)
(log-enable :cl-emacs/utils)
;; (defun read-literal-string (stream delimiter arg)
;;   (declare (ignore arg))
;;   (loop for char = (cl:read-char stream nil stream)
;;         when (eq char stream)
;;           do (error "hit end of stream")
;;         until (char= char delimiter)
;;         collect char into chars
;;         finally (return (coerce chars 'string))))
;; (set-dispatch-macro-character #\# #\" #'read-literal-string)
