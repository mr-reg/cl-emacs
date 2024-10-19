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

(uiop:define-package :cl-emacs/lib/commons
    (:use 
     :common-lisp 
     :cl-emacs/lib/log 
     :alexandria 
     :fiveam
     :defstar)
  (:shadow #:code-char)
  (:export
   #:safe-code-char
   #:reexport-symbols
   #:safe-code
   ))
(in-package :cl-emacs/lib/commons)
(log-enable :cl-emacs/lib/commons :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)

(defvar *timer* 0)
(defun set-timer ()
  (let* ((new-time (get-internal-real-time))
         (time (- new-time *timer*)))
    (setq *timer* new-time)
    (* 1.0 (/ time internal-time-units-per-second))))

(defun* reexport-symbols (package-from)
  (do-external-symbols (sym package-from)
    (shadowing-import sym)
    (export sym)))


(defmacro import-cl-basics ()
  )

(defun safe-code-char (code)
  #M"sometimes emacs code relies on characters above unicode limit, so we need some safe way to
     process them as cl:char, even if it will cause some side-effects..."
  (let ((safe-code (mod code cl-unicode:+code-point-limit+)))
    (cl:code-char safe-code)))

