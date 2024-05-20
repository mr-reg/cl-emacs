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

(uiop:define-package :cl-emacs/commons
    (:use :common-lisp :cl-emacs/log :alexandria :fiveam)
  (:export 
   #:reexport-symbols
   #:unimplemented-error))
(in-package :cl-emacs/commons)
(log-enable :cl-emacs/commons :debug1)

(defvar *timer* 0)
(defun set-timer ()
  (let* ((new-time (get-internal-real-time))
         (time (- new-time *timer*)))
    (setq *timer* new-time)
    (* 1.0 (/ time internal-time-units-per-second))))

(defun reexport-symbols (package-from)
  (do-external-symbols (sym package-from)
    (shadowing-import sym)
    (export sym)))


(defclass unimplemented-error (error)
  ())
