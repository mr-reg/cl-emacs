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

(uiop:define-package :cl-emacs/fringe
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/fringe)
(log-enable :cl-emacs/fringe :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* define-fringe-bitmap () "Define fringe bitmap BITMAP from BITS of size HEIGHT x WIDTH.
BITMAP is a symbol identifying the new fringe bitmap.
BITS is either a string or a vector of integers.
HEIGHT is height of bitmap.  If HEIGHT is nil, use length of BITS.
WIDTH must be an integer from 1 to 16, or nil which defaults to 8.  An
error is signaled if WIDTH is outside this range.
Optional fifth arg ALIGN may be one of ‘top’, ‘center’, or ‘bottom’,
indicating the positioning of the bitmap relative to the rows where it
is used; the default is to center the bitmap.  Fifth arg may also be a
list (ALIGN PERIODIC) where PERIODIC non-nil specifies that the bitmap
should be repeated.
If BITMAP already exists, the existing definition is replaced.

(fn BITMAP BITS &optional HEIGHT WIDTH ALIGN)"
  (error ’unimplemented-error))
(defun* destroy-fringe-bitmap () "Destroy fringe bitmap BITMAP.
If BITMAP overrides a standard fringe bitmap, the original bitmap is restored.

(fn BITMAP)"
  (error ’unimplemented-error))
(defun* fringe-bitmaps-at-pos () "Return fringe bitmaps of row containing position POS in window WINDOW.
If WINDOW is nil, use selected window.  If POS is nil, use value of point
in that window.  Return value is a list (LEFT RIGHT OV), where LEFT
is the symbol for the bitmap in the left fringe (or nil if no bitmap),
RIGHT is similar for the right fringe, and OV is non-nil if there is an
overlay arrow in the left fringe.
Return nil if POS is not visible in WINDOW.

(fn &optional POS WINDOW)"
  (error ’unimplemented-error))
(defun* set-fringe-bitmap-face () "Set face for fringe bitmap BITMAP to FACE.
FACE is merged with the ‘fringe’ face, so normally FACE should specify
only the foreground color.
If FACE is nil, reset face to default fringe face.

(fn BITMAP &optional FACE)"
  (error ’unimplemented-error))
