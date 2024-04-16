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
(uiop:define-package :cl-emacs/elisp/globals
    (:use :common-lisp :cl-emacs/log
          :cl-emacs/elisp/internals)
  )
(in-package :cl-emacs/elisp/globals)
(log-enable :cl-emacs/elisp/globals)


;; key - var symbol
;; value - default value
(defvar *defvar-defaults* (make-hash-table))
(defun-elisp elisp/init-globals '(:internal :rpc-debug) ()
  "set emacs global-vars to default values"
  (loop for var-sym being each hash-key of *defvar-defaults*
        do (setf (symbol-value var-sym) (gethash var-sym *defvar-defaults*))))

(defmacro defvar-elisp (var-name type init-value docstring)
  `(progn
     (declaim (,type ,var-name))
     (setf (gethash ',var-name *defvar-defaults*) ,init-value)
     ,(append (list 'defvar var-name init-value docstring))
     (export ',var-name)))

;; (defvar invocation-directory nil )
(defvar-elisp test1 fixnum 10
  "")
(defvar-elisp gcs-done fixnum 0
  "Accumulated number of garbage collections done.")
(defvar-elisp cairo-version-string string ""
  "Version info for cairo")
(defvar-elisp font-encoding-alist cons nil
  "Alist of fontname patterns vs the corresponding encoding and repertory info.
Each element looks like (REGEXP . (ENCODING . REPERTORY)),
where ENCODING is a charset or a char-table,
and REPERTORY is a charset, a char-table, or nil.

If ENCODING and REPERTORY are the same, the element can have the form
\(REGEXP . ENCODING).

ENCODING is for converting a character to a glyph code of the font.
If ENCODING is a charset, encoding a character by the charset gives
the corresponding glyph code.  If ENCODING is a char-table, looking up
the table by a character gives the corresponding glyph code.

REPERTORY specifies a repertory of characters supported by the font.
If REPERTORY is a charset, all characters belonging to the charset are
supported.  If REPERTORY is a char-table, all characters who have a
non-nil value in the table are supported.  If REPERTORY is nil, Emacs
gets the repertory information by an opened font and ENCODING.")


