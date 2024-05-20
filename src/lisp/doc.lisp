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

(uiop:define-package :cl-emacs/doc
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/doc)
(log-enable :cl-emacs/doc :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* Snarf-documentation () "Used during Emacs initialization to scan the ‘etc/DOC...’ file.
This searches the ‘etc/DOC...’ file for doc strings and
records them in function and variable definitions.
The function takes one argument, FILENAME, a string;
it specifies the file name (without a directory) of the DOC file.
That file is found in ‘../etc’ now; later, when the dumped Emacs is run,
the same file name is found in the ‘doc-directory’.

(fn FILENAME)"
  (error ’unimplemented-error))
(defun* documentation () "Return the documentation string of FUNCTION.
Unless a non-nil second argument RAW is given, the
string is passed through ‘substitute-command-keys’.

(fn FUNCTION &optional RAW)"
  (error ’unimplemented-error))
(defun* documentation-property () "Return the documentation string that is SYMBOL’s PROP property.
Third argument RAW omitted or nil means pass the result through
‘substitute-command-keys’ if it is a string.

This differs from ‘get’ in that it can refer to strings stored in the
‘etc/DOC’ file; and that it evaluates documentation properties that
aren’t strings.

(fn SYMBOL PROP &optional RAW)"
  (error ’unimplemented-error))
(defun* text-quoting-style () "Return the current effective text quoting style.
If the variable ‘text-quoting-style’ is ‘grave’, ‘straight’ or
‘curve’, just return that value.  If it is nil (the default), return
‘grave’ if curved quotes cannot be displayed (for instance, on a
terminal with no support for these characters), otherwise return
‘quote’.  Any other value is treated as ‘grave’.

Note that in contrast to the variable ‘text-quoting-style’, this
function will never return nil.

(fn)"
  (error ’unimplemented-error))
