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

(uiop:define-package :cl-emacs/casetab
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/casetab)
(log-enable :cl-emacs/casetab :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* case-table-p ()
  #M"Return t if OBJECT is a case table.
See ‘set-case-table' for more information on these data structures.

(fn OBJECT)"
  (error 'unimplemented-error))
(defun* current-case-table ()
  #M"Return the case table of the current buffer.

(fn)"
  (error 'unimplemented-error))
(defun* set-case-table ()
  #M"Select a new case table for the current buffer.
A case table is a char-table which maps characters
to their lower-case equivalents.  It also has three \"extra\" slots
which may be additional char-tables or nil.
These slots are called UPCASE, CANONICALIZE and EQUIVALENCES.
UPCASE maps each non-upper-case character to its upper-case equivalent.
 (The value in UPCASE for an upper-case character is never used.)
 If lower and upper case characters are in 1-1 correspondence,
 you may use nil and the upcase table will be deduced from DOWNCASE.
CANONICALIZE maps each character to a canonical equivalent;
 any two characters that are related by case-conversion have the same
 canonical equivalent character; it may be nil, in which case it is
 deduced from DOWNCASE and UPCASE.
EQUIVALENCES is a map that cyclically permutes each equivalence class
 (of characters with the same canonical equivalent); it may be nil,
 in which case it is deduced from CANONICALIZE.

(fn TABLE)"
  (error 'unimplemented-error))
(defun* set-standard-case-table ()
  #M"Select a new standard case table for new buffers.
See ‘set-case-table' for more info on case tables.

(fn TABLE)"
  (error 'unimplemented-error))
(defun* standard-case-table ()
  #M"Return the standard case table.
This is the one used for new buffers.

(fn)"
  (error 'unimplemented-error))
