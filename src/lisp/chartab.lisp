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

(cl-emacs/elisp-packages:define-elisp-package :cl-emacs/chartab
    (:use
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/chartab)
(log-enable :cl-emacs/chartab :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* char-table-extra-slot ()
  #M"Return the value of CHAR-TABLE's extra-slot number N.

(fn CHAR-TABLE N)"
  (error 'unimplemented-error))
(defun* char-table-parent ()
  #M"Return the parent char-table of CHAR-TABLE.
The value is either nil or another char-table.
If CHAR-TABLE holds nil for a given character,
then the actual applicable value is inherited from the parent char-table
(or from its parents, if necessary).

(fn CHAR-TABLE)"
  (error 'unimplemented-error))
(defun* char-table-range ()
  #M"Return the value in CHAR-TABLE for a range of characters RANGE.
RANGE should be nil (for the default value),
a cons of character codes (for characters in the range), or a character code.

(fn CHAR-TABLE RANGE)"
  (error 'unimplemented-error))
(defun* char-table-subtype ()
  #M"Return the subtype of char-table CHAR-TABLE.  The value is a symbol.

(fn CHAR-TABLE)"
  (error 'unimplemented-error))
(defun* get-unicode-property-internal ()
  #M"Return an element of CHAR-TABLE for character CH.
CHAR-TABLE must be what returned by ‘unicode-property-table-internal'.

(fn CHAR-TABLE CH)"
  (error 'unimplemented-error))
(defun* make-char-table ()
  #M"Return a newly created char-table, with purpose PURPOSE.
Each element is initialized to INIT, which defaults to nil.

PURPOSE should be a symbol.  If it has a ‘char-table-extra-slots'
property, the property's value should be an integer between 0 and 10
that specifies how many extra slots the char-table has.  Otherwise,
the char-table has no extra slot.

(fn PURPOSE &optional INIT)"
  (error 'unimplemented-error))
(defun* map-char-table ()
  #M"Call FUNCTION for each character in CHAR-TABLE that has non-nil value.
FUNCTION is called with two arguments, KEY and VALUE.
KEY is a character code or a cons of character codes specifying a
range of characters that have the same value.
VALUE is what (char-table-range CHAR-TABLE KEY) returns.

(fn FUNCTION CHAR-TABLE)"
  (error 'unimplemented-error))
(defun* optimize-char-table ()
  #M"Optimize CHAR-TABLE.
TEST is the comparison function used to decide whether two entries are
equivalent and can be merged.  It defaults to ‘equal'.

(fn CHAR-TABLE &optional TEST)"
  (error 'unimplemented-error))
(defun* put-unicode-property-internal ()
  #M"Set an element of CHAR-TABLE for character CH to VALUE.
CHAR-TABLE must be what returned by ‘unicode-property-table-internal'.

(fn CHAR-TABLE CH VALUE)"
  (error 'unimplemented-error))
(defun* set-char-table-extra-slot ()
  #M"Set CHAR-TABLE's extra-slot number N to VALUE.

(fn CHAR-TABLE N VALUE)"
  (error 'unimplemented-error))
(defun* set-char-table-parent ()
  #M"Set the parent char-table of CHAR-TABLE to PARENT.
Return PARENT.  PARENT must be either nil or another char-table.

(fn CHAR-TABLE PARENT)"
  (error 'unimplemented-error))
(defun* set-char-table-range ()
  #M"Set the value in CHAR-TABLE for a range of characters RANGE to VALUE.
RANGE should be t (for all characters), nil (for the default value),
a cons of character codes (for characters in the range),
or a character code.  Return VALUE.

(fn CHAR-TABLE RANGE VALUE)"
  (error 'unimplemented-error))
(defun* unicode-property-table-internal ()
  #M"Return a char-table for Unicode character property PROP.
Use ‘get-unicode-property-internal' and
‘put-unicode-property-internal' instead of ‘aref' and ‘aset' to get
and put an element value.

(fn PROP)"
  (error 'unimplemented-error))

;; (let ((ct (make-char-table 'test 10)))
;;   (set-char-table-range ct nil 3)
;;   (prin1 ct)
;;   ;; (map-char-table '(lambda (x y)
;;   ;;                    (message "%s=%s" x y))
;;   ;;                 ct)
;;   )
