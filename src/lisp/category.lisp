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

(uiop:define-package :cl-emacs/category
    (:use
     :common-lisp
     :defstar
     :cl-emacs/lib/log
     :alexandria
     :fiveam
     :cl-emacs/lib/commons))
(in-package :cl-emacs/category)
(log-enable :cl-emacs/category :debug2)
(named-readtables:in-readtable elisp-function-syntax)
(defun* category-docstring ()
  #M"Return the documentation string of CATEGORY, as defined in TABLE.
TABLE should be a category table and defaults to the current buffer's
category table.

(fn CATEGORY &optional TABLE)"
  (error 'unimplemented-error))
(defun* category-set-mnemonics ()
  #M"Return a string containing mnemonics of the categories in CATEGORY-SET.
CATEGORY-SET is a bool-vector, and the categories \"in\" it are those
that are indexes where t occurs in the bool-vector.
The return value is a string containing those same categories.

(fn CATEGORY-SET)"
  (error 'unimplemented-error))
(defun* category-table ()
  #M"Return the current category table.
This is the one specified by the current buffer.

(fn)"
  (error 'unimplemented-error))
(defun* category-table-p ()
  #M"Return t if ARG is a category table.

(fn ARG)"
  (error 'unimplemented-error))
(defun* char-category-set ()
  #M"Return the category set of CHAR.

(fn CHAR)"
  (error 'unimplemented-error))
(defun* copy-category-table ()
  #M"Construct a new category table and return it.
It is a copy of the TABLE, which defaults to the standard category table.

(fn &optional TABLE)"
  (error 'unimplemented-error))
(defun* define-category ()
  #M"Define CATEGORY as a category which is described by DOCSTRING.
CATEGORY should be an ASCII printing character in the range ‘ ' to ‘~'.
DOCSTRING is the documentation string of the category.  The first line
should be a terse text (preferably less than 16 characters),
and the rest lines should be the full description.
The category is defined only in category table TABLE, which defaults to
the current buffer's category table.

(fn CATEGORY DOCSTRING &optional TABLE)"
  (error 'unimplemented-error))
(defun* get-unused-category ()
  #M"Return a category which is not yet defined in TABLE.
If no category remains available, return nil.
The optional argument TABLE specifies which category table to modify;
it defaults to the current buffer's category table.

(fn &optional TABLE)"
  (error 'unimplemented-error))
(defun* make-category-set ()
  #M"Return a newly created category-set which contains CATEGORIES.
CATEGORIES is a string of category mnemonics.
The value is a bool-vector which has t at the indices corresponding to
those categories.

(fn CATEGORIES)"
  (error 'unimplemented-error))
(defun* make-category-table ()
  #M"Construct a new and empty category table and return it.

(fn)"
  (error 'unimplemented-error))
(defun* modify-category-entry ()
  #M"Modify the category set of CHARACTER by adding CATEGORY to it.
The category is changed only for table TABLE, which defaults to
the current buffer's category table.
CHARACTER can be either a single character or a cons representing the
lower and upper ends of an inclusive character range to modify.
CATEGORY must be a category name (a character between ‘ ' and ‘~').
Use ‘describe-categories' to see existing category names.
If optional fourth argument RESET is non-nil,
then delete CATEGORY from the category set instead of adding it.

(fn CHARACTER CATEGORY &optional TABLE RESET)"
  (error 'unimplemented-error))
(defun* set-category-table ()
  #M"Specify TABLE as the category table for the current buffer.
Return TABLE.

(fn TABLE)"
  (error 'unimplemented-error))
(defun* standard-category-table ()
  #M"Return the standard category table.
This is the one used for new buffers.

(fn)"
  (error 'unimplemented-error))
