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

(uiop:define-package :cl-emacs/casefiddle
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/casefiddle)
(log-enable :cl-emacs/casefiddle :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* capitalize () "Convert argument to capitalized form and return that.
This means that each word’s first character is converted to either
title case or upper case, and the rest to lower case.

The argument may be a character or string.  The result has the same
type.  (See ‘downcase’ for further details about the type.)

The argument object is not altered--the value is a copy.  If argument
is a character, characters which map to multiple code points when
cased, e.g. ﬁ, are returned unchanged.

(fn OBJ)"
  (error ’unimplemented-error))
(defun* capitalize-region () "Convert the region to capitalized form.
This means that each word’s first character is converted to either
title case or upper case, and the rest to lower case.
In programs, give two arguments, the starting and ending
character positions to operate on.

(fn BEG END &optional REGION-NONCONTIGUOUS-P)"
  (error ’unimplemented-error))
(defun* capitalize-word () "Capitalize from point to the end of word, moving over.
With numerical argument ARG, capitalize the next ARG-1 words as well.
This gives the word(s) a first character in upper case
and the rest lower case.

If point is in the middle of a word, the part of that word before point
is ignored when moving forward.

With negative argument, capitalize previous words but do not move.

(fn ARG)"
  (error ’unimplemented-error))
(defun* downcase () "Convert argument to lower case and return that.
The argument may be a character or string.  The result has the same type,
including the multibyteness of the string.

This means that if this function is called with a unibyte string
argument, and downcasing it would turn it into a multibyte string
(according to the current locale), the downcasing is done using ASCII
\"C\" rules instead.  To accurately downcase according to the current
locale, the string must be converted into multibyte first.

The argument object is not altered--the value is a copy.

(fn OBJ)"
  (error ’unimplemented-error))
(defun* downcase-region () "Convert the region to lower case.  In programs, wants two arguments.
These arguments specify the starting and ending character numbers of
the region to operate on.  When used as a command, the text between
point and the mark is operated on.

(fn BEG END &optional REGION-NONCONTIGUOUS-P)"
  (error ’unimplemented-error))
(defun* downcase-word () "Convert to lower case from point to end of word, moving over.

If point is in the middle of a word, the part of that word before point
is ignored when moving forward.

With negative argument, convert previous words but do not move.

(fn ARG)"
  (error ’unimplemented-error))
(defun* upcase () "Convert argument to upper case and return that.
The argument may be a character or string.  The result has the same
type.  (See ‘downcase’ for further details about the type.)

The argument object is not altered--the value is a copy.  If argument
is a character, characters which map to multiple code points when
cased, e.g. ﬁ, are returned unchanged.

See also ‘capitalize’, ‘downcase’ and ‘upcase-initials’.

(fn OBJ)"
  (error ’unimplemented-error))
(defun* upcase-initials () "Convert the initial of each word in the argument to upper case.
This means that each word’s first character is converted to either
title case or upper case, and the rest are left unchanged.

The argument may be a character or string.  The result has the same
type.  (See ‘downcase’ for further details about the type.)

The argument object is not altered--the value is a copy.  If argument
is a character, characters which map to multiple code points when
cased, e.g. ﬁ, are returned unchanged.

(fn OBJ)"
  (error ’unimplemented-error))
(defun* upcase-initials-region () "Upcase the initial of each word in the region.
This means that each word’s first character is converted to either
title case or upper case, and the rest are left unchanged.
In programs, give two arguments, the starting and ending
character positions to operate on.

(fn BEG END &optional REGION-NONCONTIGUOUS-P)"
  (error ’unimplemented-error))
(defun* upcase-region () "Convert the region to upper case.  In programs, wants two arguments.
These arguments specify the starting and ending character numbers of
the region to operate on.  When used as a command, the text between
point and the mark is operated on.
See also ‘capitalize-region’.

(fn BEG END &optional REGION-NONCONTIGUOUS-P)"
  (error ’unimplemented-error))
(defun* upcase-word () "Convert to upper case from point to end of word, moving over.

If point is in the middle of a word, the part of that word before point
is ignored when moving forward.

With negative argument, convert previous words but do not move.
See also ‘capitalize-word’.

(fn ARG)"
  (error ’unimplemented-error))
