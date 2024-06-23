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

(uiop:define-package :cl-emacs/charset
    (:use
     :common-lisp
     :defstar
     :cl-emacs/lib/log
     :alexandria
     :fiveam
     :cl-emacs/lib/commons))
(in-package :cl-emacs/charset)
(log-enable :cl-emacs/charset :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* char-charset ()
  #M"Return the charset of highest priority that contains CH.
ASCII characters are an exception: for them, this function always
returns ‘ascii'.
If optional 2nd arg RESTRICTION is non-nil, it is a list of charsets
from which to find the charset.  It may also be a coding system.  In
that case, find the charset from what supported by that coding system.

(fn CH &optional RESTRICTION)"
  (error 'unimplemented-error))
(defun* charset-after ()
  #M"Return charset of a character in the current buffer at position POS.
If POS is nil, it defaults to the current point.
If POS is out of range, the value is nil.

(fn &optional POS)"
  (error 'unimplemented-error))
(defun* charset-id-internal ()
  #M"Internal use only.
Return charset identification number of CHARSET.

(fn &optional CHARSET)"
  (error 'unimplemented-error))
(defun* charset-plist ()
  #M"Return the property list of CHARSET.

(fn CHARSET)"
  (error 'unimplemented-error))
(defun* charset-priority-list ()
  #M"Return the list of charsets ordered by priority.
HIGHESTP non-nil means just return the highest priority one.

(fn &optional HIGHESTP)"
  (error 'unimplemented-error))
(defun* charsetp ()
  #M"Return non-nil if and only if OBJECT is a charset.

(fn OBJECT)"
  (error 'unimplemented-error))
(defun* clear-charset-maps ()
  #M"Internal use only.
Clear temporary charset mapping tables.
It should be called only from temacs invoked for dumping.

(fn)"
  (error 'unimplemented-error))
(defun* declare-equiv-charset ()
  #M"Declare an equivalent charset for ISO-2022 decoding.

On decoding by an ISO-2022 base coding system, when a charset
specified by DIMENSION, CHARS, and FINAL-CHAR is designated, behave as
if CHARSET is designated instead.

(fn DIMENSION CHARS FINAL-CHAR CHARSET)"
  (error 'unimplemented-error))
(defun* decode-char ()
  #M"Decode the pair of CHARSET and CODE-POINT into a character.
Return nil if CODE-POINT is not valid in CHARSET.

CODE-POINT may be a cons (HIGHER-16-BIT-VALUE . LOWER-16-BIT-VALUE),
although this usage is obsolescent.

(fn CHARSET CODE-POINT)"
  (error 'unimplemented-error))
(defun* define-charset-alias ()
  #M"Define ALIAS as an alias for charset CHARSET.

(fn ALIAS CHARSET)"
  (error 'unimplemented-error))
(defun* define-charset-internal ()
  #M"For internal use only.

(fn ...)"
  (error 'unimplemented-error))
(defun* encode-char ()
  #M"Encode the character CH into a code-point of CHARSET.
Return the encoded code-point as an integer,
or nil if CHARSET doesn't support CH.

(fn CH CHARSET)"
  (error 'unimplemented-error))
(defun* find-charset-region ()
  #M"Return a list of charsets in the region between BEG and END.
BEG and END are buffer positions.
Optional arg TABLE if non-nil is a translation table to look up.

If the current buffer is unibyte, the returned list may contain
only ‘ascii', ‘eight-bit-control', and ‘eight-bit-graphic'.

(fn BEG END &optional TABLE)"
  (error 'unimplemented-error))
(defun* find-charset-string ()
  #M"Return a list of charsets in STR.
Optional arg TABLE if non-nil is a translation table to look up.

If STR is unibyte, the returned list may contain
only ‘ascii', ‘eight-bit-control', and ‘eight-bit-graphic'.

(fn STR &optional TABLE)"
  (error 'unimplemented-error))
(defun* get-unused-iso-final-char ()
  #M"Return an unused ISO final char for a charset of DIMENSION and CHARS.
DIMENSION is the number of bytes to represent a character: 1 or 2.
CHARS is the number of characters in a dimension: 94 or 96.

This final char is for private use, thus the range is ‘0' (48) .. ‘?' (63).
If there's no unused final char for the specified kind of charset,
return nil.

(fn DIMENSION CHARS)"
  (error 'unimplemented-error))
(defun* iso-charset ()
  #M"Return charset of ISO's specification DIMENSION, CHARS, and FINAL-CHAR.

ISO 2022's designation sequence (escape sequence) distinguishes charsets
by their DIMENSION, CHARS, and FINAL-CHAR,
whereas Emacs distinguishes them by charset symbol.
See the documentation of the function ‘charset-info' for the meanings of
DIMENSION, CHARS, and FINAL-CHAR.

(fn DIMENSION CHARS FINAL-CHAR)"
  (error 'unimplemented-error))
(defun* make-char ()
  #M"Return a character of CHARSET whose position codes are CODEn.

CODE1 through CODE4 are optional, but if you don't supply sufficient
position codes, it is assumed that the minimum code in each dimension
is specified.

(fn CHARSET &optional CODE1 CODE2 CODE3 CODE4)"
  (error 'unimplemented-error))
(defun* map-charset-chars ()
  #M"Call FUNCTION for all characters in CHARSET.
Optional 3rd argument ARG is an additional argument to be passed
to FUNCTION, see below.
Optional 4th and 5th arguments FROM-CODE and TO-CODE specify the
range of code points (in CHARSET) of target characters on which to
map the FUNCTION.  Note that these are not character codes, but code
points of CHARSET; for the difference see ‘decode-char' and
‘list-charset-chars'.  If FROM-CODE is nil or imitted, it stands for
the first code point of CHARSET; if TO-CODE is nil or omitted, it
stands for the last code point of CHARSET.

FUNCTION will be called with two arguments: RANGE and ARG.
RANGE is a cons (FROM .  TO), where FROM and TO specify a range of
characters that belong to CHARSET on which FUNCTION should do its
job.  FROM and TO are Emacs character codes, unlike FROM-CODE and
TO-CODE, which are CHARSET code points.

(fn FUNCTION CHARSET &optional ARG FROM-CODE TO-CODE)"
  (error 'unimplemented-error))
(defun* set-charset-plist ()
  #M"Set CHARSET's property list to PLIST.

(fn CHARSET PLIST)"
  (error 'unimplemented-error))
(defun* set-charset-priority ()
  #M"Assign higher priority to the charsets given as arguments.

(fn &rest charsets)"
  (error 'unimplemented-error))
(defun* sort-charsets ()
  #M"Sort charset list CHARSETS by a priority of each charset.
Return the sorted list.  CHARSETS is modified by side effects.
See also ‘charset-priority-list' and ‘set-charset-priority'.

(fn CHARSETS)"
  (error 'unimplemented-error))
(defun* split-char ()
  #M"Return list of charset and one to four position-codes of CH.
The charset is decided by the current priority order of charsets.
A position-code is a byte value of each dimension of the code-point of
CH in the charset.

(fn CH)"
  (error 'unimplemented-error))
(defun* unify-charset ()
  #M"Unify characters of CHARSET with Unicode.
This means reading the relevant file and installing the table defined
by CHARSET's ‘:unify-map' property.

Optional second arg UNIFY-MAP is a file name string or a vector.  It has
the same meaning as the ‘:unify-map' attribute in the function
‘define-charset' (which see).

Optional third argument DEUNIFY, if non-nil, means to de-unify CHARSET.

(fn CHARSET &optional UNIFY-MAP DEUNIFY)"
  (error 'unimplemented-error))
