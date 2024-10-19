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

(uiop:define-package :cl-emacs/fontset
    (:use
     :common-lisp
     :defstar
     :cl-emacs/lib/log
     :alexandria
     :fiveam
     :cl-emacs/lib/commons))
(in-package :cl-emacs/fontset)
(log-enable :cl-emacs/fontset :debug2)
(named-readtables:in-readtable elisp-function-syntax)
(defun* fontset-font ()
  #M"Return a font name pattern for character CH in fontset NAME.
If NAME is t, find a pattern in the default fontset.
If NAME is nil, find a pattern in the fontset of the selected frame.

The value has the form (FAMILY . REGISTRY), where FAMILY is a font
family name and REGISTRY is a font registry name.  This is actually
the first font name pattern for CH in the fontset or in the default
fontset.

If the 2nd optional arg ALL is non-nil, return a list of all font name
patterns.

(fn NAME CH &optional ALL)"
  (error 'unimplemented-error))
(defun* fontset-info ()
  #M"Return information about a fontset FONTSET on frame FRAME.

FONTSET is a fontset name string, nil for the fontset of FRAME, or t
for the default fontset.  FRAME nil means the selected frame.

The value is a char-table whose elements have this form:

    ((FONT OPENED-FONT ...) ...)

FONT is a name of font specified for a range of characters.

OPENED-FONT is a name of a font actually opened.

The char-table has one extra slot.  If FONTSET is not the default
fontset, the value the extra slot is a char-table containing the
information about the derived fonts from the default fontset.  The
format is the same as above.

(fn FONTSET &optional FRAME)"
  (error 'unimplemented-error))
(defun* fontset-list ()
  #M"Return a list of all defined fontset names.

(fn)"
  (error 'unimplemented-error))
(defun* new-fontset ()
  #M"Create a new fontset NAME from font information in FONTLIST.

FONTLIST is an alist of scripts vs the corresponding font specification list.
Each element of FONTLIST has the form (SCRIPT FONT-SPEC ...), where a
character of SCRIPT is displayed by a font that matches one of
FONT-SPEC.

SCRIPT is a symbol that appears in the first extra slot of the
char-table ‘char-script-table'.

FONT-SPEC is a vector, a cons, or a string.  See the documentation of
‘set-fontset-font' for the meaning.

(fn NAME FONTLIST)"
  (error 'unimplemented-error))
(defun* query-fontset ()
  #M"Return the name of a fontset that matches PATTERN.
The value is nil if there is no matching fontset.
PATTERN can contain ‘*' or ‘?' as a wildcard
just as X font name matching algorithm allows.
If REGEXPP is non-nil, PATTERN is a regular expression.

(fn PATTERN &optional REGEXPP)"
  (error 'unimplemented-error))
(defun* set-fontset-font ()
  #M"Modify FONTSET to use font specification in FONT-SPEC for displaying CHARACTERS.

FONTSET should be a fontset name (a string); or nil, meaning the
fontset of FRAME; or t, meaning the default fontset.

CHARACTERS may be a single character to use FONT-SPEC for.

CHARACTERS may be a cons (FROM . TO), where FROM and TO are characters.
In that case, use FONT-SPEC for all the characters in the range
between FROM and TO (inclusive).

CHARACTERS may be a script symbol.  In that case, use FONT-SPEC for
all the characters that belong to the script.  See the variable
‘script-representative-chars' for the list of known scripts, and
see the variable ‘char-script-table' for the script of any specific
character.

CHARACTERS may be a charset symbol.  In that case, use FONT-SPEC for
all the characters in the charset.  See ‘list-character-sets' and
‘list-charset-chars' for the list of character sets and their
characters.

CHARACTERS may be nil.  In that case, use FONT-SPEC for any
character for which no font-spec is specified in FONTSET.

FONT-SPEC may one of these:
 * A font-spec object made by the function ‘font-spec' (which see).
 * A cons (FAMILY . REGISTRY), where FAMILY is a font family name and
   REGISTRY is a font registry name.  FAMILY may contain foundry
   name, and REGISTRY may contain encoding name.
 * A font name string.
 * nil, which explicitly specifies that there's no font for CHARACTERS.

Optional 4th argument FRAME is a frame whose fontset should be modified;
it is used if FONTSET is nil.  If FONTSET is nil and FRAME is omitted
or nil, that stands for the fontset of the selected frame.

Optional 5th argument ADD, if non-nil, specifies how to add FONT-SPEC
to the previously set font specifications for CHARACTERS.  If it is
‘prepend', FONT-SPEC is prepended to the existing font specifications.
If it is ‘append', FONT-SPEC is appended.  By default, FONT-SPEC
overwrites the previous settings.

(fn FONTSET CHARACTERS FONT-SPEC &optional FRAME ADD)"
  (error 'unimplemented-error))
