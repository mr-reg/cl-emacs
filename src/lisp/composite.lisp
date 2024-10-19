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

(uiop:define-package :cl-emacs/composite
    (:use
     :common-lisp
     :defstar
     :cl-emacs/lib/log
     :alexandria
     :fiveam
     :cl-emacs/lib/commons))
(in-package :cl-emacs/composite)
(log-enable :cl-emacs/composite :debug2)
(named-readtables:in-readtable elisp-function-syntax)
(defun* clear-composition-cache ()
  #M"Internal use only.
Clear composition cache.

(fn)"
  (error 'unimplemented-error))
(defun* compose-region-internal ()
  #M"Internal use only.

Compose text in the region between START and END.
Optional 3rd and 4th arguments are COMPONENTS and MODIFICATION-FUNC
for the composition.  See ‘compose-region' for more details.

(fn START END &optional COMPONENTS MODIFICATION-FUNC)"
  (error 'unimplemented-error))
(defun* compose-string-internal ()
  #M"Internal use only.

Compose text between indices START and END of STRING, where
START and END are treated as in ‘substring'.  Optional 4th
and 5th arguments are COMPONENTS and MODIFICATION-FUNC
for the composition.  See ‘compose-string' for more details.

(fn STRING START END &optional COMPONENTS MODIFICATION-FUNC)"
  (error 'unimplemented-error))
(defun* composition-get-gstring ()
  #M"Return a glyph-string for characters between FROM and TO.
If the glyph string is for graphic display, FONT-OBJECT must be
a font-object to use for those characters.
Otherwise (for terminal display), FONT-OBJECT must be a terminal ID, a
frame, or nil for the selected frame's terminal device.

If the optional 4th argument STRING is not nil, it is a string
containing the target characters between indices FROM and TO,
which are treated as in ‘substring'.  Otherwise FROM and TO are
character positions in current buffer; they can be in either order,
and can be integers or markers.

A glyph-string is a vector containing information about how to display
a specific character sequence.  The format is:
   [HEADER ID GLYPH ...]

HEADER is a vector of this form:
    [FONT-OBJECT CHAR ...]
where
    FONT-OBJECT is a font-object for all glyphs in the glyph-string,
    or the terminal coding system of the specified terminal.
    CHARs are characters to be composed by GLYPHs.

ID is an identification number of the glyph-string.  It may be nil if
not yet shaped.

GLYPH is a vector whose elements have this form:
    [ FROM-IDX TO-IDX C CODE WIDTH LBEARING RBEARING ASCENT DESCENT
      [ [X-OFF Y-OFF WADJUST] | nil] ]
where
    FROM-IDX and TO-IDX are used internally and should not be touched.
    C is the character of the glyph.
    CODE is the glyph-code of C in FONT-OBJECT.
    WIDTH thru DESCENT are the metrics (in pixels) of the glyph.
    X-OFF and Y-OFF are offsets to the base position for the glyph.
    WADJUST is the adjustment to the normal width of the glyph.

If GLYPH is nil, the remaining elements of the glyph-string vector
should be ignored.

(fn FROM TO FONT-OBJECT STRING)"
  (error 'unimplemented-error))
(defun* composition-sort-rules ()
  #M"Sort composition RULES by their LOOKBACK parameter.

If RULES include just one rule, return RULES.
Otherwise, return a new list of rules where all the rules are
arranged in decreasing order of the LOOKBACK parameter of the
rules (the second element of the rule's vector).  This is required
when combining composition rules from different sources, because
of the way buffer text is examined for matching one of the rules.

(fn RULES)"
  (error 'unimplemented-error))
(defun* find-composition-internal ()
  #M"Internal use only.

Return information about composition at or nearest to position POS.
See ‘find-composition' for more details.

(fn POS LIMIT STRING DETAIL-P)"
  (error 'unimplemented-error))
