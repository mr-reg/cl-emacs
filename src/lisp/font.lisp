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

(uiop:define-package :cl-emacs/font
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/font)
(log-enable :cl-emacs/font :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* clear-font-cache () "Clear font cache of each frame.

(fn)"
  (error ’unimplemented-error))
(defun* close-font () "Close FONT-OBJECT.

(fn FONT-OBJECT &optional FRAME)"
  (error ’unimplemented-error))
(defun* find-font () "Return a font-entity matching with FONT-SPEC on the current frame.
Optional 2nd argument FRAME, if non-nil, specifies the target frame.

(fn FONT-SPEC &optional FRAME)"
  (error ’unimplemented-error))
(defun* font-at () "Return a font-object for displaying a character at POSITION.
Optional second arg WINDOW, if non-nil, is a window displaying
the current buffer.  It defaults to the currently selected window.
Optional third arg STRING, if non-nil, is a string containing the target
character at index specified by POSITION.

(fn POSITION &optional WINDOW STRING)"
  (error ’unimplemented-error))
(defun* font-face-attributes () "Return a plist of face attributes generated by FONT.
FONT is a font name, a font-spec, a font-entity, or a font-object.
The return value is a list of the form

(:family FAMILY :height HEIGHT :weight WEIGHT :slant SLANT :width WIDTH)

where FAMILY, HEIGHT, WEIGHT, SLANT, and WIDTH are face attribute values
compatible with ‘set-face-attribute’.  Some of these key-attribute pairs
may be omitted from the list if they are not specified by FONT.

The optional argument FRAME specifies the frame that the face attributes
are to be displayed on.  If omitted, the selected frame is used.

(fn FONT &optional FRAME)"
  (error ’unimplemented-error))
(defun* font-family-list () "List available font families on the current frame.
If FRAME is omitted or nil, the selected frame is used.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* font-get () "Return the value of FONT’s property KEY.
FONT is a font-spec, a font-entity, or a font-object.
KEY can be any symbol, but these are reserved for specific meanings:
  :foundry, :family, :adstyle, :registry, :weight, :slant, :width,
  :size, :dpi, :spacing, :avgwidth, :script, :lang, :otf
See the documentation of ‘font-spec’ for their meanings.

If FONT is a font-entity or a font-object, then values of
:script and :otf properties are different from those of a font-spec
as below:

  The value of :script may be a list of scripts that are supported by
  the font.

  The value of :otf is a cons (GSUB . GPOS) where GSUB and GPOS are
  lists representing the OpenType features supported by the font, of
  this form: ((SCRIPT (LANGSYS FEATURE ...) ...) ...), where
  SCRIPT, LANGSYS, and FEATURE are all symbols representing OpenType
  Layout tags.  See ‘otf-script-alist’ for the OpenType script tags.

In addition to the keys listed above, the following keys are reserved
for the specific meanings as below:

  The value of :type is a symbol that identifies the font backend to be
  used, such as ‘ftcrhb’ or ‘xfthb’ on X , ‘harfbuzz’ or ‘uniscribe’ on
  MS-Windows, ‘ns’ on Cocoa/GNUstep, etc.

  The value of :combining-capability is non-nil if the font-backend of
  FONT supports rendering of combining characters for non-OTF fonts.

(fn FONT KEY)"
  (error ’unimplemented-error))
(defun* font-get-glyphs () "Return a vector of FONT-OBJECT’s glyphs for the specified characters.
FROM and TO are positions (integers or markers) specifying a region
of the current buffer, and can be in either order.  If the optional
fourth arg OBJECT is not nil, it is a string or a vector containing
the target characters between indices FROM and TO, which are treated
as in ‘substring’.

Each element is a vector containing information of a glyph in this format:
  [FROM-IDX TO-IDX C CODE WIDTH LBEARING RBEARING ASCENT DESCENT ADJUSTMENT]
where
  FROM is an index numbers of a character the glyph corresponds to.
  TO is the same as FROM.
  C is the character of the glyph.
  CODE is the glyph-code of C in FONT-OBJECT.
  WIDTH thru DESCENT are the metrics (in pixels) of the glyph.
  ADJUSTMENT is always nil.

If FONT-OBJECT doesn’t have a glyph for a character, the corresponding
element is nil.

Also see ‘font-has-char-p’, which is more efficient than this function
if you just want to check whether FONT-OBJECT has a glyph for a
character.

(fn FONT-OBJECT FROM TO &optional OBJECT)"
  (error ’unimplemented-error))
(defun* font-has-char-p () "Return non-nil if FONT on FRAME has a glyph for character CH.
FONT can be either a font-entity or a font-object.  If it is
a font-entity and the result is nil, it means the font needs to be
opened (with ‘open-font’) to check.
FRAME defaults to the selected frame if it is nil or omitted.

(fn FONT CH &optional FRAME)"
  (error ’unimplemented-error))
(defun* font-info () "Return information about a font named NAME on frame FRAME.
If FRAME is omitted or nil, use the selected frame.

The returned value is a vector of 14 elements:
  [ OPENED-NAME FULL-NAME SIZE HEIGHT BASELINE-OFFSET RELATIVE-COMPOSE
    DEFAULT-ASCENT MAX-WIDTH ASCENT DESCENT SPACE-WIDTH AVERAGE-WIDTH
    FILENAME CAPABILITY ]
where
  OPENED-NAME is the name used for opening the font,
  FULL-NAME is the full name of the font,
  SIZE is the pixelsize of the font,
  HEIGHT is the pixel-height of the font (i.e., ascent + descent),
  BASELINE-OFFSET is the upward offset pixels from ASCII baseline,
  RELATIVE-COMPOSE and DEFAULT-ASCENT are the numbers controlling
    how to compose characters,
  MAX-WIDTH is the maximum advance width of the font,
  ASCENT, DESCENT, SPACE-WIDTH, and AVERAGE-WIDTH are metrics of
    the font in pixels,
  FILENAME is the font file name, a string (or nil if the font backend
    doesn’t provide a file name).
  CAPABILITY is a list whose first element is a symbol representing the
    font format, one of ‘x’, ‘opentype’, ‘truetype’, ‘type1’, ‘pcf’, or ‘bdf’.
    The remaining elements describe the details of the font capabilities,
    as follows:

      If the font is OpenType font, the form of the list is
        (opentype GSUB GPOS)
      where GSUB shows which \"GSUB\" features the font supports, and GPOS
      shows which \"GPOS\" features the font supports.  Both GSUB and GPOS are
      lists of the form:
	((SCRIPT (LANGSYS FEATURE ...) ...) ...)

      where
        SCRIPT is a symbol representing OpenType script tag.
        LANGSYS is a symbol representing OpenType langsys tag, or nil
         representing the default langsys.
        FEATURE is a symbol representing OpenType feature tag.

      If the font is not an OpenType font, there are no elements
      in CAPABILITY except the font format symbol.

If the named font cannot be opened and loaded, return nil.

(fn NAME &optional FRAME)"
  (error ’unimplemented-error))
(defun* font-match-p () "Return t if and only if font-spec SPEC matches with FONT.
FONT is a font-spec, font-entity, or font-object.

(fn SPEC FONT)"
  (error ’unimplemented-error))
(defun* font-put () "Set one property of FONT: give property KEY value VAL.
FONT is a font-spec, a font-entity, or a font-object.

If FONT is a font-spec, KEY can be any symbol.  But if KEY is the one
accepted by the function ‘font-spec’ (which see), VAL must be what
allowed in ‘font-spec’.

If FONT is a font-entity or a font-object, KEY must not be the one
accepted by ‘font-spec’.

See also ‘font-get’ for KEYs that have special meanings.

(fn FONT PROP VAL)"
  (error ’unimplemented-error))
(defun* font-shape-gstring () "Shape the glyph-string GSTRING subject to bidi DIRECTION.
Shaping means substituting glyphs and/or adjusting positions of glyphs
to get the correct visual image of character sequences set in the
header of the glyph-string.

DIRECTION should be produced by the UBA, the Unicode Bidirectional
Algorithm, and should be a symbol, either L2R or R2L.  It can also
be nil if the bidi context is unknown.

If the shaping was successful, the value is GSTRING itself or a newly
created glyph-string.  Otherwise, the value is nil.

See the documentation of ‘composition-get-gstring’ for the format of
GSTRING.

(fn GSTRING DIRECTION)"
  (error ’unimplemented-error))
(defun* font-spec () "Return a newly created font-spec with arguments as properties.

ARGS must come in pairs KEY VALUE of font properties.  KEY must be a
valid font property name listed below:

‘:family’, ‘:weight’, ‘:slant’, ‘:width’

They are the same as face attributes of the same name.  See
‘set-face-attribute’.

‘:foundry’

VALUE must be a string or a symbol specifying the font foundry, e.g. ‘misc’.

‘:adstyle’

VALUE must be a string or a symbol specifying the additional
typographic style information of a font, e.g. ‘sans’.

‘:registry’

VALUE must be a string or a symbol specifying the charset registry and
encoding of a font, e.g. ‘iso8859-1’.

‘:size’

VALUE must be a non-negative integer or a floating point number
specifying the font size.  It specifies the font size in pixels (if
VALUE is an integer), or in points (if VALUE is a float).

‘:dpi’

VALUE must be a non-negative number that specifies the resolution
(dot per inch) for which the font is designed.

‘:spacing’

VALUE specifies the spacing of the font: mono, proportional, charcell,
or dual.  It can be either a number (0 for proportional, 90 for dual,
100 for mono, 110 for charcell) or a 1-letter symbol: ‘P’, ‘D’, ‘M’,
or ‘C’ (lower-case variants are also accepted).

‘:avgwidth’

VALUE must be a non-negative integer specifying the average width of
the font in 1/10 pixel units.

‘:name’

VALUE must be a string of XLFD-style or fontconfig-style font name.

‘:script’

VALUE must be a symbol representing a script that the font must
support.  It may be a symbol representing a subgroup of a script
listed in the variable ‘script-representative-chars’.

‘:lang’

VALUE must be a symbol whose name is a two-letter ISO-639 language
name, e.g. ‘ja’.  The value is matched against the \"Additional Style\"
field of the XLFD spec of a font, if it’s non-empty, on X, and
against the codepages supported by the font on w32.

‘:otf’

VALUE must be a list (SCRIPT-TAG LANGSYS-TAG GSUB [ GPOS ]) to specify
required OpenType features.

  SCRIPT-TAG: OpenType script tag symbol (e.g. ‘deva’).
  LANGSYS-TAG: OpenType language system tag symbol,
     or nil for the default language system.
  GSUB: List of OpenType GSUB feature tag symbols, or nil if none required.
  GPOS: List of OpenType GPOS feature tag symbols, or nil if none required.

GSUB and GPOS may contain nil elements.  In such a case, the font
must not have any of the remaining elements.

For instance, if the VALUE is ‘(thai nil nil (mark))’, the font must
be an OpenType font whose GPOS table of ‘thai’ script’s default
language system must contain ‘mark’ feature.

(fn ARGS...)"
  (error ’unimplemented-error))
(defun* font-variation-glyphs () "Return a list of variation glyphs for CHARACTER in FONT-OBJECT.
Each element of the value is a cons (VARIATION-SELECTOR . GLYPH-ID),
where
  VARIATION-SELECTOR is a character code of variation selector
    (#xFE00..#xFE0F or #xE0100..#xE01EF).
  GLYPH-ID is a glyph code of the corresponding variation glyph, an integer.

(fn FONT-OBJECT CHARACTER)"
  (error ’unimplemented-error))
(defun* font-xlfd-name () "Return XLFD name of FONT.
FONT is a font-spec, font-entity, or font-object.
If the name is too long for XLFD (maximum 255 chars), return nil.
If the 2nd optional arg FOLD-WILDCARDS is non-nil,
the consecutive wildcards are folded into one.

(fn FONT &optional FOLD-WILDCARDS)"
  (error ’unimplemented-error))
(defun* fontp () "Return t if OBJECT is a font-spec, font-entity, or font-object.
Return nil otherwise.
Optional 2nd argument EXTRA-TYPE, if non-nil, specifies to check
which kind of font it is.  It must be one of ‘font-spec’, ‘font-entity’,
‘font-object’.

(fn OBJECT &optional EXTRA-TYPE)"
  (error ’unimplemented-error))
(defun* frame-font-cache () "Return FRAME’s font cache.  Mainly used for debugging.
If FRAME is omitted or nil, use the selected frame.

(fn &optional FRAME)"
  (error ’unimplemented-error))
(defun* internal-char-font () "For internal use only.

(fn POSITION &optional CH)"
  (error ’unimplemented-error))
(defun* list-fonts () "List available fonts matching FONT-SPEC on the current frame.
Optional 2nd argument FRAME specifies the target frame.
Optional 3rd argument NUM, if non-nil, limits the number of returned fonts.
Optional 4th argument PREFER, if non-nil, is a font-spec to
control the order of the returned list.  Fonts are sorted by
how close they are to PREFER.

(fn FONT-SPEC &optional FRAME NUM PREFER)"
  (error ’unimplemented-error))
(defun* open-font () "Open FONT-ENTITY.

(fn FONT-ENTITY &optional SIZE FRAME)"
  (error ’unimplemented-error))
(defun* query-font () "Return information about FONT-OBJECT.
The value is a vector:
  [ NAME FILENAME PIXEL-SIZE SIZE ASCENT DESCENT SPACE-WIDTH AVERAGE-WIDTH
    CAPABILITY ]

NAME is the font name, a string (or nil if the font backend doesn’t
provide a name).

FILENAME is the font file name, a string (or nil if the font backend
doesn’t provide a file name).

PIXEL-SIZE is a pixel size by which the font is opened.

SIZE is a maximum advance width of the font in pixels.

ASCENT, DESCENT, SPACE-WIDTH, AVERAGE-WIDTH are metrics of the font in
pixels.

CAPABILITY is a list whose first element is a symbol representing the
font format (x, opentype, truetype, type1, pcf, or bdf) and the
remaining elements describe the details of the font capability.

If the font is OpenType font, the form of the list is
  (opentype GSUB GPOS)
where GSUB shows which \"GSUB\" features the font supports, and GPOS
shows which \"GPOS\" features the font supports.  Both GSUB and GPOS are
lists of the format:
  ((SCRIPT (LANGSYS FEATURE ...) ...) ...)

If the font is not OpenType font, currently the length of the form is
one.

SCRIPT is a symbol representing OpenType script tag.

LANGSYS is a symbol representing OpenType langsys tag, or nil
representing the default langsys.

FEATURE is a symbol representing OpenType feature tag.

If the font is not OpenType font, CAPABILITY is nil.

(fn FONT-OBJECT)"
  (error ’unimplemented-error))
