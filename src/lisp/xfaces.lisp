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

(uiop:define-package :cl-emacs/xfaces
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/xfaces)
(log-enable :cl-emacs/xfaces :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* bitmap-spec-p ()
  #M"Value is non-nil if OBJECT is a valid bitmap specification.
A bitmap specification is either a string, a file name, or a list
(WIDTH HEIGHT DATA) where WIDTH is the pixel width of the bitmap,
HEIGHT is its height, and DATA is a string containing the bits of
the pixmap.  Bits are stored row by row, each row occupies
(WIDTH + 7)/8 bytes.

(fn OBJECT)"
  (error 'unimplemented-error))
(defun* clear-face-cache ()
  #M"Clear face caches on all frames.
Optional THOROUGHLY non-nil means try to free unused fonts, too.

(fn &optional THOROUGHLY)"
  (error 'unimplemented-error))
(defun* color-distance ()
  #M"Return an integer distance between COLOR1 and COLOR2 on FRAME.
COLOR1 and COLOR2 may be either strings containing the color name,
or lists of the form (RED GREEN BLUE), each in the range 0 to 65535 inclusive.
If FRAME is unspecified or nil, the current frame is used.
If METRIC is specified, it should be a function that accepts
two lists of the form (RED GREEN BLUE) aforementioned.
Despite the name, this is not a true distance metric as it does not satisfy
the triangle inequality.

(fn COLOR1 COLOR2 &optional FRAME METRIC)"
  (error 'unimplemented-error))
(defun* color-gray-p ()
  #M"Return non-nil if COLOR is a shade of gray (or white or black).
FRAME specifies the frame and thus the display for interpreting COLOR.
If FRAME is nil or omitted, use the selected frame.

(fn COLOR &optional FRAME)"
  (error 'unimplemented-error))
(defun* color-supported-p ()
  #M"Return non-nil if COLOR can be displayed on FRAME.
BACKGROUND-P non-nil means COLOR is used as a background.
Otherwise, this function tells whether it can be used as a foreground.
If FRAME is nil or omitted, use the selected frame.
COLOR must be a valid color name.

(fn COLOR &optional FRAME BACKGROUND-P)"
  (error 'unimplemented-error))
(defun* color-values-from-color-spec ()
  #M"Parse color SPEC as a numeric color and return (RED GREEN BLUE).
This function recognizes the following formats for SPEC:

 #RGB, where R, G and B are hex numbers of equal length, 1-4 digits each.
 rgb:R/G/B, where R, G, and B are hex numbers, 1-4 digits each.
 rgbi:R/G/B, where R, G and B are floating-point numbers in [0,1].

If SPEC is not in one of the above forms, return nil.

Each of the 3 integer members of the resulting list, RED, GREEN, and BLUE,
is normalized to have its value in [0,65535].

(fn SPEC)"
  (error 'unimplemented-error))
(defun* display-supports-face-attributes-p ()
  #M"Return non-nil if all the face attributes in ATTRIBUTES are supported.
The optional argument DISPLAY can be a display name, a frame, or
nil (meaning the selected frame's display).

For instance, to check whether the display supports underlining:

  (display-supports-face-attributes-p \\='(:underline t))

The definition of ‘supported' is somewhat heuristic, but basically means
that a face containing all the attributes in ATTRIBUTES, when merged
with the default face for display, can be represented in a way that's

 (1) different in appearance from the default face, and
 (2) ‘close in spirit' to what the attributes specify, if not exact.

Point (2) implies that a ‘:weight black' attribute will be satisfied by
any display that can display bold, and a ‘:foreground \"yellow\"' as long
as it can display a yellowish color, but ‘:slant italic' will _not_ be
satisfied by the tty display code's automatic substitution of a ‘dim'
face for italic.

(fn ATTRIBUTES &optional DISPLAY)"
  (error 'unimplemented-error))
(defun* face-attribute-relative-p ()
  #M"Check whether a face attribute value is relative.
Specifically, this function returns t if the attribute ATTRIBUTE
with the value VALUE is relative.

A relative value is one that doesn't entirely override whatever is
inherited from another face.  For most possible attributes,
the only relative value that users see is ‘unspecified'.
However, for :height, floating point values are also relative.

(fn ATTRIBUTE VALUE)"
  (error 'unimplemented-error))
(defun* face-attributes-as-vector ()
  #M"Return a vector of face attributes corresponding to PLIST.

(fn PLIST)"
  (error 'unimplemented-error))
(defun* face-font ()
  #M"Return the font name of face FACE, or nil if it is unspecified.
The font name is, by default, for ASCII characters.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
  The font default for a face is either nil, or a list
  of the form (bold), (italic) or (bold italic).
If FRAME is omitted or nil, use the selected frame.
If FRAME is anything but t, and the optional third argument CHARACTER
is given, return the font name used by FACE for CHARACTER on FRAME.

(fn FACE &optional FRAME CHARACTER)"
  (error 'unimplemented-error))
(defun* frame--face-hash-table ()
  #M"Return a hash table of frame-local faces defined on FRAME.
For internal use only.

(fn &optional FRAME)"
  (error 'unimplemented-error))
(defun* internal-copy-lisp-face ()
  #M"Copy face FROM to TO.
If FRAME is t, copy the global face definition of FROM.
Otherwise, copy the frame-local definition of FROM on FRAME.
If NEW-FRAME is a frame, copy that data into the frame-local
definition of TO on NEW-FRAME.  If NEW-FRAME is nil,
FRAME controls where the data is copied to.

The value is TO.

(fn FROM TO FRAME NEW-FRAME)"
  (error 'unimplemented-error))
(defun* internal-face-x-get-resource ()
  #M"Get the value of X resource RESOURCE, class CLASS.
Returned value is for the display of frame FRAME.  If FRAME is not
specified or nil, use selected frame.  This function exists because
ordinary ‘x-get-resource' doesn't take a frame argument.

(fn RESOURCE CLASS &optional FRAME)"
  (error 'unimplemented-error))
(defun* internal-get-lisp-face-attribute ()
  #M"Return face attribute KEYWORD of face SYMBOL.
If SYMBOL does not name a valid Lisp face or KEYWORD isn't a valid
face attribute name, signal an error.
If the optional argument FRAME is given, report on face SYMBOL in that
frame.  If FRAME is t, report on the defaults for face SYMBOL (for new
frames).  If FRAME is omitted or nil, use the selected frame.

(fn SYMBOL KEYWORD &optional FRAME)"
  (error 'unimplemented-error))
(defun* internal-lisp-face-attribute-values ()
  #M"Return a list of valid discrete values for face attribute ATTR.
Value is nil if ATTR doesn't have a discrete set of valid values.

(fn ATTR)"
  (error 'unimplemented-error))
(defun* internal-lisp-face-empty-p ()
  #M"True if FACE has no attribute specified.
If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame.

(fn FACE &optional FRAME)"
  (error 'unimplemented-error))
(defun* internal-lisp-face-equal-p ()
  #M"True if FACE1 and FACE2 are equal.
If the optional argument FRAME is given, report on FACE1 and FACE2 in that frame.
If FRAME is t, report on the defaults for FACE1 and FACE2 (for new frames).
If FRAME is omitted or nil, use the selected frame.

(fn FACE1 FACE2 &optional FRAME)"
  (error 'unimplemented-error))
(defun* internal-lisp-face-p ()
  #M"Return non-nil if FACE names a face.
FACE should be a symbol or string.
If optional second argument FRAME is non-nil, check for the
existence of a frame-local face with name FACE on that frame.
Otherwise check for the existence of a global face.

(fn FACE &optional FRAME)"
  (error 'unimplemented-error))
(defun* internal-make-lisp-face ()
  #M"Make FACE, a symbol, a Lisp face with all attributes nil.
If FACE was not known as a face before, create a new one.
If optional argument FRAME is specified, make a frame-local face
for that frame.  Otherwise operate on the global face definition.
Value is a vector of face attributes.

(fn FACE &optional FRAME)"
  (error 'unimplemented-error))
(defun* internal-merge-in-global-face ()
  #M"Add attributes from frame-default definition of FACE to FACE on FRAME.
Default face attributes override any local face attributes.

(fn FACE FRAME)"
  (error 'unimplemented-error))
(defun* internal-set-alternative-font-family-alist ()
  #M"Define alternative font families to try in face font selection.
ALIST is an alist of (FAMILY ALTERNATIVE1 ALTERNATIVE2 ...) entries.
Each ALTERNATIVE is tried in order if no fonts of font family FAMILY can
be found.  Value is ALIST.

(fn ALIST)"
  (error 'unimplemented-error))
(defun* internal-set-alternative-font-registry-alist ()
  #M"Define alternative font registries to try in face font selection.
ALIST is an alist of (REGISTRY ALTERNATIVE1 ALTERNATIVE2 ...) entries.
Each ALTERNATIVE is tried in order if no fonts of font registry REGISTRY can
be found.  Value is ALIST.

(fn ALIST)"
  (error 'unimplemented-error))
(defun* internal-set-font-selection-order ()
  #M"Set font selection order for face font selection to ORDER.
ORDER must be a list of length 4 containing the symbols ‘:width',
‘:height', ‘:weight', and ‘:slant'.  Face attributes appearing
first in ORDER are matched first, e.g. if ‘:height' appears before
‘:weight' in ORDER, font selection first tries to find a font with
a suitable height, and then tries to match the font weight.
Value is ORDER.

(fn ORDER)"
  (error 'unimplemented-error))
(defun* internal-set-lisp-face-attribute ()
  #M"Set attribute ATTR of FACE to VALUE.
FRAME being a frame means change the face on that frame.
FRAME nil means change the face of the selected frame.
FRAME t means change the default for new frames.
FRAME 0 means change the face on all frames, and change the default
  for new frames.

(fn FACE ATTR VALUE &optional FRAME)"
  (error 'unimplemented-error))
(defun* internal-set-lisp-face-attribute-from-resource ()
  #M"

(fn FACE ATTR VALUE &optional FRAME)"
  (error 'unimplemented-error))
(defun* merge-face-attribute ()
  #M"Return face ATTRIBUTE VALUE1 merged with VALUE2.
If VALUE1 or VALUE2 are absolute (see ‘face-attribute-relative-p'), then
the result will be absolute, otherwise it will be relative.

(fn ATTRIBUTE VALUE1 VALUE2)"
  (error 'unimplemented-error))
(defun* tty-suppress-bold-inverse-default-colors ()
  #M"Suppress/allow boldness of faces with inverse default colors.
SUPPRESS non-nil means suppress it.
This affects bold faces on TTYs whose foreground is the default background
color of the display and whose background is the default foreground color.
For such faces, the bold face attribute is ignored if this variable
is non-nil.

(fn SUPPRESS)"
  (error 'unimplemented-error))
(defun* x-family-fonts ()
  #M"Return a list of available fonts of family FAMILY on FRAME.
If FAMILY is omitted or nil, list all families.
Otherwise, FAMILY must be a string, possibly containing wildcards
‘?' and ‘*'.
If FRAME is omitted or nil, use the selected frame.

Each element of the result is a vector [FAMILY WIDTH POINT-SIZE WEIGHT
SLANT FIXED-P FULL REGISTRY-AND-ENCODING].

FAMILY is the font family name.
POINT-SIZE is the size of the font in 1/10 pt.
WIDTH, WEIGHT, and SLANT are symbols describing the width, weight
  and slant of the font.  These symbols are the same as for face
  attributes, see ‘set-face-attribute'.
FIXED-P is non-nil if the font is fixed-pitch.
FULL is the full name of the font.
REGISTRY-AND-ENCODING is a string giving the registry and encoding of
  the font.

The resulting list is sorted according to the current setting of
the face font sort order, see ‘face-font-selection-order'.

(fn &optional FAMILY FRAME)"
  (error 'unimplemented-error))
(defun* x-list-fonts ()
  #M"Return a list of the names of available fonts matching PATTERN.
If optional arguments FACE and FRAME are specified, return only fonts
the same size as FACE on FRAME.

PATTERN should be a string containing a font name in the XLFD,
Fontconfig, or GTK format.  A font name given in the XLFD format may
contain wildcard characters:
  the * character matches any substring, and
  the ? character matches any single character.
  PATTERN is case-insensitive.

The return value is a list of strings, suitable as arguments to
‘set-face-font'.

Fonts Emacs can't use may or may not be excluded
even if they match PATTERN and FACE.
The optional fourth argument MAXIMUM sets a limit on how many
fonts to match.  The first MAXIMUM fonts are reported.
The optional fifth argument WIDTH, if specified, is a number of columns
occupied by a character of a font.  In that case, return only fonts
the WIDTH times as wide as FACE on FRAME.

(fn PATTERN &optional FACE FRAME MAXIMUM WIDTH)"
  (error 'unimplemented-error))
