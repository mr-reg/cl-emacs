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

(cl-emacs/lib/elisp-packages:define-elisp-package :cl-emacs/editfns
    (:use
     :alexandria
     :fiveam
     )
  (:local-nicknames (#:el #:cl-emacs/elisp)
                    (#:pstrings #:cl-emacs/types/pstrings)
                    )  )
(in-package :cl-emacs/editfns)
(log-enable :cl-emacs/editfns :debug2)
(named-readtables:in-readtable elisp-function-syntax)
(def-suite cl-emacs/editfns)
(in-suite cl-emacs/editfns)

(defun* bobp ()
  "Return t if point is at the beginning of the buffer.
If the buffer is narrowed, this means the beginning of the narrowed part.

(fn)"
  (error 'unimplemented-error))
(defun* bolp ()
  #M"Return t if point is at the beginning of a line.

(fn)"
  (error 'unimplemented-error))
(defun* buffer-size ()
  #M"Return the number of characters in the current buffer.
If BUFFER is not nil, return the number of characters in that buffer
instead.

This does not take narrowing into account; to count the number of
characters in the accessible portion of the current buffer, use
‘(- (point-max) (point-min))', and to count the number of characters
in the accessible portion of some other BUFFER, use
‘(with-current-buffer BUFFER (- (point-max) (point-min)))'.

(fn &optional BUFFER)"
  (error 'unimplemented-error))
(defun* buffer-string ()
  #M"Return the contents of the current buffer as a string.
If narrowing is in effect, this function returns only the visible part
of the buffer.

This function copies the text properties of that part of the buffer
into the result string; if you don't want the text properties,
use ‘buffer-substring-no-properties' instead.

(fn)"
  (error 'unimplemented-error))
(defun* buffer-substring ()
  #M"Return the contents of part of the current buffer as a string.
The two arguments START and END are character positions;
they can be in either order.
The string returned is multibyte if the buffer is multibyte.

This function copies the text properties of that part of the buffer
into the result string; if you don't want the text properties,
use ‘buffer-substring-no-properties' instead.

(fn START END)"
  (error 'unimplemented-error))
(defun* buffer-substring-no-properties ()
  #M"Return the characters of part of the buffer, without the text properties.
The two arguments START and END are character positions;
they can be in either order.

(fn START END)"
  (error 'unimplemented-error))
(defun* byte-to-position ()
  #M"Return the character position for byte position BYTEPOS.
If BYTEPOS is out of range, the value is nil.

(fn BYTEPOS)"
  (error 'unimplemented-error))
(defun* byte-to-string ()
  #M"Convert arg BYTE to a unibyte string containing that byte.

(fn BYTE)"
  (error 'unimplemented-error))
(defun* char-after ()
  #M"Return character in current buffer at position POS.
POS is an integer or a marker and defaults to point.
If POS is out of range, the value is nil.

(fn &optional POS)"
  (error 'unimplemented-error))
(defun* char-before ()
  #M"Return character in current buffer preceding position POS.
POS is an integer or a marker and defaults to point.
If POS is out of range, the value is nil.

(fn &optional POS)"
  (error 'unimplemented-error))
(defun* char-equal ()
  #M"Return t if two characters match, optionally ignoring case.
Both arguments must be characters (i.e. integers).
Case is ignored if ‘case-fold-search' is non-nil in the current buffer.

(fn C1 C2)"
  (error 'unimplemented-error))
(defun* char-to-string ()
  #M"Convert arg CHAR to a string containing that character.

(fn CHAR)"
  (error 'unimplemented-error))
(defun* compare-buffer-substrings ()
  #M"Compare two substrings of two buffers; return result as number.
Return -N if first string is less after N-1 chars, +N if first string is
greater after N-1 chars, or 0 if strings match.
The first substring is in BUFFER1 from START1 to END1 and the second
is in BUFFER2 from START2 to END2.
All arguments may be nil.  If BUFFER1 or BUFFER2 is nil, the current
buffer is used.  If START1 or START2 is nil, the value of ‘point-min'
in the respective buffers is used.  If END1 or END2 is nil, the value
of ‘point-max' in the respective buffers is used.
The value of ‘case-fold-search' in the current buffer
determines whether case is significant or ignored.

(fn BUFFER1 START1 END1 BUFFER2 START2 END2)"
  (error 'unimplemented-error))
(defun* constrain-to-field ()
  #M"Return the position closest to NEW-POS that is in the same field as OLD-POS.
A field is a region of text with the same ‘field' property.

If NEW-POS is nil, then use the current point instead, and move point
to the resulting constrained position, in addition to returning that
position.

If OLD-POS is at the boundary of two fields, then the allowable
positions for NEW-POS depends on the value of the optional argument
ESCAPE-FROM-EDGE: If ESCAPE-FROM-EDGE is nil, then NEW-POS is
constrained to the field that has the same ‘field' char-property
as any new characters inserted at OLD-POS, whereas if ESCAPE-FROM-EDGE
is non-nil, NEW-POS is constrained to the union of the two adjacent
fields.  Additionally, if two fields are separated by another field with
the special value ‘boundary', then any point within this special field is
also considered to be ‘on the boundary'.

If the optional argument ONLY-IN-LINE is non-nil and constraining
NEW-POS would move it to a different line, NEW-POS is returned
unconstrained.  This is useful for commands that move by line, like
\\[next-line] or \\[beginning-of-line], which should generally respect field boundaries
only in the case where they can still move to the right line.

If the optional argument INHIBIT-CAPTURE-PROPERTY is non-nil, and OLD-POS has
a non-nil property of that name, then any field boundaries are ignored.

Field boundaries are not noticed if ‘inhibit-field-text-motion' is non-nil.

(fn NEW-POS OLD-POS &optional ESCAPE-FROM-EDGE ONLY-IN-LINE INHIBIT-CAPTURE-PROPERTY)"
  (error 'unimplemented-error))
(defun* current-message ()
  #M"Return the string currently displayed in the echo area, or nil if none.

(fn)"
  (error 'unimplemented-error))
(defun* delete-and-extract-region ()
  #M"Delete the text between START and END and return it.

(fn START END)"
  (error 'unimplemented-error))
(defun* delete-field ()
  #M"Delete the field surrounding POS.
A field is a region of text with the same ‘field' property.
If POS is nil, the value of point is used for POS.

(fn &optional POS)"
  (error 'unimplemented-error))
(defun* delete-region ()
  #M"Delete the text between START and END.
If called interactively, delete the region between point and mark.
This command deletes buffer text without modifying the kill ring.

(fn START END)"
  (error 'unimplemented-error))
(defun* emacs-pid ()
  #M"Return the process ID of Emacs, as an integer.

(fn)"
  (error 'unimplemented-error))
(defun* eobp ()
  #M"Return t if point is at the end of the buffer.
If the buffer is narrowed, this means the end of the narrowed part.

(fn)"
  (error 'unimplemented-error))
(defun* eolp ()
  #M"Return t if point is at the end of a line.
‘End of a line' includes point being at the end of the buffer.

(fn)"
  (error 'unimplemented-error))
(defun* field-beginning ()
  #M"Return the beginning of the field surrounding POS.
A field is a region of text with the same ‘field' property.
If POS is nil, the value of point is used for POS.
If ESCAPE-FROM-EDGE is non-nil and POS is at the beginning of its
field, then the beginning of the *previous* field is returned.
If LIMIT is non-nil, it is a buffer position; if the beginning of the field
is before LIMIT, then LIMIT will be returned instead.

(fn &optional POS ESCAPE-FROM-EDGE LIMIT)"
  (error 'unimplemented-error))
(defun* field-end ()
  #M"Return the end of the field surrounding POS.
A field is a region of text with the same ‘field' property.
If POS is nil, the value of point is used for POS.
If ESCAPE-FROM-EDGE is non-nil and POS is at the end of its field,
then the end of the *following* field is returned.
If LIMIT is non-nil, it is a buffer position; if the end of the field
is after LIMIT, then LIMIT will be returned instead.

(fn &optional POS ESCAPE-FROM-EDGE LIMIT)"
  (error 'unimplemented-error))
(defun* field-string ()
  #M"Return the contents of the field surrounding POS as a string.
A field is a region of text with the same ‘field' property.
If POS is nil, the value of point is used for POS.

(fn &optional POS)"
  (error 'unimplemented-error))
(defun* field-string-no-properties ()
  #M"Return the contents of the field around POS, without text properties.
A field is a region of text with the same ‘field' property.
If POS is nil, the value of point is used for POS.

(fn &optional POS)"
  (error 'unimplemented-error))
(defun* following-char ()
  #M"Return the character following point, as a number.
At the end of the buffer or accessible region, return 0.

(fn)"
  (error 'unimplemented-error))

(defun* format (format &rest objects)
  #M"Format a string out of a format-string and arguments.
The first argument is a format control string.
The other arguments are substituted into it to make the result, a string.

The format control string may contain %-sequences meaning to substitute
the next available argument, or the argument explicitly specified:

%s means produce a string argument.  Actually, produces any object with `princ'.
%d means produce as signed number in decimal.
%o means produce a number in octal.
%x means produce a number in hex.
%X is like %x, but uses upper case.
%e means produce a number in exponential notation.
%f means produce a number in decimal-point notation.
%g means produce a number in exponential notation if the exponent would be
   less than -4 or greater than or equal to the precision (default: 6);
   otherwise it produces in decimal-point notation.
%c means produce a number as a single character.
%S means produce any object as an s-expression (using `prin1').

The argument used for %d, %o, %x, %e, %f, %g or %c must be a number.
%o, %x, and %X treat arguments as unsigned if `binary-as-unsigned' is t
  (this is experimental; email 32252@debbugs.gnu.org if you need it).
Use %% to put a single % into the output.

A %-sequence other than %% may contain optional field number, flag,
width, and precision specifiers, as follows:

  %<field><flags><width><precision>character

where field is [0-9]+ followed by a literal dollar \"$\", flags is
[+ #0-]+, width is [0-9]+, and precision is a literal period \".\"
followed by [0-9]+.

If a %-sequence is numbered with a field with positive value N, the
Nth argument is substituted instead of the next one.  A format can
contain either numbered or unnumbered %-sequences but not both, except
that %% can be mixed with numbered %-sequences.

The + flag character inserts a + before any nonnegative number, while a
space inserts a space before any nonnegative number; these flags
affect only numeric %-sequences, and the + flag takes precedence.
The - and 0 flags affect the width specifier, as described below.

The # flag means to use an alternate display form for %o, %x, %X, %e,
%f, and %g sequences: for %o, it ensures that the result begins with
\"0\"; for %x and %X, it prefixes nonzero results with \"0x\" or \"0X\";
for %e and %f, it causes a decimal point to be included even if the
precision is zero; for %g, it causes a decimal point to be
included even if the precision is zero, and also forces trailing
zeros after the decimal point to be left in place.

The width specifier supplies a lower limit for the length of the
produced representation.  The padding, if any, normally goes on the
left, but it goes on the right if the - flag is present.  The padding
character is normally a space, but it is 0 if the 0 flag is present.
The 0 flag is ignored if the - flag is present, or the format sequence
is something other than %d, %o, %x, %e, %f, and %g.

For %e and %f sequences, the number after the \".\" in the precision
specifier says how many decimal places to show; if zero, the decimal
point itself is omitted.  For %g, the precision specifies how many
significant digits to produce; zero or omitted are treated as 1.
For %s and %S, the precision specifier truncates the string to the
given width.

Text properties, if any, are copied from the format-string to the
produced text.

usage: (format STRING &rest OBJECTS) "
  (error 'unimplemented-error))

(defun* format-message ()
  #M"Format a string out of a format-string and arguments.
The first argument is a format control string.
The other arguments are substituted into it to make the result, a string.

This acts like ‘format', except it also replaces each grave accent (\\=‘)
by a left quote, and each apostrophe (\\=') by a right quote.  The left
and right quote replacement characters are specified by
‘text-quoting-style'.

(fn STRING &rest OBJECTS)"
  (error 'unimplemented-error))
(defun* gap-position ()
  #M"Return the position of the gap, in the current buffer.
See also ‘gap-size'.

(fn)"
  (error 'unimplemented-error))
(defun* gap-size ()
  #M"Return the size of the current buffer's gap.
See also ‘gap-position'.

(fn)"
  (error 'unimplemented-error))
(defun* get-pos-property ()
  #M"Return the value of POSITION's property PROP, in OBJECT.
Almost identical to ‘get-char-property' except for the following difference:
Whereas ‘get-char-property' returns the property of the char at (i.e. right
after) POSITION, this pays attention to properties's stickiness and overlays's
advancement settings, in order to find the property of POSITION itself,
i.e. the property that a char would inherit if it were inserted
at POSITION.

(fn POSITION PROP &optional OBJECT)"
  (error 'unimplemented-error))
(defun* goto-char ()
  #M"Set point to POSITION, a number or marker.
Beginning of buffer is position (point-min), end is (point-max).

The return value is POSITION.

If called interactively, a numeric prefix argument specifies
POSITION; without a numeric prefix argument, read POSITION from the
minibuffer.  The default value is the number at point (if any).

(fn POSITION)"
  (error 'unimplemented-error))
(defun* group-gid ()
  #M"Return the effective gid of Emacs, as an integer.

(fn)"
  (error 'unimplemented-error))
(defun* group-name ()
  #M"Return the name of the group whose numeric group ID is GID.
The argument GID should be an integer or a float.
Return nil if a group with such GID does not exists or is not known.

(fn GID)"
  (error 'unimplemented-error))
(defun* group-real-gid ()
  #M"Return the real gid of Emacs, as an integer.

(fn)"
  (error 'unimplemented-error))
(defun* insert ()
  #M"Insert the arguments, either strings or characters, at point.
Point and after-insertion markers move forward to end up
 after the inserted text.
Any other markers at the point of insertion remain before the text.

If the current buffer is multibyte, unibyte strings are converted
to multibyte for insertion (see ‘string-make-multibyte').
If the current buffer is unibyte, multibyte strings are converted
to unibyte for insertion (see ‘string-make-unibyte').

When operating on binary data, it may be necessary to preserve the
original bytes of a unibyte string when inserting it into a multibyte
buffer; to accomplish this, apply ‘string-as-multibyte' to the string
and insert the result.

(fn &rest ARGS)"
  (error 'unimplemented-error))
(defun* insert-and-inherit ()
  #M"Insert the arguments at point, inheriting properties from adjoining text.
Point and after-insertion markers move forward to end up
 after the inserted text.
Any other markers at the point of insertion remain before the text.

If the current buffer is multibyte, unibyte strings are converted
to multibyte for insertion (see ‘unibyte-char-to-multibyte').
If the current buffer is unibyte, multibyte strings are converted
to unibyte for insertion.

(fn &rest ARGS)"
  (error 'unimplemented-error))
(defun* insert-before-markers ()
  #M"Insert strings or characters at point, relocating markers after the text.
Point and markers move forward to end up after the inserted text.

If the current buffer is multibyte, unibyte strings are converted
to multibyte for insertion (see ‘unibyte-char-to-multibyte').
If the current buffer is unibyte, multibyte strings are converted
to unibyte for insertion.

If an overlay begins at the insertion point, the inserted text falls
outside the overlay; if a nonempty overlay ends at the insertion
point, the inserted text falls inside that overlay.

(fn &rest ARGS)"
  (error 'unimplemented-error))
(defun* insert-before-markers-and-inherit ()
  #M"Insert text at point, relocating markers and inheriting properties.
Point and markers move forward to end up after the inserted text.

If the current buffer is multibyte, unibyte strings are converted
to multibyte for insertion (see ‘unibyte-char-to-multibyte').
If the current buffer is unibyte, multibyte strings are converted
to unibyte for insertion.

(fn &rest ARGS)"
  (error 'unimplemented-error))
(defun* insert-buffer-substring ()
  #M"Insert before point a substring of the contents of BUFFER.
BUFFER may be a buffer or a buffer name.
Arguments START and END are character positions specifying the substring.
They default to the values of (point-min) and (point-max) in BUFFER.

Point and before-insertion markers move forward to end up after the
inserted text.
Any other markers at the point of insertion remain before the text.

If the current buffer is multibyte and BUFFER is unibyte, or vice
versa, strings are converted from unibyte to multibyte or vice versa
using ‘string-make-multibyte' or ‘string-make-unibyte', which see.

(fn BUFFER &optional START END)"
  (error 'unimplemented-error))
(defun* insert-byte ()
  #M"Insert COUNT (second arg) copies of BYTE (first arg).
Both arguments are required.
BYTE is a number of the range 0..255.

If BYTE is 128..255 and the current buffer is multibyte, the
corresponding eight-bit character is inserted.

Point, and before-insertion markers, are relocated as in the function ‘insert'.
The optional third arg INHERIT, if non-nil, says to inherit text properties
from adjoining text, if those properties are sticky.

(fn BYTE COUNT &optional INHERIT)"
  (error 'unimplemented-error))
(defun* insert-char ()
  #M"Insert COUNT copies of CHARACTER.
Interactively, prompt for CHARACTER using ‘read-char-by-name'.
You can specify CHARACTER in one of these ways:

 - As its Unicode character name, e.g. \"LATIN SMALL LETTER A\".
   Completion is available; if you type a substring of the name
   preceded by an asterisk ‘*', Emacs shows all names which include
   that substring, not necessarily at the beginning of the name.

 - As a hexadecimal code point, e.g. 263A.  Note that code points in
   Emacs are equivalent to Unicode up to 10FFFF (which is the limit of
   the Unicode code space).

 - As a code point with a radix specified with #, e.g. #o21430
   (octal), #x2318 (hex), or #10r8984 (decimal).

If called interactively, COUNT is given by the prefix argument.  If
omitted or nil, it defaults to 1.

Inserting the character(s) relocates point and before-insertion
markers in the same ways as the function ‘insert'.

The optional third argument INHERIT, if non-nil, says to inherit text
properties from adjoining text, if those properties are sticky.  If
called interactively, INHERIT is t.

(fn CHARACTER &optional COUNT INHERIT)"
  (error 'unimplemented-error))
(defun* internal--labeled-narrow-to-region ()
  #M"Restrict editing in this buffer to START-END, and label the restriction with LABEL.

This is an internal function used by ‘with-restriction'.

(fn START END LABEL)"
  (error 'unimplemented-error))
(defun* internal--unlabel-restriction ()
  #M"If the current restriction is labeled with LABEL, remove its label.

This is an internal function used by ‘without-restriction'.

(fn LABEL)"
  (error 'unimplemented-error))
(defun* line-beginning-position ()
  #M"Return the position of the first character in the current line/field.
This function is like ‘pos-bol' (which see), but respects fields.

This function constrains the returned position to the current field
unless that position would be on a different line from the original,
unconstrained result.  If N is nil or 1, and a front-sticky field
starts at point, the scan stops as soon as it starts.  To ignore field
boundaries, bind ‘inhibit-field-text-motion' to t.

This function does not move point.

(fn &optional N)"
  (error 'unimplemented-error))
(defun* line-end-position ()
  #M"Return the position of the last character in the current line/field.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.

This function is like ‘pos-eol' (which see), but respects fields.

This function constrains the returned position to the current field
unless that would be on a different line from the original,
unconstrained result.  If N is nil or 1, and a rear-sticky field ends
at point, the scan stops as soon as it starts.  To ignore field
boundaries bind ‘inhibit-field-text-motion' to t.

This function does not move point.

(fn &optional N)"
  (error 'unimplemented-error))
(defun* mark-marker ()
  #M"Return this buffer's mark, as a marker object.
Watch out!  Moving this marker changes the mark position.
If you set the marker not to point anywhere, the buffer will have no mark.

(fn)"
  (error 'unimplemented-error))
(defun* message-box ()
  #M"Display a message, in a dialog box if possible.
If a dialog box is not available, use the echo area.
The first argument is a format control string, and the rest are data
to be formatted under control of the string.  See ‘format-message' for
details.

If the first argument is nil or the empty string, clear any existing
message; let the minibuffer contents show.

(fn FORMAT-STRING &rest ARGS)"
  (error 'unimplemented-error))
(defun* message-or-box ()
  #M"Display a message in a dialog box or in the echo area.
If this command was invoked with the mouse, use a dialog box if
‘use-dialog-box' is non-nil.
Otherwise, use the echo area.
The first argument is a format control string, and the rest are data
to be formatted under control of the string.  See ‘format-message' for
details.

If the first argument is nil or the empty string, clear any existing
message; let the minibuffer contents show.

(fn FORMAT-STRING &rest ARGS)"
  (error 'unimplemented-error))
(defun* narrow-to-region ()
  #M"Restrict editing in this buffer to the current region.
The rest of the text becomes temporarily invisible and untouchable
but is not deleted; if you save the buffer in a file, the invisible
text is included in the file.  \\[widen] makes all visible again.
See also ‘save-restriction'.

When calling from Lisp, pass two arguments START and END:
positions (integers or markers) bounding the text that should
remain visible.

However, when restrictions have been set by ‘with-restriction' with a
label, ‘narrow-to-region' can be used only within the limits of these
restrictions.  If the START or END arguments are outside these limits,
the corresponding limit set by ‘with-restriction' is used instead of the
argument.  To gain access to other portions of the buffer, use
‘without-restriction' with the same label.

(fn START END)"
  (error 'unimplemented-error))
(defun* ngettext ()
  #M"Return the translation of MSGID (plural MSGID-PLURAL) depending on N.
MSGID is the singular form of the string to be converted;
use it as the key for the search in the translation catalog.
MSGID-PLURAL is the plural form.  Use N to select the proper translation.
If no message catalog is found, MSGID is returned if N is equal to 1,
otherwise MSGID-PLURAL.

(fn MSGID MSGID-PLURAL N)"
  (error 'unimplemented-error))
(defun* point ()
  #M"Return value of point, as an integer.
Beginning of buffer is position (point-min).

(fn)"
  (error 'unimplemented-error))
(defun* point-marker ()
  #M"Return value of point, as a marker object.

(fn)"
  (error 'unimplemented-error))
(defun* point-max ()
  #M"Return the maximum permissible value of point in the current buffer.
This is (1+ (buffer-size)), unless narrowing (a buffer restriction)
is in effect, in which case it is less.

(fn)"
  (error 'unimplemented-error))
(defun* point-max-marker ()
  #M"Return a marker to the maximum permissible value of point in this buffer.
This is (1+ (buffer-size)), unless narrowing (a buffer restriction)
is in effect, in which case it is less.

(fn)"
  (error 'unimplemented-error))
(defun* point-min ()
  #M"Return the minimum permissible value of point in the current buffer.
This is 1, unless narrowing (a buffer restriction) is in effect.

(fn)"
  (error 'unimplemented-error))
(defun* point-min-marker ()
  #M"Return a marker to the minimum permissible value of point in this buffer.
This is the beginning, unless narrowing (a buffer restriction) is in effect.

(fn)"
  (error 'unimplemented-error))
(defun* pos-bol ()
  #M"Return the position of the first character on the current line.
With optional argument N, scan forward N - 1 lines first.
If the scan reaches the end of the buffer, return that position.

This function ignores text display directionality; it returns the
position of the first character in logical order, i.e. the smallest
character position on the logical line.  See ‘vertical-motion' for
movement by screen lines.

This function does not move point.  Also see ‘line-beginning-position'.

(fn &optional N)"
  (error 'unimplemented-error))
(defun* pos-eol ()
  #M"Return the position of the last character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.

This function ignores text display directionality; it returns the
position of the last character in logical order, i.e. the largest
character position on the line.

This function does not move point.  Also see ‘line-end-position'.

(fn &optional N)"
  (error 'unimplemented-error))
(defun* position-bytes ()
  #M"Return the byte position for character position POSITION.
If POSITION is out of range, the value is nil.

(fn POSITION)"
  (error 'unimplemented-error))
(defun* preceding-char ()
  #M"Return the character preceding point, as a number.
At the beginning of the buffer or accessible region, return 0.

(fn)"
  (error 'unimplemented-error))
(defun* (propertize -> pstrings:pstring) ((pstr pstrings:pstring) &rest properties)
  #M"Return a copy of STRING with text properties added.
     First argument is the string to copy.
     Remaining arguments form a sequence of PROPERTY VALUE pairs for text
     properties to add to the result.

     See Info node ‘(elisp) Text Properties' for more information.
     "
  (let ((result (pstrings:copy-pstring pstr)))
    (add-text-properties 0 (pstrings:pstring-len pstr)
                         properties result)
    result))

(test test-propertize
  (is (string= "#(\"abc\" 0 3 (p1 v1 p2 v2))"
               (cl:format nil "~s"
                          (propertize (pstrings:build-pstring "abc") 'p1 'v1 'p2 'v2))))
  (let ((pstr (pstrings:build-pstring "abc")))
    (add-text-properties 0 1 '(p3 v3) pstr)
    (is (string= "#(\"abc\" 0 1 (p1 v1 p2 v2 p3 v3) 1 3 (p1 v1 p2 v2))"
                 (cl:format nil "~s"
                            (propertize pstr 'p1 'v1 'p2 'v2)))))
  )

(defun* region-beginning ()
  #M"Return the integer value of point or mark, whichever is smaller.

(fn)"
  (error 'unimplemented-error))
(defun* region-end ()
  #M"Return the integer value of point or mark, whichever is larger.

(fn)"
  (error 'unimplemented-error))
(defun* replace-buffer-contents ()
  #M"Replace accessible portion of current buffer with that of SOURCE.
SOURCE can be a buffer or a string that names a buffer.
Interactively, prompt for SOURCE.

As far as possible the replacement is non-destructive, i.e. existing
buffer contents, markers, properties, and overlays in the current
buffer stay intact.

Because this function can be very slow if there is a large number of
differences between the two buffers, there are two optional arguments
mitigating this issue.

The MAX-SECS argument, if given, defines a hard limit on the time used
for comparing the buffers.  If it takes longer than MAX-SECS, the
function falls back to a plain ‘delete-region' and
‘insert-buffer-substring'.  (Note that the checks are not performed
too evenly over time, so in some cases it may run a bit longer than
allowed).

The optional argument MAX-COSTS defines the quality of the difference
computation.  If the actual costs exceed this limit, heuristics are
used to provide a faster but suboptimal solution.  The default value
is 1000000.

This function returns t if a non-destructive replacement could be
performed.  Otherwise, i.e., if MAX-SECS was exceeded, it returns
nil.

(fn SOURCE &optional MAX-SECS MAX-COSTS)"
  (error 'unimplemented-error))
(defun* save-current-buffer ()
  #M"Record which buffer is current; execute BODY; make that buffer current.
BODY is executed just like ‘progn'.

(fn &rest BODY)"
  (error 'unimplemented-error))
(defun* save-excursion ()
  #M"Save point, and current buffer; execute BODY; restore those things.
Executes BODY just like ‘progn'.
The values of point and the current buffer are restored
even in case of abnormal exit (throw or error).

If you only want to save the current buffer but not point,
then just use ‘save-current-buffer', or even ‘with-current-buffer'.

Before Emacs 25.1, ‘save-excursion' used to save the mark state.
To save the mark state as well as point and the current buffer, use
‘save-mark-and-excursion'.

(fn &rest BODY)"
  (error 'unimplemented-error))
(defun* save-restriction ()
  #M"Execute BODY, saving and restoring current buffer's restrictions.
The buffer's restrictions make parts of the beginning and end invisible.
(They are set up with ‘narrow-to-region' and eliminated with ‘widen'.)
This special form, ‘save-restriction', saves the current buffer's
restrictions, including those that were set by ‘with-restriction' with a
label argument, when it is entered, and restores them when it is exited.
So any ‘narrow-to-region' within BODY lasts only until the end of the form.
The old restrictions settings are restored even in case of abnormal exit
(throw or error).

The value returned is the value of the last form in BODY.

Note: if you are using both ‘save-excursion' and ‘save-restriction',
use ‘save-excursion' outermost:
    (save-excursion (save-restriction ...))

(fn &rest BODY)"
  (error 'unimplemented-error))
(defun* string-to-char ()
  #M"Return the first character in STRING.

(fn STRING)"
  (error 'unimplemented-error))
(defun* subst-char-in-region ()
  #M"From START to END, replace FROMCHAR with TOCHAR each time it occurs.
If optional arg NOUNDO is non-nil, don't record this change for undo
and don't mark the buffer as really changed.
Both characters must have the same length of multi-byte form.

(fn START END FROMCHAR TOCHAR &optional NOUNDO)"
  (error 'unimplemented-error))
(defun* system-name ()
  #M"Return the host name of the machine you are running on, as a string.

(fn)"
  (error 'unimplemented-error))
(defun* translate-region-internal ()
  #M"Internal use only.
From START to END, translate characters according to TABLE.
TABLE is a string or a char-table; the Nth character in it is the
mapping for the character with code N.
It returns the number of characters changed.

(fn START END TABLE)"
  (error 'unimplemented-error))
(defun* transpose-regions ()
  #M"Transpose region STARTR1 to ENDR1 with STARTR2 to ENDR2.
The regions should not be overlapping, because the size of the buffer is
never changed in a transposition.

Optional fifth arg LEAVE-MARKERS, if non-nil, means don't update
any markers that happen to be located in the regions.

Transposing beyond buffer boundaries is an error.

Interactively, STARTR1 and ENDR1 are point and mark; STARTR2 and ENDR2
are the last two marks pushed to the mark ring; LEAVE-MARKERS is nil.
If a prefix argument N is given, STARTR2 and ENDR2 are the two
successive marks N entries back in the mark ring.  A negative prefix
argument instead counts forward from the oldest mark in the mark
ring.

(fn STARTR1 ENDR1 STARTR2 ENDR2 &optional LEAVE-MARKERS)"
  (error 'unimplemented-error))
(defun* user-full-name ()
  #M"Return the full name of the user logged in, as a string.
If the full name corresponding to Emacs's userid is not known,
return \"unknown\".

If optional argument UID is an integer, return the full name
of the user with that uid, or nil if there is no such user.
If UID is a string, return the full name of the user with that login
name, or nil if there is no such user.

If the full name includes commas, remove everything starting with
the first comma, because the \\='gecos\\=' field of the \\='/etc/passwd\\=' file
is in general a comma-separated list.

(fn &optional UID)"
  (error 'unimplemented-error))
(defun* user-login-name ()
  #M"Return the name under which the user logged in, as a string.
This is based on the effective uid, not the real uid.
Also, if the environment variables LOGNAME or USER are set,
that determines the value of this function.

If optional argument UID is an integer, return the login name
of the user with that uid, or nil if there is no such user.

(fn &optional UID)"
  (error 'unimplemented-error))
(defun* user-real-login-name ()
  #M"Return the name of the user's real uid, as a string.
This ignores the environment variables LOGNAME and USER, so it differs from
‘user-login-name' when running under ‘su'.

(fn)"
  (error 'unimplemented-error))
(defun* user-real-uid ()
  #M"Return the real uid of Emacs, as an integer.

(fn)"
  (error 'unimplemented-error))
(defun* user-uid ()
  #M"Return the effective uid of Emacs, as an integer.

(fn)"
  (error 'unimplemented-error))
(defun* widen ()
  #M"Remove restrictions (narrowing) from current buffer.

This allows the buffer's full text to be seen and edited.

However, when restrictions have been set by ‘with-restriction' with a
label, ‘widen' restores the narrowing limits set by ‘with-restriction'.
To gain access to other portions of the buffer, use
‘without-restriction' with the same label.

(fn)"
  (error 'unimplemented-error))

(defun test-me ()
  (run! 'cl-emacs/editfns))
