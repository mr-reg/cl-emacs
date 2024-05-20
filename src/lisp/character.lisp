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

(uiop:define-package :cl-emacs/character
    (:use
     :common-lisp
     :defstar
     :cl-emacs/log
     :alexandria
     :fiveam
     :cl-emacs/commons))
(in-package :cl-emacs/character)
(log-enable :cl-emacs/character :debug2)
(named-readtables:in-readtable mstrings:mstring-syntax)
(defun* char-resolve-modifiers () "Resolve modifiers in the character CHAR.
The value is a character with modifiers resolved into the character
code.  Unresolved modifiers are kept in the value.

(fn CHAR)"
  (error ’unimplemented-error))
(defun* char-width () "Return width of CHAR when displayed in the current buffer.
The width is measured by how many columns it occupies on the screen.
Tab is taken to occupy ‘tab-width’ columns.

(fn CHAR)"
  (error ’unimplemented-error))
(defun* characterp () "Return non-nil if OBJECT is a character.
In Emacs Lisp, characters are represented by character codes, which
are non-negative integers.  The function ‘max-char’ returns the
maximum character code.

(fn OBJECT)"
  (error ’unimplemented-error))
(defun* get-byte () "Return a byte value of a character at point.
Optional 1st arg POSITION, if non-nil, is a position of a character to get
a byte value.
Optional 2nd arg STRING, if non-nil, is a string of which first
character is a target to get a byte value.  In this case, POSITION, if
non-nil, is an index of a target character in the string.

If the current buffer (or STRING) is multibyte, and the target
character is not ASCII nor 8-bit character, an error is signaled.

(fn &optional POSITION STRING)"
  (error ’unimplemented-error))
(defun* max-char () "Return the maximum character code.
If UNICODE is non-nil, return the maximum character code defined
by the Unicode Standard.

(fn &optional UNICODE)"
  (error ’unimplemented-error))
(defun* multibyte-char-to-unibyte () "Convert the multibyte character CH to a byte.
If the multibyte character does not represent a byte, return -1.

(fn CH)"
  (error ’unimplemented-error))
(defun* string () "Concatenate all the argument characters and make the result a string.

(fn &rest CHARACTERS)"
  (error ’unimplemented-error))
(defun* string-width () "Return width of STRING when displayed in the current buffer.
Width is measured by how many columns it occupies on the screen.
Optional arguments FROM and TO specify the substring of STRING to
consider, and are interpreted as in ‘substring’.

When calculating width of a multibyte character in STRING,
only the base leading-code is considered; the validity of
the following bytes is not checked.  Tabs in STRING are always
taken to occupy ‘tab-width’ columns.  The effect of faces and fonts
used for non-Latin and other unusual characters (such as emoji) is
ignored as well, as are display properties and invisible text.
For these reasons, the results are not generally reliable;
for accurate dimensions of text as it will be displayed,
use ‘window-text-pixel-size’ instead.

(fn STRING &optional FROM TO)"
  (error ’unimplemented-error))
(defun* unibyte-char-to-multibyte () "Convert the byte CH to multibyte character.

(fn CH)"
  (error ’unimplemented-error))
(defun* unibyte-string () "Concatenate all the argument bytes and make the result a unibyte string.

(fn &rest BYTES)"
  (error ’unimplemented-error))
